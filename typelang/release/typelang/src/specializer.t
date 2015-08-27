local S = {}
package.loaded["typelang.src.specializer"] = S

-------------------------------------------------------------------------------
-- Specialize is a compiler pass that substitutes in as many up-values as
-- possible, such that a proper closure can be formed
-- It should always be run at declaration time, not deferred till
-- execution/compilation.
--    Specialization also converts parsed names into symbols.
-------------------------------------------------------------------------------

local A = require 'typelang.src.ast'
local T = require 'typelang.src.types'

local LangLib = require 'typelang.typelib'

-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

function Context.New(env, diag)
  local ctxt = setmetatable({
    env     = env,
    diag    = diag,
  }, Context)
  return ctxt
end
function Context:localenv()
  return self.env:localenv()
end
function Context:luaenv()
  return self.env:luaenv()
end
function Context:enterblock()
  self.env:enterblock()
end
function Context:leaveblock()
  self.env:leaveblock()
end
function Context:error(ast, ...)
  self.diag:reporterror(ast, ...)
end


-------------------------------------------------------------------------------
--[[                             Entry Point                               ]]--
-------------------------------------------------------------------------------

function S.specialize(input_ast, luaenv)
  local env     = terralib.newenvironment(luaenv)
  local diag    = terralib.newdiagnostics()
  local ctxt    = Context.New(env, diag)

  diag:begin()
  env:enterblock()
  local output_ast = input_ast:specialize(ctxt)
  env:leaveblock()
  diag:finishandabortiferrors("Errors during specialization", 1)

  return output_ast
end


local function luaeval(expr, anchor, ctxt, default)
  local status, val = pcall(function()
    return expr(ctxt:luaenv())
  end)
  if not status then
    ctxt:error(anchor, "Error evaluating Lua expression")
    return default
  else
    return val
  end
end

local function eval_type_annotation(annotation, anchor, ctxt)
  local typ = luaeval(annotation, anchor, ctxt, T.error)
  -- handle promoting non-types into types?
    -- no such handling right now

  if not T.istype(typ) then
    ctxt:error(anchor, "Expected type but found "..type(typ))
    typ = T.error
  end
  return typ
end

local function introsym(namestr, ctxt)
  local namesym = A.NewSymbol(namestr)
  ctxt:localenv()[namestr] = namesym
  return namesym
end

-------------------------------------------------------------------------------
--[[                         AST Structural Rules                          ]]--
-------------------------------------------------------------------------------

A.NewCopyPass {
  passname = 'specialize',
  --verbose = true,
}

function A.DoStmt:specialize(ctxt)
  local body = {}

  ctxt:enterblock()
  for i,stmt in ipairs(self.body) do
    body[i] = stmt:specialize(ctxt)
  end
  ctxt:leaveblock()

  return A.DoStmt:NewFrom(self,{
    body = body,
  })
end

function A.Let:specialize(ctxt)
  local block = {}

  ctxt:enterblock()
  for i,stmt in ipairs(self.block) do
    block[i] = stmt:specialize(ctxt)
  end
  local expr = self.expr:specialize(ctxt)
  ctxt:leaveblock()

  return A.Let:NewFrom(self,{
    block = block, expr = expr
  })
end



-------------------------------------------------------------------------------
--[[                  Conversion from Lua Values into AST                  ]]--
-------------------------------------------------------------------------------

local function NewLuaObj(lobj, anchor)
  local ast   = A.LuaObj:NewFrom(anchor,{})
  ast.type    = T.internal(lobj)
  return ast
end

local function constant_to_ast(cval, typ, anchor)
  if typ:isprimitive() then
    if typ:isnumeric() then
      return A.Number:NewFrom(anchor, { value=cval, valuetype=typ })
    elseif typ:islogical() then
      return A.Bool:NewFrom(anchor, { value=cval })
    else error('INTERNAL: unrecognized primitive type') end
  else
    error('INTERNAL: bad constant type')
  end
end

local function lua_primitive_to_ast(luav, anchor)
  if type(luav) == 'number' then
    return A.Number:NewFrom(anchor, { value=luav })
  elseif type(luav) == 'boolean' then
    return A.Bool:NewFrom(anchor, { value=luav })
  else
    error('INTERNAL: SHOULD only see primitive types number/boolean here')
  end
end

local function luaval_to_ast(luav, anchor)

  -- convert constants
  if LangLib.isconstant(luav) then
    return constant_to_ast(luav._value, luav._type, anchor)

  -- default table case
  elseif type(luav) == 'table' then
    return NewLuaObj(luav, anchor)

  -- primitive value
  elseif type(luav) == 'number' or type(luav) == 'boolean' then
    return lua_primitive_to_ast(luav, anchor)

  -- failure case
  else
    return nil
  end
end

-------------------------------------------------------------------------------
--[[                          Name Related Rules                           ]]--
-------------------------------------------------------------------------------

function A.DeclStmt:specialize(ctxt)
  -- evalute the annotation
  local annotation = self.annotation and
                     eval_type_annotation(self.annotation, self, ctxt)
  -- optional handling of the expression
  local expr       = self.expr and self.expr:specialize(ctxt)
  -- Register New Symbol
  local namesym = introsym(self.name, ctxt)

  return A.DeclStmt:NewFrom(self, {
    name = namesym, annotation = annotation, expr = expr
  })
end

function A.Name:specialize(ctxt)
  -- First, try to look up the name in the local scope and assign
  -- the relevant symbol
  local sym = ctxt:localenv()[self.value]
  if sym then
    return A.Name:NewFrom(self, { value = sym })
  end

  -- Second, try to look up the name in the enclosing Lua scope,
  -- converting it into an appropriate AST node
  local luaval = ctxt:luaenv()[self.value]
  if luaval ~= nil then
    -- convert lua value into an ast node
    local ast = luaval_to_ast(luaval, self)
    if ast then
      return ast
    else
      ctxt:error(self, "Could not successfully convert the Lua value "..
        "referred to by '"..self.value.."'")
      return self
    end
  end

  -- Otherwise we failed to find the name
  ctxt:error(self, "variable '"..self.value.."' is undefined")
  return self
end

function A.Lookup:specialize(ctxt)
  local base = self.base:specialize(ctxt)
  local arg  = self.arg:specialize(ctxt)

  -- if the base access is a lua object, then we might
  -- want to specialize the lookup...
  if A.LuaObj:isproto(base) then
    local obj = base.type.value -- grab object from internal(obj) type

    -- base is a table with one string argument
    if type(obj) == 'table' and A.String:isproto(arg) then
      local str = arg.value

      -- try the lookup
      local luaval = obj[str]
      if luaval == nil then
        ctxt:error(self, "Could not find entry '"..str.."' in lua table")
        return self
      end

      -- and to convert the value looked up
      local ast = luaval_to_ast(luaval, self)
      if ast then return ast
      else
        ctxt:error(self, "The lua table entry '"..str.."' could not be "..
                         "successfully converted")
        return self
      end
    end
  end

  -- otherwise...
  return A.Lookup:NewFrom(self, { base=base, arg=arg })
end

-------------------------------------------------------------------------------
--[[                          Function Entrypoint                          ]]--
-------------------------------------------------------------------------------

function A.ArgDecl:specialize(ctxt)
  local annotation = self.annotation and
                     eval_type_annotation(self.annotation, self, ctxt)
  local namesym = introsym(self.name, ctxt)

  return A.ArgDecl:NewFrom(self, { name = namesym, annotation = annotation })
end

function A.Function:specialize(ctxt)
  ctxt:enterblock()
  local args = {}
  for i,a in ipairs(self.args) do
    args[i] = a:specialize(ctxt)
  end

  local body = {}
  for i,stmt in ipairs(self.body) do
    body[i] = stmt:specialize(ctxt)
  end

  local rets = {}
  for i,expr in ipairs(self.rets) do
    rets[i] = expr:specialize(ctxt)
  end
  ctxt:leaveblock()

  return A.Function:NewFrom(self,{
    name = self.name,
    args = args,
    body = body,
    rets = rets,
  })
end




