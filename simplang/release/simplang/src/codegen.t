local CG = {}
package.loaded["simplang.src.codegen"] = CG

local A = require 'simplang.src.ast'
local T = require 'simplang.src.types'

local LangLib = require 'simplang.simplib'
local B       = require 'simplang.src.builtins'
local F       = require 'simplang.src.functions'

-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

function Context.New(env)
  local ctxt = setmetatable({
    env     = env,
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




-------------------------------------------------------------------------------
--[[                              Entry Point                              ]]--
-------------------------------------------------------------------------------

function CG.codegen(func_ast)
  local env     = terralib.newenvironment(nil) -- lua env already captured
  local ctxt    = Context.New(env)
  assert(A.Function:isproto(func_ast), 'expecting Function ast')

  ctxt:enterblock()
  local compiled_func = func_ast:codegen(ctxt)
  ctxt:leaveblock()

  return compiled_func
end


A.NewInertPass {
  passname = 'codegen',
  --verbose = true,
}


-------------------------------------------------------------------------------
--[[                               Functions                               ]]--
-------------------------------------------------------------------------------

function A.ArgDecl:codegen(ctxt)
  error('should be handled by A.function:codegen()')
end

function A.Function:codegen(ctxt)
  local functype = nil

  -- code generate the pieces
  ctxt:enterblock()
  local argsyms = {}
  for i,a in ipairs(self.args) do
    local argtyp  = a.type:terratype()
    local sym     = symbol(argtyp, tostring(a.name))

    ctxt:localenv()[a.name] = sym
    argsyms[i] = sym
  end

  local bodycode = quote end
  for i,stmt in ipairs(self.body) do
    local stmtcode = stmt:codegen(ctxt)
    bodycode = quote [bodycode]
      [stmtcode]
    end
  end

  local retexprs = {}
  for i,r in ipairs(self.rets) do
    local exprcode  = r:codegen(ctxt)
    retexprs[i] = exprcode
  end
  ctxt:leaveblock()

  if #retexprs > 0 then
    bodycode = quote [bodycode]
      return [retexprs]
    end
  end

  local funccode = terra( [argsyms] )
    [bodycode]
  end
  funccode:setname(self.name)
  return funccode
end


-------------------------------------------------------------------------------
--[[                              Statements                               ]]--
-------------------------------------------------------------------------------

function A.DoStmt:codegen(ctxt)
  local bodycode = quote end

  ctxt:enterblock()
  for i,stmt in ipairs(self.body) do
    local stmtcode = stmt:codegen(ctxt)
    bodycode = quote [bodycode]
      [stmtcode]
    end
  end
  ctxt:leaveblock()

  return quote do [bodycode] end end
end

function A.ExprStmt:codegen(ctxt)
  return self.expr:codegen(ctxt)
end

function A.DeclStmt:codegen(ctxt)
  local ttyp      = self.type:terratype()
  local exprcode  = self.expr and self.expr:codegen(ctxt)

  local sym = symbol(ttyp, tostring(self.name))
  ctxt:localenv()[self.name] = sym

  local declcode
  if exprcode then
    declcode = quote var [sym] = exprcode end
  else
    declcode = quote var [sym] end
  end

  return declcode
end

function A.Assignment:codegen(ctxt)
  local lvals = {}
  for i,lv in ipairs(self.lvalues) do
    lvals[i] = lv:codegen(ctxt)
  end

  local rvals = {}
  for i,rv in ipairs(self.rvalues) do
    rvals[i] = rv:codegen(ctxt)
  end

  return quote [lvals] = [rvals] end
end


-------------------------------------------------------------------------------
--[[                         Terminal Expressions                          ]]--
-------------------------------------------------------------------------------

-- use the type given by the literal syntax; otherwise infer type
function A.Number:codegen(ctxt)
  local ttyp = self.type:terratype()
  return terralib.constant(ttyp, self.value)
end

function A.Bool:codegen(ctxt)
  local ttyp = self.type:terratype()
  return terralib.constant(ttyp, self.value)
end

-- shouldn't really have to process this as is.
function A.String:codegen(ctxt)
  error('INTERNAL: should not be compiling strings')
end

-- internal object nodes should be type annotated on construction
function A.LuaObj:codegen(ctxt)
  -- should never happen?
  error('INTERNAL: should not be compiling Lua Objects')
  --return `{}
end

-- names lookup types in the environment record
function A.Name:codegen(ctxt)
  local sym = ctxt:localenv()[self.value]
  assert(terralib.issymbol(sym), "INTERNAL: names should resolve to symbols")
  return `sym
end


-------------------------------------------------------------------------------
--[[                        Structural Expressions                         ]]--
-------------------------------------------------------------------------------

-- namespace use of lookup is resolved by now,
-- so we just have to deal with tensor-indexing
function A.Lookup:codegen(ctxt)
  error('INTERNAL: should not be compiling Lookup nodes')
end

-- typecast or function call
function A.Call:codegen(ctxt)
  if self.base.type:isinternal() then
    local obj   = self.base.type.value
    local args  = {}
    for i,a in ipairs(self.args) do args[i] = a:codegen(ctxt) end

    if B.isbuiltin(obj) then
      return obj.genfunc(self, unpack(args))
    elseif F.isfunction(obj) then
      local tfunc = obj:_INTERNAL_getterrafunc()
      return `tfunc( [args] )
    end
  end
  error('INTERNAL: unrecognized call variant')
end

function A.Let:codegen(ctxt)
  local blockcode = quote end

  ctxt:enterblock()
  for i,stmt in ipairs(self.block) do
    local stmtcode = stmt:codegen(ctxt)
    blockcode = quote [blockcode]
      [stmtcode]
    end
  end
  local exprcode = self.expr:codegen(ctxt)
  ctxt:leaveblock()

  return quote [blockcode] in [exprcode] end
end


-------------------------------------------------------------------------------
--[[                      Operator-level Expressions                       ]]--
-------------------------------------------------------------------------------

function A.UnaryOp:codegen(ctxt)  
  local op    = self.op
  local expr  = self.expr:codegen(ctxt)

  if self.type:isprimitive() then
    if      op == '-'   then  return `-[expr]
    elseif  op == 'not' then  return `not [expr] end
  else
    error('INTERNAL: unexpected type operand: '..tostring(self.type))
  end
  error('INTERNAL: operation unrecognized: '..op)
end

local function prim_bin_exp(op, lhe, rhe)
  if     op == '+'   then return `[lhe] +   [rhe]
  elseif op == '-'   then return `[lhe] -   [rhe]
  elseif op == '/'   then return `[lhe] /   [rhe]
  elseif op == '*'   then return `[lhe] *   [rhe]
  elseif op == 'or'  then return `[lhe] or  [rhe]
  elseif op == 'and' then return `[lhe] and [rhe]
  elseif op == '<'   then return `[lhe] <   [rhe]
  elseif op == '>'   then return `[lhe] >   [rhe]
  elseif op == '<='  then return `[lhe] <=  [rhe]
  elseif op == '>='  then return `[lhe] >=  [rhe]
  elseif op == '=='  then return `[lhe] ==  [rhe]
  elseif op == '~='  then return `[lhe] ~=  [rhe]
  else
    error('INTERNAL: unrecognized binary operator \''..op..'\' in codegen')
  end
end

function A.BinaryOp:codegen(ctxt)
  local op  = self.op
  local lhe = self.lhs:codegen(ctxt)
  local rhe = self.rhs:codegen(ctxt)

  local ltyp    = self.lhs.type
  local rtyp    = self.rhs.type
  local outtyp  = self.type

  -- handle simple scalars
  if ltyp:isprimitive() and rtyp:isprimitive() then
    return prim_bin_exp(op, lhe, rhe)
  end

  -- BAD FALL-THROUGH
  error('INTERNAL: unexpected type operands: '..tostring(ltyp)..
        ' and '..tostring(rtyp))
end


-------------------------------------------------------------------------------
--[[                                 Casts                                 ]]--
-------------------------------------------------------------------------------


function A.Cast:codegen(ctxt)
  local exprcode = self.expr:codegen(ctxt)
  if self.type:isscalar() then
    return `[self.type:terratype()](exprcode)
  elseif self.type:istuple() then
    local pretypes  = self.expr.type:unpacktuple()
    local posttypes = self.type:unpacktuple()
    local tempsyms  = {}
    local casts     = {}
    for i=1,#posttypes do
      tempsyms[i] = symbol(pretypes[i]:terratype())
      casts[i]    = `[posttypes[i]:terratype()]( [tempsyms[i]] )
    end

    return quote
      var [tempsyms] = [exprcode]
    in
      [casts]
    end
  else
    error('INTERNAL: Cast codegen unimplemented for type '..
          tostring(self.type))
  end
end



