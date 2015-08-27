local TC = {}
package.loaded["simplang.src.typechecker"] = TC

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
--[[                              Entry Point                              ]]--
-------------------------------------------------------------------------------

function TC.typecheck(input_ast)
  local env     = terralib.newenvironment(nil) -- lua env already captured
  local diag    = terralib.newdiagnostics()
  local ctxt    = Context.New(env, diag)

  diag:begin()
  env:enterblock()
  local output_ast = input_ast:typecheck(ctxt)
  env:leaveblock()
  diag:finishandabortiferrors("Errors during typechecking", 1)

  return output_ast
end

A.NewCopyPass {
  passname = 'typecheck',
  defaultvals = { type = T.error },
  --verbose = true,
}


-------------------------------------------------------------------------------
--[[                               Functions                               ]]--
-------------------------------------------------------------------------------

function A.ArgDecl:typecheck(ctxt)
  local typ   = self.annotation
  if not T.istype(typ) then
    ctxt:error(self, 'argument must be annotated with a type')
    typ = T.error
  elseif T.istype(typ) and not typ:isvalue() then
    ctxt:error(self, 'arguments may only have value types')
    typ = T.error
  end

  -- record the type
  ctxt:localenv()[self.name] = typ

  return A.ArgDecl:NewFrom(self, { name = self.name }):SetVals{ type = typ }
end
function A.Function:typecheck(ctxt)
  local functype = nil
  ctxt:enterblock()
  local args = {}
  local argtyps = {}
  for i,a in ipairs(self.args) do
    args[i]     = a:typecheck(ctxt)
    argtyps[i]  = args[i].type
    if argtyps[i] == T.error then functype = T.error end
  end

  local body = {}
  for i,stmt in ipairs(self.body) do body[i] = stmt:typecheck(ctxt) end

  local rets = {}
  local rettyps = {}
  for i,r in ipairs(self.rets) do
    rets[i]     = r:typecheck(ctxt)
    rettyps[i]  = rets[i].type
    if rettyps[i] == T.error then functype = T.error end
  end
  ctxt:leaveblock()

  -- if we didn't see any errors then
  if not functype then functype = T.arrow(argtyps,rettyps) end

  return A.Function:NewFrom(self,{
    name = self.name,
    args = args,
    body = body,
    rets = rets,
  }):SetVals { type = functype } -- 
end


-------------------------------------------------------------------------------
--[[                              Statements                               ]]--
-------------------------------------------------------------------------------

-- Boiler-plate
function A.DoStmt:typecheck(ctxt)
  local body = {}

  ctxt:enterblock()
  for i,stmt in ipairs(self.body) do
    body[i] = stmt:typecheck(ctxt)
  end
  ctxt:leaveblock()

  -- no need for type on a statement
  return A.DoStmt:NewFrom(self,{ body = body })
end

-- Boiler-plate
function A.ExprStmt:typecheck(ctxt)
  return A.ExprStmt:NewFrom(self, { expr = self.expr:typecheck(ctxt) })
end

-- Summary: determine new variable type either from the
-- explicit annotation or from the initializing expression.
-- check annotation-expr type consistency if both present
function A.DeclStmt:typecheck(ctxt)
  local typ   = self.annotation
  local expr  = self.expr and self.expr:typecheck(ctxt)

  -- check consistency and determine type
  if expr and typ then
    if typ ~= expr.type then
      ctxt:error(self, "Cannot assign expression of type '"..
                       tostring(expr.type) .. "' to a variable of type '"..
                       tostring(typ) .. "'")
    end
  elseif expr then
    typ = expr.type
  elseif not typ then
    ctxt:error(self, "cannot declare variables without providing either "..
                     "a type or initial value")
    typ = T.error
  end

  -- error on non-value types
  if not typ:iserror() and not typ:isvalue() then
    ctxt:error(self, "cannot introduce variables with non-value type "..
                     tostring(typ))
    typ = T.error
  end

  ctxt:localenv()[self.name] = typ

  return A.DeclStmt:NewFrom(self, {
    name = self.name,
    expr = expr,
  }):SetVals { type = typ }
end

-- We allow for multiple assignment syntax
-- This comes in two flavors:
--    1. the number of lvalues and rvalues match, and match in type
--    2. there is one rvalue of tuple type matching the lvalue list
function A.Assignment:typecheck(ctxt)
  local lvalues = {}
  local ltypes  = {}
  for i,lval in ipairs(self.lvalues) do
    lvalues[i] = lval:typecheck(ctxt)
    ltypes[i]  = lvalues[i].type
    if ltypes[i] == T.error then return self end
    if not lvalues[i]:islvalue() then
      ctxt:error(lvalues[i],'can only assign to lvalues')
      return self
    end
  end

  local rvalues = {}
  local rtypes  = {}
  for i,rval in ipairs(self.rvalues) do
    rvalues[i] = rval:typecheck(ctxt)
    rtypes[i]  = rvalues[i].type
    if rtypes[i] == T.error then return self end
  end

  -- TODO-test Lvalueness

  -- handle 4 cases for different numbers of lvalues and rvalues
  local ltyp, rtyp
  if #lvalues == 1 and #rvalues == 1 then
    ltyp = ltypes[1]
    rtyp = rtypes[1]
    if ltyp ~= rtyp then
      ctxt:error(self,'Cannot assign value of type '..tostring(rtyp)..
                 'to a variable of type '..tostring(ltyp))
    end
  elseif #lvalues > 1 and #rvalues == 1 then -- function multi-return case
    ltyp = T.tuple(ltypes)
    rtyp = rtypes[1]
    if ltyp ~= rtyp then
      ctxt:error(self,'Cannot assign value of type '..tostring(rtyp)..
                 'to variables of type '..tostring(ltyp))
    end
  elseif #lvalues == #rvalues then
    for i=1,#lvalues do
      if ltypes[i] ~= rtypes[i] then
        ctxt:error(lvalues[i],'Cannot assign value of type '..tostring(rtyp)..
                   'to a variable of type '..tostring(ltyp))
      end
    end
    ltyp = T.tuple(ltypes)
    rtyp = T.tuple(rtypes)
  else
    ctxt:error(self, 'assignment has a mismatched number of terms on the '..
                     'left ('..tostring(#ltypes)..') '..
                     'and right ('..tostring(#rtypes)..')')
    return self
  end

  return A.Assignment:NewFrom(self, {
    lvalues = lvalues,
    rvalues = rvalues,
  }):SetVals{ type = ltyp }
end


-------------------------------------------------------------------------------
--[[                         Terminal Expressions                          ]]--
-------------------------------------------------------------------------------

-- use the type given by the literal syntax; otherwise infer type
function A.Number:typecheck(ctxt)
  return A.Number:NewFrom(self, { value = self.value }):SetVals{ type = T.num }
end

function A.Bool:typecheck(ctxt)
  return A.Bool:NewFrom(self, { value = self.value }):SetVals{ type = T.bool }
end

-- shouldn't really have to process this as is.
function A.String:typecheck(ctxt)
  return A.String:NewFrom(self, { value = self.value })
                 :SetVals{ type = T.internal(self.value) }
end

-- internal object nodes should be type annotated on construction
function A.LuaObj:typecheck(ctxt)
  return A.LuaObj:NewFrom(self,{}):SetVals{ type = self.type }
end

-- names lookup types in the environment record
function A.Name:typecheck(ctxt)
  local typ = ctxt:localenv()[self.value]
  if not typ then
    ctxt:error(self, "variable '"..tostring(self.value).."' is not defined")
    typ = T.error
  end

  return A.Name:NewFrom(self, { value = self.value }):SetVals{ type = typ }
end


-------------------------------------------------------------------------------
--[[                        Structural Expressions                         ]]--
-------------------------------------------------------------------------------

-- namespace use of lookup is resolved by now,
-- so we shouldn't actually have any lookups left...
function A.Lookup:typecheck(ctxt)
  local base = self.base:typecheck(ctxt)
  local arg  = self.arg:typecheck(ctxt)

  local btyp = base.type
  local typ  = T.error

  if btyp:iserror() then -- fall-through
  else
    ctxt:error(self,'can only index into lua tables.')
  end

  return A.Lookup:NewFrom(self, { base=base, arg=arg })
                 :SetVals{ type = typ }
end

-- typecast or function call
function A.Call:typecheck(ctxt)
  local base = self.base:typecheck(ctxt)
  local args = {}
  for i,a in ipairs(self.args) do args[i] = a:typecheck(ctxt) end

  local btyp = base.type
  local typ  = T.error

  if btyp:iserror() then -- fall-through

  elseif btyp:isinternal() then
    local obj = btyp.value

    -- Type-Cast
    if T.istype(obj) then
      -- Run a bunch of checks that this cast is valid
      if #args ~= 1 then
        ctxt:error(self,'typecasts expect a single argument')
      elseif not obj:isvalue() then
        ctxt:error(self,'cannot cast to non-value type: '..tostring(obj))
      else
        local atyp = args[1].type
        if not atyp:isvalue() then
          ctxt:error(self, 'cannot cast from a non-value type: '..
                           tostring(atyp))
        else
          return A.Cast:NewFrom(self,{ expr = args[1] })
                       :SetVals{ type = obj }
        end
      end
    elseif B.isbuiltin(obj) or F.isfunction(obj) then
      local ftyp = F.isfunction(obj) and obj:gettype() or obj.type

      -- get and check type compatibility
      local argtyps = ftyp:argtypes()
      if #argtyps ~= #args then
        ctxt:error(self,'builtin function '..obj.name..'() expects '..
                        tostring(#argtyps)..' arguments but found '..
                        tostring(#args))
      else
        for i,recv_type in ipairs(argtyps) do
          if args[i].type ~= recv_type then
            ctxt:error(args[i],'argument has type '..tostring(args[i].type)..
                               ' but we expected type '..tostring(recv_type))
          end
        end
      end

      -- a bit inefficient to rebuild tuple; oh well for now
      local retlist = ftyp:rettypes()
      if #retlist == 1 then typ = retlist[1]
                       else typ = T.tuple(retlist) end
    else
      ctxt:error(self,'expected function to call')
    end

  else
    ctxt:error(self,'expected function to call')
  end

  return A.Call:NewFrom(self, { base=base, args=args }):SetVals{ type = typ }
end

function A.Let:typecheck(ctxt)
  local block = {}

  ctxt:enterblock()
  for i,stmt in ipairs(self.block) do
    block[i] = stmt:typecheck(ctxt)
  end
  local expr = self.expr:typecheck(ctxt)
  ctxt:leaveblock()

  return A.Let:NewFrom(self,{ block = block, expr = expr })
              :SetVals{ type = expr.type }
end


-------------------------------------------------------------------------------
--[[                      Operator-level Expressions                       ]]--
-------------------------------------------------------------------------------

function A.UnaryOp:typecheck(ctxt)
  local op    = self.op
  local expr  = self.expr:typecheck(ctxt)
  local etyp  = expr.type
  local typ   = T.error

  if etyp:iserror() then -- fall-through...
  elseif op == 'not' then
    if etyp:islogical() then typ = etyp
    else ctxt:error(self, "unary 'not' expects a bool operand") end
  elseif op == '-' then
    if etyp:isnumeric() then typ = etyp
    else ctxt:error(self, "'-' expects a numeric operand") end
  else
    ctxt:error(self, 'INTERNAL: unrecognized unary operator \''..op..'\'')
  end

  return A.UnaryOp:NewFrom(self, { op=op, expr=expr }):SetVals{ type = typ }
end

function A.BinaryOp:typecheck(ctxt)
  local astnode = self

  local op    = self.op
  local lhs   = self.lhs:typecheck(ctxt)
  local rhs   = self.rhs:typecheck(ctxt)
  local ltyp  = lhs.type
  local rtyp  = rhs.type

  local function err(prefix)
    ctxt:error(astnode,prefix.."; lhs/rhs types: "..
               tostring(ltyp)..' / '..tostring(rtyp))
    return A.BinaryOp:NewFrom(self, { op=op, lhs=lhs, rhs=rhs })
                     :SetVals{ type = T.error }
  end

  if ltyp:iserror() or rtyp:iserror() then
    return A.BinaryOp:NewFrom(self, { op=op, lhs=lhs, rhs=rhs })
                     :SetVals{ type = T.error }
  end

  if not ltyp:isvalue() or not rtyp:isvalue() then
    return err("cannot combine non-values using '"..op.."'")
  end

  -- break everything down by operator, then type

  -- LOGICAL; types: LOGICAL
  if op == 'and' or op == 'or' then
    if not ltyp:islogical() or not rtyp:islogical() then
      return err("expected boolean operands")
    else
      return A.BinaryOp:NewFrom(self,{ op=op, lhs=lhs, rhs=rhs })
                       :SetVals{ type = ltyp }
    end
  end

  -- ORDER; types: NUMERIC
  if op == '<=' or op == '>=' or op == '<' or op == '>' then
    if not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    else
      return A.BinaryOp:NewFrom(self,{ op=op, lhs=lhs, rhs=rhs })
                       :SetVals{ type = T.bool }
    end
  end

  -- EQUALITY; types: all
  if op == '==' or op == '~=' then
    if ltyp ~= rtyp then
      return err("cannot compare values of different type")
    else
      return A.BinaryOp:NewFrom(self,{ op=op, lhs=lhs, rhs=rhs })
                       :SetVals{ type = T.bool }
    end
  end

  -- +-; types: NUMERIC
  if op == '+' or op == '-' then
    if not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    else
      return A.BinaryOp:NewFrom(self,{ op=op, lhs=lhs, rhs=rhs })
                       :SetVals{ type = ltyp }
    end
  end

  -- *; types: NUMERIC
  if op == '*' then
    if not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    else
      return A.BinaryOp:NewFrom(self,{ op=op, lhs=lhs, rhs=rhs })
                       :SetVals{ type = ltyp }
    end
  end

  -- /; types: NUMERIC
  if op == '/' then
    if not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    else
      return A.BinaryOp:NewFrom(self,{ op=op, lhs=lhs, rhs=rhs })
                       :SetVals{ type = ltyp }
    end
  end

  -- BAD FALL-THROUGH
  ctxt:error(self, 'INTERNAL: unrecognized binary operator \''..op..'\'')
  return A.BinaryOp:NewFrom(self, { op=op, lhs=lhs, rhs=rhs })
                   :SetVals{ type = T.error }
end





-------------------------------------------------------------------------------
--[[                      Typechecker Helper Methods                       ]]--
-------------------------------------------------------------------------------

-- abusing this pass generator to generate a pass that computes
-- a boolean value, rather than no value; defaults will return nil
-- which is false-y
A.NewInertPass {
  passname = 'islvalue',
  --verbose = true,
}
function A.Name:islvalue(ctxt)
  -- can this variable be assigned to?
  -- current rule: only if it's value-typed
  if self.type:isvalue() then return true end
  -- otherwise no
end
function A.Lookup:islvalue(ctxt)
  -- lookups can be done in lvalues, provided the result is value typed
  -- and that the base itself is an lvalue
  if self.type:isvalue() and self.base:islvalue() then return true end
  -- otherwise no
end
-- For simplicity, we're prohibiting Let expressions right now...

-- This pass makes sure that we have a copy of the AST where
-- all of the symbols have been replaced by unique copies (consistently)
-- such that the resulting AST won't have symbol collisions with any
-- existing AST that it's spliced into.
A.NewCopyPass {
  passname = 'alpharename',
  copymembers = { 'type' },
  --verbose = true,
}
function A.ArgDecl:alpharename(ctxt)
  local argdecl = self:clone()

  argdecl.name  = self.name:UniqueCopy()
  ctxt:localenv()[argdecl.name] = argdecl.name
  return argdecl
end
function A.Function:alpharename(ctxt)
  local func = self:clone()

  ctxt:enterblock()
  for i,a in ipairs(self.args) do
    func.args[i] = a:alpharename(ctxt)
  end
  for i,stmt in ipairs(self.body) do
    func.body[i] = stmt:alpharename(ctxt)
  end
  for i,expr in ipairs(self.rets) do
    func.rets[i] = expr:alpharename(ctxt)
  end
  ctxt:leaveblock()

  return func
end

function A.DoStmt:alpharename(ctxt)
  local dostmt = self:clone()

  ctxt:enterblock()
  for i,stmt in ipairs(self.body) do
    dostmt.body[i] = stmt:alpharename(ctxt)
  end
  ctxt:leaveblock()

  return dostmt
end
function A.DeclStmt:alpharename(ctxt)
  local decl = self:clone()

  decl.name  = self.name:UniqueCopy()
  ctxt:localenv()[self.name] = decl.name

  decl.expr  = self.expr:alpharename()

  return decl
end
function A.Let:alpharename(ctxt)
  local let = self:clone()

  ctxt:enterblock()
    for i,stmt in ipairs(self.block) do
      let.block[i] = stmt:alpharename(ctxt)
    end
    let.expr = self.expr:alpharename(ctxt)
  ctxt:leaveblock()

  return let
end




