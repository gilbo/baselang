local TC = {}
package.loaded["plexlang.src.typechecker"] = TC

local A = require 'plexlang.src.ast'
local T = require 'plexlang.src.types'

local LangLib = require 'plexlang.plexlib'
local B       = require 'plexlang.src.builtins'
local F       = require 'plexlang.src.functions'

-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

function Context.New(env, diag)
  local ctxt = setmetatable({
    env     = env,
    diag    = diag,
    exprenv = {},
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

-- we maintain a separate type environment for tensor indexing
-- that is better suited to aiding mutability patterns used by
-- our lightweight type-inference
function Context:pushexprvar(sym,val)
  local prev = self.exprenv[sym]
  self.exprenv[sym] = { val=val, prev=prev } -- shadow
end
function Context:setexprvar(sym,val)
  local box = assert(self.exprenv[sym])
  box.val = val
end
function Context:getexprvar(sym)
  return assert(self.exprenv[sym]).val
end
function Context:popexprvar(sym)
  local box = assert(self.exprenv[sym])
  self.exprenv[sym] = box.prev
  return box.val
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

local function do_coercion(ast, targettype, ctxt)
  if ast.type == targettype then -- shortcut
    return ast
  else
    -- return a cast regardless then, but
    -- if the cast is not good, report an error and set the error
    -- type on the cast!
    if not ast.type:iscoercableto(targettype) then
      ctxt:error(ast, "Could not coerce expression of type '"..
                      tostring(ast.type) .. "' into type '"..
                      tostring(targettype) .. "'")
      targettype = T.error
    end
    return A.Cast:NewFrom(ast, { expr = ast }):SetVals{ type = targettype }
  end
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
-- check annotation-expr type consistency if both present,
-- possibly inserting a coerced cast
function A.DeclStmt:typecheck(ctxt)
  local typ   = self.annotation
  local expr  = self.expr and self.expr:typecheck(ctxt)

  -- check consistency and determine type
  if expr and typ then
    expr = do_coercion(expr, typ, ctxt)
    typ = expr.type -- could be error
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
      rvalues[1] = do_coercion(rvalues[1], ltyp, ctxt)
    end
  elseif #lvalues > 1 and #rvalues == 1 then -- function multi-return case
    ltyp = T.tuple(ltypes)
    rtyp = rtypes[1]
    if ltyp ~= rtyp then
      rvalues[1] = do_coercion(rvalues[1], ltyp, ctxt)
    end
  elseif #lvalues == #rvalues then
    for i=1,#lvalues do
      if ltypes[i] ~= rtypes[i] then
        rvalues[i] = do_coercion(rvalues[i], ltypes[i], ctxt)
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
  local typ = self.valuetype
  if not typ then
    -- TODO: check number bounds to see if it fits in a int32?
    if tonumber(self.value) % 1 == 0 then
      typ = T.int32
    else
      typ = T.double
    end
  end

  return A.Number:NewFrom(self, { value = self.value }):SetVals{ type = typ }
end

-- two trivial checks
function A.TensorLiteral:typecheck(ctxt)
  return A.TensorLiteral:NewFrom(self, { value = self.value })
                        :SetVals{ type = self.valuetype }
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
  elseif typ:istensorindex() then -- re-alias tensor indices
    typ = ctxt:getexprvar(self.value)
  end

  return A.Name:NewFrom(self, { value = self.value }):SetVals{ type = typ }
end


-------------------------------------------------------------------------------
--[[                        Structural Expressions                         ]]--
-------------------------------------------------------------------------------

-- namespace use of lookup is resolved by now,
-- so we just have to deal with tensor-indexing
function A.Lookup:typecheck(ctxt)
  local base = self.base:typecheck(ctxt)
  local args = {}

  local btyp = base.type
  local typ  = T.error

  if btyp:iserror() then -- fall-through

  -- tensor indexing requires that the # of args is equal to the rank
  -- and that those arguments are integral (actually, should be non-neg...)
  elseif btyp:istensor() then
    -- we have sufficient info to continue typechecking even if the
    -- indexing is done wrong
    typ = btyp:basetype()

    if #btyp.dims ~= #self.args then
      ctxt:error(self,'rank '..tostring(#btyp.dims)..' tensor expected '..
                      tostring(#btyp.dims)..' indices but got '..
                      tostring(#self.args))
      -- check for errors anyway
      for i,a in ipairs(self.args) do args[i] = a:typecheck(ctxt) end
    else
      for i,origa in ipairs(self.args) do
        local a = origa:typecheck(ctxt)
        args[i] = a

        if a.type:iserror() then -- fall-through...
        -- tensor-index case
        elseif a.type:istensorindex() then
          assert(A.Name:isproto(a))

          local inferredtype = T.tensorindex(btyp.dims[i])
          if a.type:isunknowntensorindex() then
            -- infer here
            ctxt:setexprvar(a.value, inferredtype)
            args[i] = A.Name:NewFrom(a, { value = a.value })
                            :SetVals{ type=inferredtype }
          -- if already inferred, then check consistency
          elseif a.type ~= inferredtype then
            ctxt:error(a, "expected index with range "..
                          tostring(inferredtype.range).." but found index "..
                          "with range "..tostring(a.type.range))
          else -- matching? then no problem
          end
        elseif not (a.type:isintegral() and a.type:isscalar()) then
          ctxt:error(a,'expected integral scalar indexing value')
        end
      end
    end

  else
    ctxt:error(self,'expected tensor value to index into')
  end

  return A.Lookup:NewFrom(self, { base=base, args=args })
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
        -- tensor index exception
        if atyp:istensorindex() then
          if not obj:isnumeric() then
            ctxt:error(self, 'can only cast tensor indices to numbers')
          elseif not obj:isscalar() then
            ctxt:error(self, 'can only cast tensor indices to scalars')
          else
            return A.Cast:NewFrom(self,{ expr = args[1] })
                         :SetVals{ type = obj }
          end
        elseif not atyp:isvalue() then
          ctxt:error(self, 'cannot cast from a non-value type: '..
                           tostring(atyp))
        elseif not obj:dimsmatch(atyp) then
          ctxt:error(self,"dimensions don't match: "..
                          tostring(obj)..' vs. '..tostring(atyp))
        else
          return A.Cast:NewFrom(self,{ expr = args[1] })
                       :SetVals{ type = obj }
        end
      end
    elseif B.isbuiltin(obj) or F.isfunction(obj) then
      local ftyp = F.isfunction(obj) and obj:gettype() or obj.type

      -- get and check type compatibility, inserting coercions
      local argtyps = ftyp:argtypes()
      if #argtyps ~= #args then
        ctxt:error(self,'builtin function '..obj.name..'() expects '..
                        tostring(#argtyps)..' arguments but found '..
                        tostring(#args))
      else
        for i,recv_type in ipairs(argtyps) do
          args[i] = do_coercion(args[i], recv_type, ctxt)
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
--[[                          Tensor Expressions                           ]]--
-------------------------------------------------------------------------------

local function tensorindexcheck_helper(ast, ctxt)
  local indexerror = false
  local indexranges = {}
  local expr

  -- handle index ranges and typing the sub-expression
  ctxt:enterblock()
  -- prime all the introduced tensor indices with unknowns
  for _,name in ipairs(ast.names) do
    ctxt:localenv()[name] = T.tensorindex() -- unknown dimension tensor index
    ctxt:pushexprvar(name, T.tensorindex())
  end
  -- recurse into the expression
  expr = ast.expr:typecheck(ctxt)
  -- and then check to make sure every tensor index was used / has a dimension
  for i,name in ipairs(ast.names) do
    local typ = ctxt:popexprvar(name)
    if typ:isunknowntensorindex() then
      ctxt:error(ast,"tensor index '"..tostring(name).."' was not used; "..
                     "so we cannot infer its range")
      indexerror = true
    elseif typ:istensorindex() then
      indexranges[i] = typ.range
    else
      indexerror = true
    end
  end
  ctxt:leaveblock()

  return indexerror, indexranges, expr
end

function A.TensorMap:typecheck(ctxt)
  local indexerror, indexranges, expr = tensorindexcheck_helper(self, ctxt)
  local typ = T.error

  -- then handle constructing the type of the result expression
  if not indexerror and not expr.type:iserror() then
    if not expr.type:isvalue() then
      ctxt:error(self, "tensor maps expect value typed-expressions")
    else
      -- prepend the mapped indices to the existing tensor dimensions
      if expr.type:istensor() then
        for _,d in ipairs(expr.type.dims) do
          table.insert(indexranges, d)
        end
      end
      typ = T.tensor(expr.type:basetype(), unpack(indexranges))
    end
  end

  return A.TensorMap:NewFrom(self, { names=self.names, expr=expr })
                    :SetVals{ type = typ }
end

function A.TensorFold:typecheck(ctxt)
  local indexerror, indexranges, expr = tensorindexcheck_helper(self, ctxt)
  local typ = T.error
  
  -- then handle constructing the type of the result expression
  if not indexerror and not expr.type:iserror() then
    if not expr.type:isvalue() then
      ctxt:error(self, "tensor reductions expect value typed-expressions")
    elseif (self.op == '+' or self.op == '*') and
           not expr.type:isnumeric()
    then
      ctxt:error(self,"tensor reduction using '"..self.op.."' "..
                      "expects to reduce a numeric expression")
    else
      typ = expr.type
    end
  end

  -- pass through range data...
  return A.TensorFold:NewFrom(self, {
    names=self.names, op=self.op, expr=expr
  }):SetVals{ type = typ, ranges=indexranges }
end


-------------------------------------------------------------------------------
--[[                      Operator-level Expressions                       ]]--
-------------------------------------------------------------------------------

-- a list can create a higher rank tensor out of uniformly dimensioned
-- lower rank tensors or a vector out of scalars (degenerate case)
function A.List:typecheck(ctxt)
  local exprs = {}
  for i,e in ipairs(self.exprs) do exprs[i] = e:typecheck(ctxt) end

  local function err(msg,node)
    if msg then ctxt:error(node or self, msg) end
    return A.List:NewFrom(self, { exprs = exprs }):SetVals{ type = T.error }
  end

  -- must have at least one entry
  local nexpr = #self.exprs
  if nexpr == 0 then
    return err('vector/matrix/tensor constructors '..
               'must contain at least one entry')
  end

  -- process the first entry's type
  local maxtyp = exprs[1].type
  if maxtyp:iserror() then return err() end
  if not maxtyp:isvalue() then
    return err('expecting value type, but got: '..tostring(maxtyp), exprs[1])
  end

  -- run through and compute the maximum type under type-join
  for i=2,nexpr do
    local typ = exprs[i].type
    if typ:iserror() then return err() end
    if not typ:isvalue() then
      return err('expecting value type, but got: '..tostring(typ), exprs[i])
    end
    local jointyp = maxtyp:join(typ)
    if jointyp:iserror() then
      return err('could not find common type to coerce this entry ('..
                 tostring(typ)..') and previous entries ('..
                 tostring(maxtyp)..') to', exprs[i])
    end
    maxtyp = jointyp
  end

  -- then go ahead and coerce any entries as needed
  for i=1,nexpr do
    exprs[i] = do_coercion(exprs[i], maxtyp, ctxt)
  end

  -- then compute the resulting type
  local dims = {nexpr}
  if maxtyp:istensor() then
    for i=1,#maxtyp.dims do dims[i+1] = maxtyp.dims[i] end
  end
  local finaltyp = T.tensor(maxtyp:basetype(), unpack(dims))

  return A.List:NewFrom(self, { exprs=exprs }):SetVals{ type = finaltyp }
end

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

  --if typ:iserror() or typ:isscalar() then
    return A.UnaryOp:NewFrom(self, { op=op, expr=expr }):SetVals{ type = typ }
--  else
--    -- REWRITE unary operations applied to tensors into tensor maps
--    assert(typ:istensor())
--    -- GENERATE an ast that has the form
--    --    let temp = expr in :[i,...] OP(temp[i,...])
--    local sym       = A.NewSymbol()
--    local isyms     = {}
--    local inames    = {}
--    for k=1,#typ.dims do
--      isyms[k]  = A.NewSymbol()
--      inames[k] = A.Name:NewFrom(self, { value=isyms[k] })
--                        :SetVals{ type = T.tensorindex(typ.dims[k]) }
--    end
--
--    local mapexpr   = A.TensorMap:NewFrom(self,{
--      names = isyms,
--      expr  = A.UnaryOp:NewFrom(self, { op=op,
--        expr = A.Lookup:NewFrom(self, {
--          base = A.Name:NewFrom(self, {value=sym}):SetVals{ type = etyp },
--          args = inames,
--        }):SetVals{ type = typ:basetype() }
--      }):SetVals{ type = typ:basetype() },
--    }):SetVals{ type = typ }
--
--    local letbind   = A.Let:NewFrom(self, {
--      block={ A.DeclStmt:NewFrom(self, { name = sym,
--          expr = expr,
--        }):SetVals{ type = etyp }
--      },
--      expr = mapexpr,
--    }):SetVals{ type = typ }
--
--    return letbind
--  end
end

local function do_bin_coercion(self, lhs, rhs, ctxt, booloverride)
  local ltyp = lhs.type
  local rtyp = rhs.type
  local jtyp = ltyp:join(rtyp)
  if jtyp == T.error then
    ctxt:error(self, 'could not find common type to coerce operands to; '..
                     'lhs/rhs types: '..tostring(ltyp)..' / '..tostring(rtyp))
    return A.BinaryOp:NewFrom(self, { op=self.op, lhs=lhs, rhs=rhs })
                     :SetVals{ type = T.error }
  end
  if ltyp ~= jtyp then lhs = do_coercion(lhs, jtyp, ctxt) end
  if rtyp ~= jtyp then rhs = do_coercion(rhs, jtyp, ctxt) end
  return A.BinaryOp:NewFrom(self, { op=self.op, lhs=lhs, rhs=rhs })
                   :SetVals{ type = (booloverride and T.bool) or jtyp }
end
-- expects exactly one argument a tensor and one a scalar
local function do_scalartensor_bin_coercion(self, lhs, rhs, ctxt)
  local ltyp  = lhs.type
  local rtyp  = rhs.type
  assert(ltyp:istensor() ~= rtyp:istensor()) -- simplifies logic
  local lbtyp = ltyp:basetype()
  local rbtyp = rtyp:basetype()
  local jbtyp = lbtyp:join(rbtyp)
  if jbtyp == T.error then
    ctxt:error(self,
               'could not find common base type to coerce operands to; '..
               'lhs/rhs types: '..tostring(ltyp)..' / '..tostring(rtyp))
    return A.BinaryOp:NewFrom(self, { op=self.op, lhs=lhs, rhs=rhs })
                     :SetVals{ type = T.error }
  end

  -- joint type with promoted tensor dimensions
  local jtyp  =  ( ltyp:istensor() and T.tensor(jbtyp, unpack(ltyp.dims)) ) or
                 ( rtyp:istensor() and T.tensor(jbtyp, unpack(rtyp.dims)) )

  -- coercion may depend on tensor shape
  if lbtyp ~= jbtyp then
    lhs = do_coercion(lhs, ltyp:istensor() and jtyp or jbtyp, ctxt)
  end
  if rbtyp ~= jbtyp then
    rhs = do_coercion(rhs, rtyp:istensor() and jtyp or jbtyp, ctxt)
  end
  return A.BinaryOp:NewFrom(self, { op=self.op, lhs=lhs, rhs=rhs })
                   :SetVals{ type = jtyp }
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

  -- break everything down by operator, type dimensions, then basetypes

  -- LOGICAL; dims: L = R; basetypes: LOGICAL
  if op == 'and' or op == 'or' then
    if not ltyp:islogical() or not rtyp:islogical() then
      return err("expected boolean operands")
    elseif not ltyp:dimsmatch(rtyp) then
      return err("dimensions don't match")
    else
      return A.BinaryOp:NewFrom(self,{ op=op, lhs=lhs, rhs=rhs })
                       :SetVals{ type = ltyp }
    end
  end

  -- ORDER; dims: scalar; basetypes: NUMERIC (coercion)
  if op == '<=' or op == '>=' or op == '<' or op == '>' then
    if not ltyp:isscalar() or not rtyp:isscalar() then
      return err('expected scalar operands')
    elseif not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    else
      return do_bin_coercion(self, lhs, rhs, ctxt, true) -- bool override
    end
  end

  -- EQUALITY; dims: L = R; basetypes: all (coercion)
  if op == '==' or op == '~=' then
    if not ltyp:dimsmatch(rtyp) then
      return err("dimensions don't match")
    else
      return do_bin_coercion(self, lhs, rhs, ctxt, true) -- bool override
    end
  end

  -- +-; dims: L = R; basetypes: NUMERIC (coercion)
  if op == '+' or op == '-' then
    if not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    elseif not ltyp:dimsmatch(rtyp) then
      return err("dimensions don't match")
    else
      return do_bin_coercion(self, lhs, rhs, ctxt)
    end
  end

  -- *; dims: scalar * scalar and (scalar*tensor and vice-versa);
  -- basetypes: NUMERIC (coercion)
  if op == '*' then
    if not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    elseif ltyp:istensor() == rtyp:istensor() then
      if ltyp:istensor() then
        return err("cannot take product of two tensors; ambiguous meaning")
      else
        return do_bin_coercion(self, lhs, rhs, ctxt)
      end
    else -- one is tensor, the other is scalar
      return do_scalartensor_bin_coercion(self, lhs, rhs, ctxt)
    end
  end

  -- /; dims: _ scalar; basetypes: NUMERIC (coercion)
  if op == '/' then
    if not ltyp:isnumeric() or not rtyp:isnumeric() then
      return err('expected numeric operands')
    elseif rtyp:istensor() then
      return err('cannot divide by a tensor')
    elseif ltyp:istensor() then
      return do_scalartensor_bin_coercion(self, lhs, rhs, ctxt)
    else -- ltyp is scalar
      return do_bin_coercion(self, lhs, rhs, ctxt)
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

function A.TensorMap:alpharename(ctxt)
  local tm = self:clone()

  ctxt:enterblock()
  for i,sym in ipairs(self.names) do
    tm.names[i] = sym:UniqueCopy()
    ctxt:localenv()[sym] = tm.names[i]
  end
  tm.expr = self.expr:alpharename(ctxt)
  ctxt:leaveblock()

  return tm
end
function A.TensorFold:alpharename(ctxt)
  local tf = self:clone()

  ctxt:enterblock()
  for i,sym in ipairs(self.names) do
    tf.names[i] = sym:UniqueCopy()
    ctxt:localenv()[sym] = tf.names[i]
  end
  tf.expr = self.expr:alpharename(ctxt)
  ctxt:leaveblock()

  return tf
end





