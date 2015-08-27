local CG = {}
package.loaded["plexlang.src.codegen"] = CG

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

-- two trivial checks
function A.TensorLiteral:codegen(ctxt)
  return T.luatoterraval(self.value, self.type)
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
  assert(self.base.type:istensor(), "Expected Tensor Lookup")

  local strides = self.base.type.strides
  local base = self.base:codegen(ctxt)
  local indexexpr = `0
  for i,a in ipairs(self.args) do
    local argexpr = a:codegen(ctxt)
    indexexpr = `indexexpr + argexpr * [strides[i]]
  end

  return `[base].d[indexexpr]
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
--[[                          Tensor Expressions                           ]]--
-------------------------------------------------------------------------------

-- common stuff
local function tensor_subgen(ast, ctxt)
  local idxttype = T.tensorindex():terratype()

  ctxt:enterblock()
  local idxvars = {}
  for i,name in ipairs(ast.names) do
    idxvars[i] = symbol(idxttype, tostring(name))
    ctxt:localenv()[name] = idxvars[i]
  end
  local exprcode = ast.expr:codegen(ctxt)
  ctxt:leaveblock()

  return idxvars, exprcode
end

local function tensor_loopgen(idxvars, ranges, body)
  local loopcode = body
  -- wrap the loops from back to front around the body
  for fwd=0,(#idxvars - 1) do
    local i         = #idxvars - fwd
    local prevcode  = loopcode
    loopcode = quote
      for [idxvars[i]]=0,[ranges[i]] do [prevcode] end
    end
  end
  return loopcode
end

function A.TensorMap:codegen(ctxt)
  local exprtyp = self.expr.type

  -- compute the index variables and offset expression
  local idxvars, exprcode = tensor_subgen(self, ctxt)

  -- compute the offset
  local offset  = `0
  for i,ivar in ipairs(idxvars) do
    offset = `[offset] + [self.type.strides[i]] * [idxvars[i]]
  end

  -- build the inner loop body for the map
  local result  = symbol(self.type:terratype())
  local loopbody
  if exprtyp:isscalar() then
    loopbody = quote [result].d[offset] = [exprcode] end
  else
    local innersize = exprtyp.dims[1] * exprtyp.strides[1]
    loopbody = quote
      var temp = [exprcode]
      for i=0,innersize do
        [result].d[ offset + i ] = temp.d[i]
      end
    end
  end

  -- now nest a couple of for loops around the loop body
  local loopcode = tensor_loopgen(idxvars, self.type.dims, loopbody)

  return quote var [result]; [loopcode] in [result] end
end

local function tensor_foldid_gen(op, typ)
  local val
      if op == '+' then val = 0
  elseif op == '*' then val = 1
  else error("INTENRAL: unexpected operator '"..op.."'") end

  if typ:isscalar() then return val
  else
    for d=1,#typ.dims do
      local prev = val
      val = {}
      for k=1,typ.dims[d] do val[k] = prev end
    end
    return T.luatoterraval(val, typ)
  end
end

local function tensor_loopbody_gen(op, result, exprcode, exprtyp)
  if exprtyp:isscalar() then
        if op == '+' then return quote result = result + exprcode end
    elseif op == '*' then return quote result = result * exprcode end
    else error("INTERNAL: unrecognized operator '"..op.."'") end
  else
    assert(exprtyp:istensor())
    local size = exprtyp.dims[1] * exprtyp.strides[1]
    local i    = symbol(int32)
    local e    = symbol(exprtyp:terratype())
    local body
        if op == '+' then body = quote result.d[i] = result.d[i] + e.d[i] end
    elseif op == '*' then body = quote result.d[i] = result.d[i] * e.d[i] end
    else error("INTERNAL: unrecognized operator '"..self.op.."'") end

    return quote var [e] = exprcode; for [i]=0,size do body end end
  end
end

function A.TensorFold:codegen(ctxt)
  local exprtyp   = self.expr.type

  -- compute the index variables and offset expression
  local idxvars, exprcode = tensor_subgen(self, ctxt)

  -- loop body and initial value for reduction
  local result    = symbol(self.type:terratype())
  local initval   = tensor_foldid_gen(self.op, exprtyp)
  local loopbody  = tensor_loopbody_gen(self.op, result, exprcode, exprtyp)

  -- now nest a couple of for loops around the loop body
  local loopcode = tensor_loopgen(idxvars, self.ranges, loopbody)

  return quote var [result] = [initval]; [loopcode] in [result] end
end


function A.List:codegen(ctxt)
  local bttyp = self.type:terrabasetype()
  local ttyp  = self.type:terratype()

  local entries = {}
  for i,e in ipairs(self.exprs) do entries[i] = e:codegen(ctxt) end

  -- case: vector out of scalars
  if #self.type.dims == 1 then
    return `[ttyp]({ d = arrayof(bttyp, [entries] ) })
  else
  -- case: tensor out of lower rank tensors
    local offset      = symbol(int32)
    local result      = symbol(ttyp)
    local stride      = self.type.strides[1]

    -- initialize the inner copy statement
    -- we also need to capture the subexpressions to prevent duplication
    local copybody  = quote end
    local initbody  = quote end
    local entrysyms = {}
    for i=1,#entries do
      entrysyms[i] = symbol(self.exprs[i].type:terratype())
      initbody = quote [initbody]
        var [entrysyms[i]] = [entries[i]]
      end
      local outeroffset = stride * (i-1)
      copybody = quote [copybody]
        result.d[outeroffset + offset] = [entrysyms[i]].d[offset]
      end
    end

    return quote
      [initbody]
      var [result]
      for [offset]=0,[stride] do
        [copybody]
      end
    in
      [result]
    end
  end
end

-------------------------------------------------------------------------------
--[[                      Operator-level Expressions                       ]]--
-------------------------------------------------------------------------------

local function mapgen(typ, func)
  assert(typ:istensor(), 'expected tensor type for mapgen')
  local size = typ.dims[1] * typ.strides[1]
  local iter = symbol(int32)
  local rtyp = typ:terratype()
  return quote
    var result : rtyp
    for [iter]=0,[size] do result.d[iter] = [func(iter)] end
  in
    result
  end
end
local function foldgen(size, init, func)
  local iter = symbol(int32)
  return quote
    var result = init
    for [iter]=0,[size] do result = [func(iter, result)] end
  in
    result
  end
end
local function symbind(typ, exp, func)
  local s = symbol(typ:terratype())
  return quote var [s] = [exp] in [func(s)] end
end
local function symbind2(typ1, typ2, exp1, exp2, func)
  local s1 = symbol(typ1:terratype())
  local s2 = symbol(typ2:terratype())
  return quote
    var [s1] = [exp1]
    var [s2] = [exp2]
  in [func(s1,s2)] end
end
function A.UnaryOp:codegen(ctxt)  
  local op    = self.op
  local expr  = self.expr:codegen(ctxt)

  if self.type:istensor() then
    return symbind(self.type, expr, function(arg)
      return mapgen(self.type, function(i)
        if op == '-' then
          return `-[arg].d[i]
        elseif op == 'not' then
          return `not [arg].d[i]
        else
          error('INTERNAL: operation unrecognized: '..op)
        end
      end)
    end)
  elseif self.type:isscalar() then
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
  if ltyp:isscalar() and rtyp:isscalar() then
    return prim_bin_exp(op, lhe, rhe)
  end

  -- tensor?

  -- LOGICAL ops
  if op == 'and' or op == 'or' or op == '+' or op == '-' then
    return symbind2(ltyp, rtyp, lhe, rhe, function(lexp, rexp)
      return mapgen(outtyp, function(i)
        return prim_bin_exp(op, `[lexp].d[i], `[rexp].d[i])
      end)
    end)
  end

  -- EQUALITY ops
  if op == '==' then
    -- can't use outtyp here cause it's just 'bool'
    local size    = ltyp.dims[1] * ltyp.strides[1]
    return symbind2(ltyp, rtyp, lhe, rhe, function(lexp, rexp)
      return foldgen(size, `true, function(i, acc)
        local testexpr = prim_bin_exp(op, `[lexp].d[i], `[rexp].d[i])
        return `[acc] and [testexpr]
      end)
    end)
  end
  if op == '~=' then
    -- can't use outtyp here cause it's just 'bool'
    local size    = ltyp.dims[1] * ltyp.strides[1]
    return symbind2(ltyp, rtyp, lhe, rhe, function(lexp, rexp)
      return foldgen(size, `false, function(i, acc)
        local testexpr = prim_bin_exp(op, `[lexp].d[i], `[rexp].d[i])
        return `[acc] or [testexpr]
      end)
    end)
  end


  -- * / ops
  if op == '*' or op == '/' then
    return symbind2(ltyp, rtyp, lhe, rhe, function(lexp, rexp)
      return mapgen(outtyp, function(i)
        if ltyp:istensor() and rtyp:isscalar() then
          return prim_bin_exp(op, ( `[lexp].d[i] ), rexp)
        elseif ltyp:isscalar() and rtyp:istensor() then
          return prim_bin_exp(op, lexp, `[rexp].d[i])
        else
          error('INTERNAL: for * / only suppporting '..
                'tensor/scalar, scalar/tensor codegen')
        end
      end)
    end)
  end

  -- BAD FALL-THROUGH
  error('INTERNAL: unrecognized binary operator \''..op..'\' in codegen')
end


-------------------------------------------------------------------------------
--[[                                 Casts                                 ]]--
-------------------------------------------------------------------------------


function A.Cast:codegen(ctxt)
  local exprcode = self.expr:codegen(ctxt)
  if self.type:isscalar() then
    return `[self.type:terratype()](exprcode)
  elseif self.type:istensor() then
    local bttype = self.type:terrabasetype()
    return symbind(self.expr.type, exprcode, function(arg)
      return mapgen(self.type, function(i)
        return `[bttype]([arg].d[i])
      end)
    end)
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



