local test  = require 'tests.test'

import 'simplang.simplang'

local Lib   = require 'simplang.simplib'
local A     = require 'simplang.src.ast'
local T     = require 'simplang.src.types'


------------------------------------------------------------------------------

local function get_ast_copy(fn)
  return fn._decl_ast:clone()
end

local function insert_cast(ast, typ)
  return A.Cast:NewFrom(ast,{expr = ast}):SetVals{ type = typ }
end

------------------------------------------------------------------------------

simpl retzero(a : T.num, b : T.num)
  return 0
end
local retzeroT = get_ast_copy(retzero)
-- zero out annotations
retzeroT.args[1].annotation = nil
retzeroT.args[2].annotation = nil
retzeroT.rets[1].valuetype = nil
-- manually decorate types
retzeroT.args[1].type = T.num
retzeroT.args[2].type = T.num
retzeroT.rets[1].type = T.num
retzeroT.type = T.arrow({T.num,T.num},{T.num})
-- check that type decorations match
test.asteq(retzero._typed_ast, retzeroT, {'type'})


simpl retpair()
  return 0, 1
end
local retpairT = get_ast_copy(retpair)
-- zero out annotations
retpairT.rets[1].valuetype = nil
retpairT.rets[2].valuetype = nil
-- manually decorate types
retpairT.rets[1].type = T.num
retpairT.rets[2].type = T.num
retpairT.type = T.arrow({},{T.num,T.num})
-- check that type decorations match
test.asteq(retpair._typed_ast, retpairT, {'type'})


simpl doshadow(a : T.num, b : T.num)
  var x = 0.0
  do
    var a = x + b
  end
  x = a + b
  return x
end
local doshadowT = get_ast_copy(doshadow)
-- zero out annotations
doshadowT.args[1].annotation = nil
doshadowT.args[2].annotation = nil
doshadowT.body[1].expr.valuetype = nil
-- manually decorate types
doshadowT.args[1].type = T.num
doshadowT.args[2].type = T.num
doshadowT.body[1].type = T.num
doshadowT.body[1].expr.type = T.num
doshadowT.body[2].body[1].type = T.num
doshadowT.body[2].body[1].expr.type = T.num
doshadowT.body[2].body[1].expr.lhs.type = T.num
doshadowT.body[2].body[1].expr.rhs.type = T.num
doshadowT.body[3].type = T.num
doshadowT.body[3].lvalues[1].type = T.num
doshadowT.body[3].rvalues[1].type = T.num
doshadowT.body[3].rvalues[1].lhs.type = T.num
doshadowT.body[3].rvalues[1].rhs.type = T.num
doshadowT.rets[1].type = T.num
doshadowT.type = T.arrow({T.num,T.num},{T.num})
test.asteq(doshadow._typed_ast, doshadowT, {'type'})



simpl annotate( x : T.num )
  var y : T.num
  y = x
  var z : T.num = y
  return z
end
local annotateT = get_ast_copy(annotate)
-- zero out annotations
annotateT.args[1].annotation = nil
annotateT.body[1].annotation = nil
annotateT.body[3].annotation = nil
-- manually decorate types
annotateT.args[1].type = T.num
annotateT.body[1].type = T.num
annotateT.body[2].lvalues[1].type = T.num
annotateT.body[2].rvalues[1].type = T.num
annotateT.body[2].type = T.num
annotateT.body[3].expr.type = T.num
annotateT.body[3].type = T.num
annotateT.rets[1].type = T.num
annotateT.type = T.arrow({T.num},{T.num})
test.asteq(annotate._typed_ast, annotateT, {'type'})


------------------------------------------------------------------------------
-- infering numeric types without clear type annotations

local answer = 42
simpl retanswer()
  return answer
end
local retanswerT = get_ast_copy(retanswer)
-- decorate types
retanswerT.rets[1].type = T.num
retanswerT.type = T.arrow({},{T.num})
test.asteq(retanswer._typed_ast, retanswerT, {'type'})

local pinum = 3.1415
simpl retpi()
  return pinum
end
local retpiT = get_ast_copy(retpi)
-- decorate types
retpiT.rets[1].type = T.num
retpiT.type = T.arrow({},{T.num})
test.asteq(retpi._typed_ast, retpiT, {'type'})

simpl voidfn() end
local voidfnT = get_ast_copy(voidfn)
voidfnT.type = T.arrow({},{})
test.asteq(voidfn._typed_ast, voidfnT, {'type'})


simpl doubleassign()
  var x : T.num
  var y : T.num
  x,y = 1,2
  return x,y
end
local doubleassignT = get_ast_copy(doubleassign)
-- zero out annotations
doubleassignT.body[1].annotation = nil
doubleassignT.body[2].annotation = nil
doubleassignT.body[3].rvalues[1].valuetype = nil
doubleassignT.body[3].rvalues[2].valuetype = nil
-- decorate types
doubleassignT.body[1].type = T.num
doubleassignT.body[2].type = T.num
doubleassignT.body[3].lvalues[1].type = T.num
doubleassignT.body[3].lvalues[2].type = T.num
doubleassignT.body[3].rvalues[1].type = T.num
doubleassignT.body[3].rvalues[2].type = T.num
doubleassignT.body[3].type = T.tuple({T.num, T.num})
doubleassignT.rets[1].type = T.num
doubleassignT.rets[2].type = T.num
doubleassignT.type = T.arrow({},{T.num,T.num})

------------------------------------------------------------------------------

simpl domath(x : T.num)
  var y = x * x
  return 1.0f * y + 32 * -x + 12.0f, 3.0/1.5
end
local domathT = get_ast_copy(domath)
-- zero out annotations
domathT.args[1].annotation = nil
domathT.rets[1].lhs.lhs.lhs.valuetype = nil
domathT.rets[1].lhs.rhs.lhs.valuetype = nil
domathT.rets[1].rhs.valuetype = nil
domathT.rets[2].lhs.valuetype = nil
domathT.rets[2].rhs.valuetype = nil
-- decorate types
domathT.args[1].type = T.num
domathT.body[1].expr.lhs.type = T.num
domathT.body[1].expr.rhs.type = T.num
domathT.body[1].expr.type = T.num
domathT.body[1].type = T.num
domathT.rets[1].lhs.lhs.lhs.type = T.num
domathT.rets[1].lhs.lhs.rhs.type = T.num
domathT.rets[1].lhs.lhs.type = T.num
domathT.rets[1].lhs.rhs.lhs.type = T.num
domathT.rets[1].lhs.rhs.rhs.expr.type = T.num
domathT.rets[1].lhs.rhs.rhs.type = T.num
domathT.rets[1].lhs.rhs.type = T.num
domathT.rets[1].lhs.type = T.num
domathT.rets[1].rhs.type = T.num
domathT.rets[1].type = T.num
domathT.rets[2].lhs.type = T.num
domathT.rets[2].rhs.type = T.num
domathT.rets[2].type = T.num
domathT.type = T.arrow({T.num},{T.num,T.num})
test.asteq(domath._typed_ast, domathT, {'type'})


simpl simplexpr()
  12 + 24
end
local simplexprT = get_ast_copy(simplexpr)
-- zero out annotations
simplexprT.body[1].expr.lhs.valuetype = nil
simplexprT.body[1].expr.rhs.valuetype = nil
-- decorate types
simplexprT.body[1].expr.lhs.type = T.num
simplexprT.body[1].expr.rhs.type = T.num
simplexprT.body[1].expr.type = T.num
simplexprT.type = T.arrow({},{})
test.asteq(simplexpr._typed_ast, simplexprT, {'type'})


simpl booltest()
  return not false
end
local booltestT = get_ast_copy(booltest)
-- decorate types
booltestT.rets[1].expr.type = T.bool
booltestT.rets[1].type = T.bool
booltestT.type = T.arrow({},T.bool)
test.asteq(booltest._typed_ast, booltestT, {'type'})


simpl strtest()
  'foobar'
end
local strtestT = get_ast_copy(strtest)
strtestT.body[1].expr.type = T.internal('foobar')
strtestT.type = T.arrow({},{})
test.asteq(strtest._typed_ast, strtestT, {'type'})

------------------------------------------------------------------------------


test.fail(function()
  local simpl foo( x ) end
end, "argument must be annotated")


test.fail(function()
  local simpl foo( x : T.error ) end
end, "arguments may only have value types")

test.fail(function()
  local simpl foo()
    var x
  end
end,  "cannot declare variables without providing either a type "..
      "or initial value")

test.fail(function()
  local simpl foo( x : T.bool )
    var y : T.num = x
  end
end, "Cannot assign expression of type 'bool' to a variable of type 'num'")

test.fail(function()
  local simpl foo()
    var y : T.arrow({},{})
  end
end, "cannot introduce variables with non%-value type")

test.fail(function()
  local simpl mismatch_assign()
    var y : T.num
    y = 0,1
  end
end, "assignment has a mismatched number of terms")

test.fail(function()
  local simpl foo()
    return x
  end
end, "variable 'x' is undefined")

test.fail(function()
  local simpl foo()
    return not 0
  end
end, "unary 'not' expects a bool operand")

test.fail(function()
  local simpl foo()
    return - true
  end
end, "'-' expects a numeric operand")


------------------------------------------------------------------------------

-- binary operations

test.fail(function()
  local simpl foo()
    return 23 + 'nonval'
  end
end, "cannot combine non%-values using '%+'")

test.fail(function()
  local simpl foo()
    return 23 and true
  end
end, "expected boolean operands")

test.fail(function()
  local simpl foo()
    return false <= 1
  end
end, "expected numeric operands")

test.fail(function()
  local simpl foo()
    return 3 + (1 - true)
  end
end, "expected numeric operands")

test.fail(function()
  local simpl foo()
    return 1 * true
  end
end, "expected numeric operands")

test.fail(function()
  local simpl foo()
    return true / 1.0
  end
end, "expected numeric operands")

------------------------------------------------------------------------------

-- Casts via call syntax

local simpl castround(x : T.num)
  return T.bool(x)
end
local castroundT = get_ast_copy(castround)
-- zero out annotations
castroundT.args[1].annotation = nil
-- decorate types
castroundT.args[1].type = T.num
  -- rewrite collapse of call AST into cast node
castroundT.rets[1].args[1].type = T.num
castroundT.rets[1] = A.Cast:NewFrom(castroundT.rets[1],
                                    {expr = castroundT.rets[1].args[1]})
                           :SetVals{ type = T.bool }
castroundT.type = T.arrow(T.num,T.bool)
test.asteq(castround._typed_ast, castroundT, {'type'})

test.fail(function()
  local simpl foo()
    return T.num(34.5, 2.3)
  end
end, "typecasts expect a single argument")

local voidfntype = T.arrow({},T.num)
test.fail(function()
  local simpl foo()
    return voidfntype(34.5)
  end
end, "cannot cast to non%-value type")

test.fail(function()
  local simpl foo()
    return T.num("not a value")
  end
end, "cannot cast from a non%-value type")

test.fail(function()
  local simpl foo()
    var x = 3.0
    return x(true)
  end
end, "expected function to call")

local random_obj = {}
test.fail(function()
  local simpl foo()
    return random_obj(true)
  end
end, "expected function to call")


------------------------------------------------------------------------------


