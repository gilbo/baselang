local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'
local A     = require 'typelang.src.ast'
local T     = require 'typelang.src.types'


------------------------------------------------------------------------------

local function get_ast_copy(fn)
  return fn._decl_ast:clone()
end

local function insert_cast(ast, typ)
  return A.Cast:NewFrom(ast,{expr = ast}):SetVals{ type = typ }
end

------------------------------------------------------------------------------

typel retzero(a : T.int32, b : T.int32)
  return 0
end
local retzeroT = get_ast_copy(retzero)
-- zero out annotations
retzeroT.args[1].annotation = nil
retzeroT.args[2].annotation = nil
retzeroT.rets[1].valuetype = nil
-- manually decorate types
retzeroT.args[1].type = T.int32
retzeroT.args[2].type = T.int32
retzeroT.rets[1].type = T.int32
retzeroT.type = T.arrow({T.int32,T.int32},{T.int32})
-- check that type decorations match
test.asteq(retzero._typed_ast, retzeroT, {'type'})


typel retpair()
  return 0, 1
end
local retpairT = get_ast_copy(retpair)
-- zero out annotations
retpairT.rets[1].valuetype = nil
retpairT.rets[2].valuetype = nil
-- manually decorate types
retpairT.rets[1].type = T.int32
retpairT.rets[2].type = T.int32
retpairT.type = T.arrow({},{T.int32,T.int32})
-- check that type decorations match
test.asteq(retpair._typed_ast, retpairT, {'type'})


typel doshadow(a : T.double, b : T.double)
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
doshadowT.args[1].type = T.double
doshadowT.args[2].type = T.double
doshadowT.body[1].type = T.double
doshadowT.body[1].expr.type = T.double
doshadowT.body[2].body[1].type = T.double
doshadowT.body[2].body[1].expr.type = T.double
doshadowT.body[2].body[1].expr.lhs.type = T.double
doshadowT.body[2].body[1].expr.rhs.type = T.double
doshadowT.body[3].type = T.double
doshadowT.body[3].lvalues[1].type = T.double
doshadowT.body[3].rvalues[1].type = T.double
doshadowT.body[3].rvalues[1].lhs.type = T.double
doshadowT.body[3].rvalues[1].rhs.type = T.double
doshadowT.rets[1].type = T.double
doshadowT.type = T.arrow({T.double,T.double},{T.double})
test.asteq(doshadow._typed_ast, doshadowT, {'type'})



typel annotate( x : T.int32 )
  var y : T.double
  y = x
  var z : T.double = y
  return z
end
local annotateT = get_ast_copy(annotate)
-- zero out annotations
annotateT.args[1].annotation = nil
annotateT.body[1].annotation = nil
annotateT.body[3].annotation = nil
-- insert casts due to coercions
annotateT.body[2].rvalues[1] =
  insert_cast(annotateT.body[2].rvalues[1], T.double)
-- manually decorate types
annotateT.args[1].type = T.int32
annotateT.body[1].type = T.double
annotateT.body[2].lvalues[1].type = T.double
annotateT.body[2].rvalues[1].expr.type = T.int32
annotateT.body[2].rvalues[1].type = T.double
annotateT.body[2].type = T.double
annotateT.body[3].expr.type = T.double
annotateT.body[3].type = T.double
annotateT.rets[1].type = T.double
annotateT.type = T.arrow({T.int32},{T.double})
test.asteq(annotate._typed_ast, annotateT, {'type'})


------------------------------------------------------------------------------
-- infering numeric types without clear type annotations

local answer = 42
typel retanswer()
  return answer
end
local retanswerT = get_ast_copy(retanswer)
-- decorate types
retanswerT.rets[1].type = T.int32
retanswerT.type = T.arrow({},{T.int32})
test.asteq(retanswer._typed_ast, retanswerT, {'type'})

local pinum = 3.1415
typel retpi()
  return pinum
end
local retpiT = get_ast_copy(retpi)
-- decorate types
retpiT.rets[1].type = T.double
retpiT.type = T.arrow({},{T.double})
test.asteq(retpi._typed_ast, retpiT, {'type'})

typel voidfn() end
local voidfnT = get_ast_copy(voidfn)
voidfnT.type = T.arrow({},{})
test.asteq(voidfn._typed_ast, voidfnT, {'type'})


typel doubleassign()
  var x : T.int32
  var y : T.double
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
doubleassignT.body[1].type = T.int32
doubleassignT.body[2].type = T.double
doubleassignT.body[3].lvalues[1].type = T.int32
doubleassignT.body[3].lvalues[2].type = T.double
doubleassignT.body[3].rvalues[1].type = T.int32
doubleassignT.body[3].rvalues[2].type = T.int32
doubleassignT.body[3].rvalues[2] =
  insert_cast(doubleassignT.body[3].rvalues[2], T.double)
doubleassignT.body[3].type = T.tuple({T.int32, T.double})
doubleassignT.rets[1].type = T.int32
doubleassignT.rets[2].type = T.double
doubleassignT.type = T.arrow({},{T.int32,T.double})

------------------------------------------------------------------------------

typel domath(x : T.float)
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
domathT.args[1].type = T.float
domathT.body[1].expr.lhs.type = T.float
domathT.body[1].expr.rhs.type = T.float
domathT.body[1].expr.type = T.float
domathT.body[1].type = T.float
domathT.rets[1].lhs.lhs.lhs.type = T.float
domathT.rets[1].lhs.lhs.rhs.type = T.float
domathT.rets[1].lhs.lhs.type = T.float
domathT.rets[1].lhs.rhs.lhs.type = T.int32
domathT.rets[1].lhs.rhs.rhs.expr.type = T.float
domathT.rets[1].lhs.rhs.rhs.type = T.float
domathT.rets[1].lhs.rhs.lhs = insert_cast(domathT.rets[1].lhs.rhs.lhs,T.double)
domathT.rets[1].lhs.rhs.rhs = insert_cast(domathT.rets[1].lhs.rhs.rhs,T.double)
domathT.rets[1].lhs.rhs.type = T.double
domathT.rets[1].lhs.lhs = insert_cast(domathT.rets[1].lhs.lhs, T.double)
domathT.rets[1].lhs.type = T.double
domathT.rets[1].rhs.type = T.float
domathT.rets[1].rhs = insert_cast(domathT.rets[1].rhs, T.double)
domathT.rets[1].type = T.double
domathT.rets[2].lhs.type = T.double
domathT.rets[2].rhs.type = T.double
domathT.rets[2].type = T.double
domathT.type = T.arrow({T.float},{T.double,T.double})
test.asteq(domath._typed_ast, domathT, {'type'})


typel simplexpr()
  12 + 24
end
local simplexprT = get_ast_copy(simplexpr)
-- zero out annotations
simplexprT.body[1].expr.lhs.valuetype = nil
simplexprT.body[1].expr.rhs.valuetype = nil
-- decorate types
simplexprT.body[1].expr.lhs.type = T.int32
simplexprT.body[1].expr.rhs.type = T.int32
simplexprT.body[1].expr.type = T.int32
simplexprT.type = T.arrow({},{})
test.asteq(simplexpr._typed_ast, simplexprT, {'type'})


typel booltest()
  return not false
end
local booltestT = get_ast_copy(booltest)
-- decorate types
booltestT.rets[1].expr.type = T.bool
booltestT.rets[1].type = T.bool
booltestT.type = T.arrow({},T.bool)
test.asteq(booltest._typed_ast, booltestT, {'type'})


typel strtest()
  'foobar'
end
local strtestT = get_ast_copy(strtest)
strtestT.body[1].expr.type = T.internal('foobar')
strtestT.type = T.arrow({},{})
test.asteq(strtest._typed_ast, strtestT, {'type'})

------------------------------------------------------------------------------


test.fail(function()
  local typel foo( x ) end
end, "argument must be annotated")


test.fail(function()
  local typel foo( x : T.error ) end
end, "arguments may only have value types")

test.fail(function()
  local typel foo()
    var x
  end
end,  "cannot declare variables without providing either a type "..
      "or initial value")

test.fail(function()
  local typel foo( x : T.float )
    var y : T.int32 = x
  end
end, "Could not coerce expression of type")

test.fail(function()
  local typel foo()
    var y : T.arrow({},{})
  end
end, "cannot introduce variables with non%-value type")

test.fail(function()
  local typel mismatch_assign()
    var y : T.int32
    y = 0,1
  end
end, "assignment has a mismatched number of terms")

test.fail(function()
  local typel foo()
    return x
  end
end, "variable 'x' is undefined")

test.fail(function()
  local typel foo()
    return not 0
  end
end, "unary 'not' expects a bool operand")

test.fail(function()
  local typel foo()
    return - true
  end
end, "'-' expects a numeric operand")


------------------------------------------------------------------------------

-- binary operations

test.fail(function()
  local typel foo()
    return 23 + 'nonval'
  end
end, "cannot combine non%-values using '%+'")

test.fail(function()
  local typel foo()
    return 23 and true
  end
end, "expected boolean operands")

test.fail(function()
  local typel foo()
    return false <= 1
  end
end, "expected numeric operands")

test.fail(function()
  local typel foo()
    return 3 + (1 - true)
  end
end, "expected numeric operands")

test.fail(function()
  local typel foo()
    return 1 * true
  end
end, "expected numeric operands")

test.fail(function()
  local typel foo()
    return true / 1.0
  end
end, "expected numeric operands")

------------------------------------------------------------------------------

-- Casts via call syntax

local typel castround(x : T.double)
  return T.int32(x)
end
local castroundT = get_ast_copy(castround)
-- zero out annotations
castroundT.args[1].annotation = nil
-- decorate types
castroundT.args[1].type = T.double
  -- rewrite collapse of call AST into cast node
castroundT.rets[1].args[1].type = T.double
castroundT.rets[1] = insert_cast(castroundT.rets[1].args[1], T.int32)
castroundT.type = T.arrow(T.double,T.int32)
test.asteq(castround._typed_ast, castroundT, {'type'})

test.fail(function()
  local typel foo()
    return T.int32(34.5, 2.3)
  end
end, "typecasts expect a single argument")

local voidfntype = T.arrow({},T.double)
test.fail(function()
  local typel foo()
    return voidfntype(34.5)
  end
end, "cannot cast to non%-value type")

test.fail(function()
  local typel foo()
    return T.double("not a value")
  end
end, "cannot cast from a non%-value type")

test.fail(function()
  local typel foo()
    var x = 3.0
    return x(true)
  end
end, "expected function to call")

local random_obj = {}
test.fail(function()
  local typel foo()
    return random_obj(true)
  end
end, "expected function to call")


------------------------------------------------------------------------------


