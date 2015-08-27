local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'
local A     = require 'typelang.src.ast'
local T     = require 'typelang.src.types'

-- flip a flag to enable unit testing
Lib._UNIT_TEST_SPECIALIZER = true

------------------------------------------------------------------------------

-- test that names have been converted into symbols
-- test that symbols disambiguate shadowing of variables correctly

typel retzero(a, b)
  return 0
end
test.eq(A.issymbol(retzero.args[1].name), true)
test.eq(A.issymbol(retzero.args[2].name), true)
local retzeroAST = A.Function:New{
  name='retzero',
  args={ A.ArgDecl:New{ name = retzero.args[1].name },
         A.ArgDecl:New{ name = retzero.args[2].name } },
  body={},
  rets={A.Number:New{ value = 0, valuetype = T.int32 }},
}
test.asteq(retzero, retzeroAST)

-- shadowing
typel doshadow(a, b)
  var x = 0
  do
    var a = x + b
  end
  x = a + b
  return x
end
local shadowA    = doshadow.args[1].name
local shadowB    = doshadow.args[2].name
local shadowX    = doshadow.body[1].name
local shadowSubA = doshadow.body[2].body[1].name
test.eq(A.issymbol(shadowA), true)
test.eq(A.issymbol(shadowB), true)
test.eq(A.issymbol(shadowX), true)
test.eq(A.issymbol(shadowSubA), true)
test.neq(shadowSubA, shadowA)
local doshadowAST = A.Function:New{
  name='doshadow',
  args={ A.ArgDecl:New{ name = shadowA },
         A.ArgDecl:New{ name = shadowB } },
  body={
    A.DeclStmt:New { name = shadowX,
                     expr = A.Number:New { value = 0, valuetype = T.int32 } },
    A.DoStmt:New { body = {
      A.DeclStmt:New { name = shadowSubA,
        expr = A.BinaryOp:New { op = '+',
          lhs = A.Name:New { value = shadowX },
          rhs = A.Name:New { value = shadowB }
        }
      }
    } },
    A.Assignment:New {
      lvalues = { A.Name:New { value = shadowX } },
      rvalues = { A.BinaryOp:New { op = '+',
        lhs = A.Name:New { value = shadowA },
        rhs = A.Name:New { value = shadowB }
      } },
    },
  },
  rets={A.Name:New{ value = shadowX }},
}
test.asteq(doshadow, doshadowAST)


------------------------------------------------------------------------------

-- test annotation evaluation in various forms... (DECL STMT, and Function)

typel annotate( x : T.int32 )
  var y : T.double
  y = x
  var z : T.double = y
  return z
end
local annotateX = annotate.args[1].name
local annotateY = annotate.body[1].name
local annotateZ = annotate.body[3].name
test.eq(A.issymbol(annotateX), true)
test.eq(A.issymbol(annotateY), true)
test.eq(A.issymbol(annotateZ), true)
local annotateAST = A.Function:New {
  name = 'annotate',
  args = {
    A.ArgDecl:New { name = annotateX, annotation = T.int32 }
  },
  body = {
    A.DeclStmt:New { name = annotateY, annotation = T.double },
    A.Assignment:New {
      lvalues = { A.Name:New { value = annotateY } },
      rvalues = { A.Name:New { value=annotateX } },
    },
    A.DeclStmt:New {
      name = annotateZ,
      annotation = T.double,
      expr = A.Name:New { value = annotateY },
    },
  },
  rets = {A.Name:New { value = annotateZ }},
}
test.asteq(annotate, annotateAST)

------------------------------------------------------------------------------



-- test inlining of lua values
local answer = 42
typel retanswer()
  return answer
end
local retanswerAST = A.Function:New{
  name='retanswer',
  args={},
  body={},
  rets={A.Number:New{ value = answer }},
}
test.asteq(retanswer, retanswerAST)

-- test inlining using the constant object
local aconst = Lib.Constant(T.uint64, 42)
typel retaconst()
  return aconst
end
local retaconstAST = A.Function:New{
  name='retaconst',
  args={},
  body={},
  rets={A.Number:New{ value = aconst:get(), valuetype=T.uint64 }},
}
test.asteq(retaconst, retaconstAST)


-- test inlining and eager evaluation of table access for namespacing
local mod = { foo = 3, bar = { baz = 4 } }
typel namespace()
  return mod.foo + mod.bar.baz + mod.foo.bar
end
local namespaceAST = A.Function:New{
  name='namespace',
  args={},
  body={},
  rets={A.BinaryOp:New { op = '+',
    lhs = A.BinaryOp:New { op = '+',
      lhs = A.Number:New { value = 3 },
      rhs = A.Number:New { value = 4 }
    },
    rhs = A.Lookup:New {
      base = A.Number:New { value = 3 },
      arg  = A.String:New { value = 'bar' }
    }
  }},
}
test.asteq(namespace, namespaceAST)


-- Convert via inlining/constants other major types of values
local tval  = true
local cfval = Lib.Constant(T.bool, false)
typel multi_inline()
  var y = cfval
  return tval
end
local misymY = multi_inline.body[1].name
test.eq(A.issymbol(misymY), true)
local multi_inlineAST = A.Function:New {
  name='multi_inline',
  args={},
  body={ A.DeclStmt:New {
    name = misymY,
    expr = A.Bool:New { value = false }
  } },
  rets={A.Bool:New { value = true }},
}
test.asteq(multi_inline, multi_inlineAST)


------------------------------------------------------------------------------


test.fail(function()
  local typel foo( x : a+b ) end
end, "Error evaluating Lua expression")

test.fail(function()
  local typel foo( x : 3 ) end
end, "Expected type but found number")

test.fail(function()
  local somestr = 'foobar'
  local typel foo() var x = somestr end
end, "Could not successfully convert the Lua value referred to by 'somestr'")

test.fail(function()
  local typel foo() var x = somestr end
end, "variable 'somestr' is undefined")

test.fail(function()
  local mod = {}
  local typel foo() var x = mod.a end
end, "Could not find entry 'a' in lua table")

test.fail(function()
  local mod = { a = 'foobar' }
  local typel foo() var x = mod.a end
end, "lua table entry 'a' could not be successfully converted")









