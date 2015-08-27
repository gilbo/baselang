local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'
local A     = require 'typelang.src.ast'
local T     = require 'typelang.src.types'

-- flip a flag to enable unit testing
Lib._UNIT_TEST_PARSER = true

------------------------------------------------------------------------------


typel retzero(a, b)
  return 0
end
local retzeroAST = A.Function:New{
  name='retzero',
  args={ A.ArgDecl:New{ name = 'a' }, A.ArgDecl:New{ name = 'b' } },
  body={},
  rets={A.Number:New{ value = 0, valuetype = T.int32 }},
}
test.asteq(retzero, retzeroAST)

typel retpair()
  return 0, 1
end
local retpairAST = A.Function:New{
  name='retpair',
  args={},
  body={},
  rets={ A.Number:New{ value=0, valuetype = T.int32 },
         A.Number:New{ value=1, valuetype = T.int32 } },
}
test.asteq(retpair, retpairAST)


typel domath(x)
  var y = x * x
  return 1.0f * y + 32f * -x + 12.0f
end
local domathAST = A.Function:New{
  name = 'domath',
  args = { A.ArgDecl:New { name = 'x' } },
  body = { A.DeclStmt:New {
    name = 'y',
    expr = A.BinaryOp:New { op = '*',
      lhs = A.Name:New { value = 'x' },
      rhs = A.Name:New { value = 'x' },
    }
  }, },
  rets = { A.BinaryOp:New { op = '+',
    lhs = A.BinaryOp:New { op = '+',
      lhs = A.BinaryOp:New { op = '*',
        lhs = A.Number:New { value = 1, valuetype = T.float },
        rhs = A.Name:New { value = 'y' },
      },
      rhs = A.BinaryOp:New { op = '*',
        lhs = A.Number:New { value = 32, valuetype = T.float },
        rhs = A.UnaryOp:New { op = '-', expr = A.Name:New { value = 'x' } },
      },
    },
    rhs = A.Number:New { value = 12, valuetype = T.float },
  } },
}
test.asteq(domath, domathAST)


typel doblock()
  var x = 1
  do
    x = x + x
  end
end
local doblockAST = A.Function:New {
  name = 'doblock',
  args = {},
  body = {
    A.DeclStmt:New {
      name = 'x',
      expr = A.Number:New { value = 1, valuetype = T.int32 },
    },
    A.DoStmt:New { body = {
      A.Assignment:New {
        lvalues = { A.Name:New { value = 'x' } },
        rvalues = { A.BinaryOp:New { op = '+',
          lhs = A.Name:New { value = 'x' },
          rhs = A.Name:New { value = 'x' },
        } },
      }
    } },
  },
  rets = {},
}
test.asteq(doblock, doblockAST)


------------------------------------------------------------------------------


typel emptyfunc() end
local emptyfuncAST = A.Function:New {
  name = 'emptyfunc',
  args = {},
  body = {},
  rets = {},
}
test.asteq(emptyfunc, emptyfuncAST)


local exprfunc = typel() return 32 end
local exprfuncAST = A.Function:New {
  name = exprfunc.name, -- cheat cause we don't know anonymous name
  args = {},
  body = {},
  rets = { A.Number:New { value = 32, valuetype = T.int32 } },
}
test.asteq(exprfunc, exprfuncAST)


local M = {}
typel M.foo() return false end
local MfooAST = A.Function:New {
  name = 'M.foo',
  args = {},
  body = {},
  rets = { A.Bool:New { value = false } },
}
test.asteq(M.foo, MfooAST)


------------------------------------------------------------------------------


typel annotate( x : T.int32 )
  var y : T.double
  y = T.double(x)
  var z : T.double = y
  return z
end
local annotateAST = A.Function:New {
  name = 'annotate',
  args = {
    A.ArgDecl:New { name = 'x', annotation = annotate.args[1].annotation }
  },
  body = {
    A.DeclStmt:New { name = 'y', annotation = annotate.body[1].annotation },
    A.Assignment:New {
      lvalues = { A.Name:New { value = 'y' } },
      rvalues = { A.Call:New {
        base = A.Lookup:New { base = A.Name:New { value='T' },
                              arg  = A.String:New{ value='double' } },
        args = { A.Name:New { value = 'x' } },
      } },
    },
    A.DeclStmt:New {
      name = 'z',
      annotation = annotate.body[3].annotation,
      expr = A.Name:New { value = 'y' },
    },
  },
  rets = { A.Name:New { value = 'z' } },
}
test.asteq(annotate, annotateAST)


local typel retone()
  noop
  return retzero() + 1
end
local retoneAST = A.Function:New{
  name='retone',
  args={},
  body={ A.ExprStmt:New { expr = A.Name:New { value = 'noop' } }},
  rets = { A.BinaryOp:New { op = '+',
    lhs = A.Call:New { base = A.Name:New { value = 'retzero' },
                       args = {}, },
    rhs = A.Number:New { value = 1, valuetype = T.int32 },
  } },
}
test.asteq(retone, retoneAST)


local typel doubleassign()
  var x : T.int32
  var y : T.int32
  x,y = 1,2
  return x,y
end
local doubleassignAST = A.Function:New{
  name='doubleassign',
  args={},
  body={
    A.DeclStmt:New { name = 'x',
                     annotation = doubleassign.body[1].annotation },
    A.DeclStmt:New { name = 'y',
                     annotation = doubleassign.body[2].annotation },
    A.Assignment:New {
      lvalues = { A.Name:New { value = 'x' }, A.Name:New { value = 'y' } },
      rvalues = { A.Number:New { value = 1, valuetype = T.int32 },
                  A.Number:New { value = 2, valuetype = T.int32 } },
    },
  },
  rets = { A.Name:New { value = 'x' }, A.Name:New { value = 'y' } },
}
test.asteq(doubleassign, doubleassignAST)



local typel swapfields( obj )
  var temp = obj['f1']
  obj.f1 = obj.f2
  obj['f2'] = temp
end
local swapfieldsAST = A.Function:New {
  name = 'swapfields',
  args = { A.ArgDecl:New { name = 'obj' } },
  body = {
    A.DeclStmt:New { name = 'temp',
      expr = A.Lookup:New { base = A.Name:New { value = 'obj' },
        arg = A.String:New { value='f1' },
      }},
    A.Assignment:New {
      lvalues = { A.Lookup:New { base = A.Name:New { value = 'obj' },
        arg = A.String:New { value='f1' },
      } },
      rvalues = { A.Lookup:New { base = A.Name:New { value = 'obj' },
        arg = A.String:New { value='f2' },
      } },
    },
    A.Assignment:New {
      lvalues = { A.Lookup:New { base = A.Name:New { value = 'obj' },
        arg = A.String:New { value='f2' },
      } },
      rvalues = { A.Name:New { value = 'temp' } },
    },
  },
  rets = {},
}
test.asteq(swapfields, swapfieldsAST)





