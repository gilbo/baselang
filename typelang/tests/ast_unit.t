local test  = require 'tests.test'
-- call to maybe install coverage
require 'typelang.typelang'

local A     = require 'typelang.src.ast'


------------------------------------
-- Cannot create abstract AST nodes
test.fail(function()
  local exp   = A.Expression:New()
end, 'cannot create new abstract AST')

---------------------------------------
-- Test creation of various AST nodes 

local somesym     = A.NewSymbol('somesym')
local somename    = A.Name:New { value=somesym }
local somenumber  = A.Number:New { value=42 }
local somestring  = A.String:New { value='foobar' }
local somebool    = A.Bool:New { value=true }

local someuop     = A.UnaryOp:New { op='-', expr=somenumber }
local somebop     = A.BinaryOp:New { op='+', lhs=somenumber, rhs=somenumber }

local somelookup  = A.Lookup:New {
  base = somename, arg = somename,
}
local somecall    = A.Call:New { base=somename, args={} }


local someexpr    = A.ExprStmt:New { expr = somebop }
local someargdecl = A.ArgDecl:New { name=somesym }

local somedo      = A.DoStmt:New { body={someexpr} }
local someasgn    = A.Assignment:New { lvalues={somename}, rvalues={somebop} }
local somedecl    = A.DeclStmt:New { name=somesym }
local somelet     = A.Let:New { block={someexpr}, expr=someuop }
local somefunc    = A.Function:New {
  name='foo',
  args={someargdecl},
  body={somedo},
  rets={somebool},
}

test.fail(function()
  local dummy = A.Function:New { args={someargdecl}, body={somedo} }
end, 'Could not create AST because of bad shape')

somefunc:printpretty()
test.eq(somefunc:depth(), 5)
test.eq(somefunc:size(), 8)

A.NewCopyPass {
  passname = "typecheck",
  copymembers = {"annotation1"},
  defaultvals = {
    node_type = 3,
  },
  --verbose=true,
}
A.NewInertPass {
  passname='analysis',
  --verbose = true,
}


