local test  = require 'tests.test'
-- call to maybe install coverage analysis
require 'simplang.simplang'

local T     = require 'simplang.src.types'


---------------------------------------
-- Test creation of various AST nodes 

-- dummy test
test.eq(T.istype('foo'), false)

-- test that the primitive types are present
test.eq(T.istype(T.bool), true)
test.eq(T.istype(T.num),  true)

local prims = {
  ['bool'] = bool,
  ['num' ] = double,
}

for name,terra_type in pairs(prims) do
  -- test that type is there
  local typ = T[name]
  test.eq(T.istype(typ), true)

  -- properties of type
  test.eq(typ:isprimitive(), true)
  test.eq(typ:isvalue(), true)

  -- check various sub-properties
  if name == 'bool' then
    test.eq(typ:islogical(), true)
    test.eq(typ:isnumeric(), false)
  else
    test.eq(typ:islogical(), false)
    test.eq(typ:isnumeric(), true)
  end

  -- check simple derivations
  test.eq(typ:terratype(), terra_type)
  test.eq(tostring(typ), name)

end


-- smattering of string tests
test.eq(tostring(T.error), 'error')
test.eq(tostring(T.internal('foo')), 'internal(foo)')


-- weird tests for error and internal
test.eq(T.error:terratype(), T.internal({}):terratype()) -- both {}


-- lua <--> terra conversions
test.eq(T.terratoluaval(T.luatoterraval(42, T.num), T.num), 42)
test.eq(T.terratoluaval(T.luatoterraval(true, T.bool), T.bool), true)
test.eq(T.terratoluaval(T.luatoterraval(false, T.bool), T.bool), false)
-- weird edge case for booleans...
local btval = terralib.new(bool, true)
local bfval = terralib.new(bool, false)
test.eq(T.terratoluaval(btval, T.bool), true)
test.eq(T.terratoluaval(bfval, T.bool), false)

test.eq(T.checkluaval(1, T.bool), false)
test.eq(T.checkluaval(true, T.num), false)
test.eq(T.checkluaval(true, T.error), false)



-- Arrows
local voidarrow = T.arrow({},{})
test.eq(voidarrow, T.arrow({},{}))
test.eq(voidarrow, T.arrow({},{})) -- double check cause of caching
test.eq(T.istype(voidarrow), true)
test.eq(voidarrow:isarrow(), true)
test.eq(voidarrow:isvalue(), false)
test.eq(tostring(voidarrow), "{}->{}")
test.aeq(voidarrow:argtypes(),{})
test.aeq(voidarrow:rettypes(),{})

local itoi = T.arrow({T.num},{T.num})
test.eq(itoi,T.arrow({T.num},{T.num}))
test.eq(T.istype(itoi), true)
test.eq(itoi:isarrow(), true)
test.eq(itoi:isvalue(), false)
test.eq(tostring(itoi), "{num}->{num}")
test.aeq(itoi:argtypes(),{T.num})
test.aeq(itoi:rettypes(),{T.num})

local retpairs = T.arrow({},{T.num,T.num})
test.eq(T.istype(retpairs), true)
test.eq(retpairs:isarrow(), true)
test.eq(retpairs:isvalue(), false)
test.eq(tostring(retpairs), "{}->{num,num}")
test.aeq(retpairs:argtypes(),{})
test.aeq(retpairs:rettypes(),{T.num,T.num})

local argtriple = T.arrow({T.num,T.bool,T.num},{})
test.eq(T.istype(argtriple), true)
test.eq(argtriple:isarrow(), true)
test.eq(argtriple:isvalue(), false)
test.eq(tostring(argtriple), "{num,bool,num}->{}")
test.aeq(argtriple:argtypes(),{T.num,T.bool,T.num})
test.aeq(argtriple:rettypes(),{})


test.fail(function()
  T.arrow(24, T.num)
end, 'invalid arg; expecting 2 types or lists')

test.fail(function()
  T.arrow({1}, {})
end, 'invalid type at tuple position 1')

test.fail(function()
  T.arrow({T.num,1}, {})
end, 'invalid type at tuple position 2')

test.fail(function()
  T.arrow({}, {T.error})
end, 'invalid type at tuple position 1')









