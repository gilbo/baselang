local test  = require 'tests.test'
-- call to maybe install coverage analysis
require 'typelang.typelang'

local T     = require 'typelang.src.types'


---------------------------------------
-- Test creation of various AST nodes 

-- dummy test
test.eq(T.istype('foo'), false)

-- test that the primitive types are present
test.eq(T.istype(T.int32),  true)
test.eq(T.istype(T.uint64), true)
test.eq(T.istype(T.bool),   true)
test.eq(T.istype(T.float),  true)
test.eq(T.istype(T.double), true)

local prims = {
  ['int32' ] = int32,
  ['uint64'] = uint64,
  ['bool'  ] = bool,
  ['float' ] = float,
  ['double'] = double,
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
    if name == 'int32' or name == 'uint64' then
      test.eq(typ:isintegral(), true)
    else
      test.eq(typ:isintegral(), false)
    end
  end

  -- check simple derivations
  test.eq(typ:terratype(), terra_type)
  test.eq(tostring(typ), name)

  -- check coercion identities
  test.eq(typ:iscoercableto(typ), true)
  test.eq(typ:coerceto(typ), typ)
  test.eq(typ:join(typ), typ)
end

-- check a couple of select coercion identities
test.eq(T.float:iscoercableto(T.double), true)
test.eq(T.float:iscoercableto(T.int), false)
test.eq(T.double:iscoercableto(T.uint64), false)
test.eq(T.int32:iscoercableto(T.double), true)
test.eq(T.int32:iscoercableto(T.bool), false)
test.eq(T.int32:iscoercableto(T.uint64), true)
test.eq(T.int32:iscoercableto(T.float), false) -- controversial

test.eq(T.float:join(T.int32), T.double) -- controversial
test.eq(T.int32:join(T.uint64), T.uint64)
test.eq(T.uint64:join(T.int32), T.uint64)
test.eq(T.uint64:coerceto(T.int32), T.error)

test.eq(T.error:iscoercableto(T.double), false)



-- smattering of string tests
test.eq(tostring(T.error), 'error')
test.eq(tostring(T.internal('foo')), 'internal(foo)')



-- weird tests for error and internal
test.eq(T.error:terratype(), T.internal({}):terratype()) -- both {}


-- lua <--> terra conversions
test.eq(T.terratoluaval(T.luatoterraval(42, T.int32), T.int32), 42)
test.eq(T.terratoluaval(T.luatoterraval(42, T.float), T.float), 42)
test.eq(T.terratoluaval(T.luatoterraval(42, T.double), T.double), 42)
test.eq(T.terratoluaval(T.luatoterraval(42, T.uint64), T.uint64), 42)

test.eq(T.terratoluaval(T.luatoterraval(true, T.bool), T.bool), true)
test.eq(T.terratoluaval(T.luatoterraval(false, T.bool), T.bool), false)
-- weird edge case for booleans...
local btval = terralib.new(bool, true)
local bfval = terralib.new(bool, false)
test.eq(T.terratoluaval(btval, T.bool), true)
test.eq(T.terratoluaval(bfval, T.bool), false)

test.eq(T.checkluaval(1, T.bool), false)
test.eq(T.checkluaval(true, T.int32), false)
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

local itoi = T.arrow({T.int32},{T.int32})
test.eq(itoi,T.arrow({T.int32},{T.int32}))
test.eq(T.istype(itoi), true)
test.eq(itoi:isarrow(), true)
test.eq(itoi:isvalue(), false)
test.eq(tostring(itoi), "{int32}->{int32}")
test.aeq(itoi:argtypes(),{T.int32})
test.aeq(itoi:rettypes(),{T.int32})

local retpairs = T.arrow({},{T.float,T.float})
test.eq(T.istype(retpairs), true)
test.eq(retpairs:isarrow(), true)
test.eq(retpairs:isvalue(), false)
test.eq(tostring(retpairs), "{}->{float,float}")
test.aeq(retpairs:argtypes(),{})
test.aeq(retpairs:rettypes(),{T.float,T.float})

local argtriple = T.arrow({T.float,T.bool,T.double},{})
test.eq(T.istype(argtriple), true)
test.eq(argtriple:isarrow(), true)
test.eq(argtriple:isvalue(), false)
test.eq(tostring(argtriple), "{float,bool,double}->{}")
test.aeq(argtriple:argtypes(),{T.float,T.bool,T.double})
test.aeq(argtriple:rettypes(),{})


test.fail(function()
  T.arrow(24, T.int32)
end, 'invalid arg; expecting 2 types or lists')

test.fail(function()
  T.arrow({1}, {})
end, 'invalid type at tuple position 1')

test.fail(function()
  T.arrow({T.int32,1}, {})
end, 'invalid type at tuple position 2')

test.fail(function()
  T.arrow({}, {T.error})
end, 'invalid type at tuple position 1')









