local test  = require 'tests.test'
-- call to maybe install coverage analysis
require 'plexlang.plexlang'

local T     = require 'plexlang.src.types'


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
  test.eq(typ:isscalar(), true)
  test.eq(typ:isvalue(), true)
  test.eq(typ:istensor(), false)

  -- function to check a type for tensor-independent properties
  local function tensorindependenttests(sometyp)
    -- check various sub-properties
    if name == 'bool' then
      test.eq(sometyp:islogical(), true)
      test.eq(sometyp:isnumeric(), false)
    else
      test.eq(sometyp:islogical(), false)
      test.eq(sometyp:isnumeric(), true)
      if name == 'int32' or name == 'uint64' then
        test.eq(sometyp:isintegral(), true)
      else
        test.eq(sometyp:isintegral(), false)
      end
    end
  end
  tensorindependenttests(typ)

  -- check simple derivations
  test.eq(typ:terrabasetype(), typ:terratype())
  test.eq(typ:basetype(), typ)
  test.eq(typ:terratype(), terra_type)
  test.eq(tostring(typ), name)

  -- check coercion identities
  test.eq(typ:iscoercableto(typ), true)
  test.eq(typ:coerceto(typ), typ)
  test.eq(typ:join(typ), typ)

  -- construct some vector, matrix and tensor types and test them
  for dim1 = 2,4 do
    local vectyp = T.vector(typ, dim1)
    test.eq(vectyp, T.tensor(typ, dim1))
    if typ == T.float or typ == T.double then
      -- test that it's the same as the shorthand
      local shortname = 'vec'..tostring(dim1)
      if typ == T.float then shortname = shortname..'f'
                        else shortname = shortname..'d' end
      test.eq(vectyp, T[shortname])
    end

    -- test some properties
    test.eq(vectyp:isprimitive(), false)
    test.eq(vectyp:isscalar(), false)
    test.eq(vectyp:isvalue(), true)
    test.eq(vectyp:istensor(), true)
    test.eq(vectyp:isvector(), true)

    -- check other properties and derivations
    tensorindependenttests(vectyp)
    test.eq(vectyp:basetype(), typ)
    test.neq(vectyp:terrabasetype(), vectyp:terratype())
    test.eq(tostring(vectyp),
      'vector('..tostring(typ)..','..tostring(dim1)..')')

    -- check coercion identities
    test.eq(vectyp:iscoercableto(vectyp), true)
    test.eq(vectyp:coerceto(vectyp), vectyp)
    test.eq(vectyp:join(vectyp), vectyp)

    for dim2 = 2,4 do
      local mattyp = T.matrix(typ, dim1, dim2)
      test.eq(mattyp, T.tensor(typ, dim1, dim2))
      if typ == T.float or typ == T.double then
        -- test that it's the same as the shorthand
        local shortname = 'mat'..tostring(dim1)..'x'..tostring(dim2)
        if typ == T.float then shortname = shortname..'f'
                          else shortname = shortname..'d' end
        test.eq(mattyp, T[shortname])

        if dim1 == dim2 then
          local symname = 'mat'..tostring(dim1)
          if typ == T.float then symname = symname..'f'
                            else symname = symname..'d' end
          test.eq(mattyp, T[symname])
        end
      end

      -- test some properties
      test.eq(mattyp:isprimitive(), false)
      test.eq(mattyp:isscalar(), false)
      test.eq(mattyp:isvalue(), true)
      test.eq(mattyp:istensor(), true)
      test.eq(mattyp:isvector(), false)
      test.eq(mattyp:ismatrix(), true)

      -- check other properties and derivations
      tensorindependenttests(mattyp)
      test.eq(mattyp:basetype(), typ)
      test.neq(mattyp:terrabasetype(), mattyp:terratype())
      test.eq(tostring(mattyp),
        'matrix('..tostring(typ)..','..tostring(dim1)..
                                  ','..tostring(dim2)..')')

      -- check coercion identities
      test.eq(mattyp:iscoercableto(mattyp), true)
      test.eq(mattyp:coerceto(mattyp), mattyp)
      test.eq(mattyp:join(mattyp), mattyp)

    end
  end
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

test.eq(T.vec3d:coerceto(T.mat3d), T.error)
test.eq(T.double:join(T.vec4d), T.error) -- could be debated
test.eq(T.vec2f:coerceto(T.vec2d), T.vec2d)
test.eq(T.mat2x4f:join(T.matrix(T.int32,2,4)), T.mat2x4d)
test.eq(T.error:iscoercableto(T.double), false)



-- smattering of string tests
test.eq(tostring(T.error), 'error')
test.eq(tostring(T.internal('foo')), 'internal(foo)')

local t222d = T.tensor(T.double,2,2,2)
test.eq(tostring(t222d),'tensor(double,2,2,2)')
test.eq(t222d:isscalar(), false)
test.eq(t222d:isvector(), false)
test.eq(t222d:ismatrix(), false)
test.eq(t222d:istensor(), true)


-- weird tests for error and internal
test.eq(T.error:terratype(), T.internal({}):terratype()) -- both {}
-- test known failures for these
test.fail(function()
  T.error:basetype()
end, 'basetype%(%) is not implemented')
test.fail(function()
  T.internal(23):basetype()
end, 'basetype%(%) is not implemented')

-- bad tensor construction
test.fail(function()
  T.tensor(23)
end, 'invalid %(first%) type argument to tensortype constructor')
test.fail(function()
  T.tensor(T.int32, -3)
end, 'invalid dimension for tensortype')
test.fail(function()
  T.tensor(T.int32, 0)
end, 'invalid dimension for tensortype')


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

local idmat = {{1,0},{0,1}}
test.rec_aeq(T.terratoluaval(T.luatoterraval(idmat, T.mat2f), T.mat2f), idmat)
test.rec_aeq(T.terratoluaval(T.luatoterraval(idmat, T.mat2d), T.mat2d), idmat)

test.eq(T.checkluaval(idmat, T.mat2f), true)
test.eq(T.checkluaval(idmat, T.mat2d), true)
test.eq(T.checkluaval(idmat, T.mat3f), false)
test.eq(T.checkluaval(idmat, T.vec4f), false)

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









