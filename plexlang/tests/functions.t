local test  = require 'tests.test'

import 'plexlang.plexlang'

local Lib   = require 'plexlang.plexlib'

------------------------------------------------------------------------------

-- simple call with simple return value

local plexl ret0()
  return 0
end

local plexl ret1using0()
  return ret0() + 1
end

test.eq(Lib.arrow({}, Lib.int32), ret0:gettype())
test.eq(Lib.arrow({}, Lib.int32), ret1using0:gettype())
test.eq( ret1using0(), 1)
test.eq( ret0(), 0)
test.eq( ret1using0(), 1)

------------------------------------------------------------------------------

local plexl transpose2d( m : Lib.mat2d )
  return { { m[0,0], m[1,0] },
           { m[0,1], m[1,1] } }
end

local plexl testtranspose()
  Lib.assert( transpose2d({{1,2},{3,4}}) == {{1,3},{2,4}} )
end

test.eq(Lib.arrow(Lib.mat2d, Lib.mat2d), transpose2d:gettype())
test.eq(Lib.arrow({},{}), testtranspose:gettype())
testtranspose()

local plexl retpair()
  return 1.2, 3.4
end

local plexl pair_to_vec()
  var x : Lib.vec2d
  x[0], x[1] = retpair()
  return x
end

test.eq(Lib.arrow({}, {Lib.double, Lib.double}), retpair:gettype())
test.eq(Lib.arrow({},Lib.vec2d), pair_to_vec:gettype())
test.aeq(pair_to_vec(), {1.2, 3.4})

------------------------------------------------------------------------------

local plexl reti()
  return 10,10
end

local plexl castret()
  var x : Lib.double
  var y : Lib.int32
  x,y = reti()
  return x/3,y/3
end

test.eq(Lib.arrow({}, {Lib.int32, Lib.int32}), reti:gettype())
test.eq(Lib.arrow({},{Lib.double,Lib.int32}), castret:gettype())
test.aeq({castret()}, {10.0/3.0, 3})



