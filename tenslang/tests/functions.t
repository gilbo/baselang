local test  = require 'tests.test'

import 'tenslang.tenslang'

local Lib   = require 'tenslang.tenslib'

------------------------------------------------------------------------------

-- simple call with simple return value

local tensl ret0()
  return 0
end

local tensl ret1using0()
  return ret0() + 1
end

test.eq(Lib.arrow({}, Lib.num), ret0:gettype())
test.eq(Lib.arrow({}, Lib.num), ret1using0:gettype())
test.eq( ret1using0(), 1)
test.eq( ret0(), 0)
test.eq( ret1using0(), 1)

------------------------------------------------------------------------------

local tensl transpose2d( m : Lib.mat2 )
  return { { m[0,0], m[1,0] },
           { m[0,1], m[1,1] } }
end

local tensl testtranspose()
  Lib.assert( transpose2d({{1,2},{3,4}}) == {{1,3},{2,4}} )
end

test.eq(Lib.arrow(Lib.mat2, Lib.mat2), transpose2d:gettype())
test.eq(Lib.arrow({},{}), testtranspose:gettype())
testtranspose()

local tensl retpair()
  return 1.2, 3.4
end

local tensl pair_to_vec()
  var x : Lib.vec2
  x[0], x[1] = retpair()
  return x
end

test.eq(Lib.arrow({}, {Lib.num, Lib.num}), retpair:gettype())
test.eq(Lib.arrow({},Lib.vec2), pair_to_vec:gettype())
test.aeq(pair_to_vec(), {1.2, 3.4})

------------------------------------------------------------------------------

local tensl reti()
  return 10,10
end

local tensl castret()
  var x : Lib.num
  var y : Lib.num
  x,y = reti()
  return x/3,y/3
end

test.eq(Lib.arrow({}, {Lib.num, Lib.num}), reti:gettype())
test.eq(Lib.arrow({},{Lib.num,Lib.num}), castret:gettype())
test.aeq({castret()}, {10.0/3.0, 10.0/3.0})



