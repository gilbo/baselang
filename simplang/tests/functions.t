local test  = require 'tests.test'

import 'simplang.simplang'

local Lib   = require 'simplang.simplib'

------------------------------------------------------------------------------

-- simple call with simple return value

local simpl ret0()
  return 0
end

local simpl ret1using0()
  return ret0() + 1
end

test.eq(Lib.arrow({}, Lib.num), ret0:gettype())
test.eq(Lib.arrow({}, Lib.num), ret1using0:gettype())
test.eq( ret1using0(), 1)
test.eq( ret0(), 0)
test.eq( ret1using0(), 1)

------------------------------------------------------------------------------

local simpl reti()
  return 10,10
end

local simpl castret()
  var x : Lib.num
  var y : Lib.num
  x,y = reti()
  return x/3,y/3
end

test.eq(Lib.arrow({}, {Lib.num, Lib.num}), reti:gettype())
test.eq(Lib.arrow({},{Lib.num,Lib.num}), castret:gettype())
test.aeq({castret()}, {10.0/3.0, 10.0/3.0})



