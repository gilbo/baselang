local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'

------------------------------------------------------------------------------

-- simple call with simple return value

local typel ret0()
  return 0
end

local typel ret1using0()
  return ret0() + 1
end

test.eq(Lib.arrow({}, Lib.int32), ret0:gettype())
test.eq(Lib.arrow({}, Lib.int32), ret1using0:gettype())
test.eq( ret1using0(), 1)
test.eq( ret0(), 0)
test.eq( ret1using0(), 1)

------------------------------------------------------------------------------

local typel reti()
  return 10,10
end

local typel castret()
  var x : Lib.double
  var y : Lib.int32
  x,y = reti()
  return x/3,y/3
end

test.eq(Lib.arrow({}, {Lib.int32, Lib.int32}), reti:gettype())
test.eq(Lib.arrow({},{Lib.double,Lib.int32}), castret:gettype())
test.aeq({castret()}, {10.0/3.0, 3})



