local test  = require 'tests.test'

import 'simplang.simplang'

local Lib   = require 'simplang.simplib'

------------------------------------------------------------------------------

local simpl doblock()
  var x = 3
  do
    var x = 4
    x = 5
  end
  return x
end

test.eq(Lib.arrow({},Lib.num), doblock:gettype())
test.eq( doblock(), 3 )

------------------------------------------------------------------------------

local simpl decltest()
  var x : Lib.num
  x = 1
  return x
end

test.eq(Lib.arrow({},Lib.num), decltest:gettype())
test.eq( decltest(), 1 )

------------------------------------------------------------------------------

local simpl repeatdecl()
  var x = 1
  var x = 2
  return x
end

test.eq(repeatdecl(), 2)

------------------------------------------------------------------------------

local simpl multiassgn()
  var x : Lib.num
  var y : Lib.num
  x,y = 1,2
  return y
end

test.eq(multiassgn(), 2)

------------------------------------------------------------------------------
