local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'

------------------------------------------------------------------------------

local typel doblock()
  var x = 3
  do
    var x = 4
    x = 5
  end
  return x
end

test.eq(Lib.arrow({},Lib.int32), doblock:gettype())
test.eq( doblock(), 3 )

------------------------------------------------------------------------------

local typel decltest()
  var x : Lib.double
  x = 1
  return x
end

test.eq(Lib.arrow({},Lib.double), decltest:gettype())
test.eq( decltest(), 1 )

------------------------------------------------------------------------------

local typel repeatdecl()
  var x = 1
  var x = 2
  return x
end

test.eq(repeatdecl(), 2)

------------------------------------------------------------------------------

local typel multiassgn()
  var x : Lib.double
  var y : Lib.double
  x,y = 1,2
  return y
end

test.eq(multiassgn(), 2)

------------------------------------------------------------------------------
