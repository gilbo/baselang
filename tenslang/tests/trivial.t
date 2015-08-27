local test  = require 'tests.test'

import 'tenslang.tenslang'

local Lib   = require 'tenslang.tenslib'

------------------------------------------------------------------------------

local tensl noop() end

test.eq(Lib.arrow({},{}), noop:gettype())
test.eq(nil, noop())

------------------------------------------------------------------------------

local tensl ret0()
  return 0
end

test.eq(Lib.arrow({},Lib.num), ret0:gettype())
test.eq(ret0(), 0)

------------------------------------------------------------------------------

local tensl ret01()
  return 0, 1
end

test.eq(Lib.arrow({},{Lib.num, Lib.num}), ret01:gettype())
test.aeq( {ret01()} , {0,1} )

------------------------------------------------------------------------------





