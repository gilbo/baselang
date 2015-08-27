local test  = require 'tests.test'

import 'simplang.simplang'

local Lib   = require 'simplang.simplib'

------------------------------------------------------------------------------

local simpl noop() end

test.eq(Lib.arrow({},{}), noop:gettype())
test.eq(nil, noop())

------------------------------------------------------------------------------

local simpl ret0()
  return 0
end

test.eq(Lib.arrow({},Lib.num), ret0:gettype())
test.eq(ret0(), 0)

------------------------------------------------------------------------------

local simpl ret01()
  return 0, 1
end

test.eq(Lib.arrow({},{Lib.num, Lib.num}), ret01:gettype())
test.aeq( {ret01()} , {0,1} )

------------------------------------------------------------------------------





