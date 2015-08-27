local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'

------------------------------------------------------------------------------

local typel noop() end

test.eq(Lib.arrow({},{}), noop:gettype())
test.eq(nil, noop())

------------------------------------------------------------------------------

local typel ret0()
  return 0
end

test.eq(Lib.arrow({},Lib.int32), ret0:gettype())
test.eq(ret0(), 0)

------------------------------------------------------------------------------

local typel ret01()
  return 0, 1
end

test.eq(Lib.arrow({},{Lib.int32, Lib.int32}), ret01:gettype())
test.aeq( {ret01()} , {0,1} )

------------------------------------------------------------------------------





