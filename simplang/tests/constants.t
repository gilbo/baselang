local test  = require 'tests.test'

import 'simplang.simplang'

local Lib   = require 'simplang.simplib'

------------------------------------------------------------------------------

local cn = Lib.Constant(Lib.num,1)
local cb = Lib.Constant(Lib.bool,false)
local simpl retcs()
  return cn, cb
end

test.eq(Lib.arrow({},{Lib.num, Lib.bool}),
        retcs:gettype())
test.aeq( {retcs()}, {1,false})
