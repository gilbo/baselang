local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'

------------------------------------------------------------------------------

local ci = Lib.Constant(Lib.int32,1)
local cf = Lib.Constant(Lib.float,1)
local cd = Lib.Constant(Lib.double,1)
local cb = Lib.Constant(Lib.bool,false)
local typel retcs()
  return ci, cf, cd, cb
end

test.eq(Lib.arrow({},{Lib.int32, Lib.float, Lib.double, Lib.bool}),
        retcs:gettype())
test.aeq( {retcs()}, {1,1,1,false})
