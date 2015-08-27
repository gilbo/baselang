local test  = require 'tests.test'

import 'plexlang.plexlang'

local Lib   = require 'plexlang.plexlib'

------------------------------------------------------------------------------

local vecconst = Lib.Constant(Lib.vec3d,{1,2,3})
local plexl retvec()
  return vecconst
end

test.eq(Lib.arrow({}, Lib.vec3d), retvec:gettype())
test.aeq( retvec(), {1,2,3})

local matconst = Lib.Constant(Lib.mat3d,{{1,0,0},{0,1,0},{0,0,1}})
local plexl retmatid()
  return matconst
end

test.eq(Lib.arrow({}, Lib.mat3d), retmatid:gettype())
test.rec_aeq( retmatid(), {{1,0,0},{0,1,0},{0,0,1}})

------------------------------------------------------------------------------

local ci = Lib.Constant(Lib.int32,1)
local cf = Lib.Constant(Lib.float,1)
local cd = Lib.Constant(Lib.double,1)
local cb = Lib.Constant(Lib.bool,false)
local plexl retcs()
  return ci, cf, cd, cb
end

test.eq(Lib.arrow({},{Lib.int32, Lib.float, Lib.double, Lib.bool}),
        retcs:gettype())
test.aeq( {retcs()}, {1,1,1,false})
