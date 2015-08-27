local test  = require 'tests.test'

import 'tenslang.tenslang'

local Lib   = require 'tenslang.tenslib'

------------------------------------------------------------------------------

local vecconst = Lib.Constant(Lib.vec3,{1,2,3})
local tensl retvec()
  return vecconst
end

test.eq(Lib.arrow({}, Lib.vec3), retvec:gettype())
test.aeq( retvec(), {1,2,3})

local matconst = Lib.Constant(Lib.mat3,{{1,0,0},{0,1,0},{0,0,1}})
local tensl retmatid()
  return matconst
end

test.eq(Lib.arrow({}, Lib.mat3), retmatid:gettype())
test.rec_aeq( retmatid(), {{1,0,0},{0,1,0},{0,0,1}})

------------------------------------------------------------------------------

local cn = Lib.Constant(Lib.num,1)
local cb = Lib.Constant(Lib.bool,false)
local tensl retcs()
  return cn, cb
end

test.eq(Lib.arrow({},{Lib.num, Lib.bool}),
        retcs:gettype())
test.aeq( {retcs()}, {1,false})
