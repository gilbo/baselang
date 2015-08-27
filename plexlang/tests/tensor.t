local test  = require 'tests.test'

import 'plexlang.plexlang'

local Lib   = require 'plexlang.plexlib'

------------------------------------------------------------------------------

local plexl retvec()
  return { 1.0,2.0,3.0,4.0 }
end

test.eq(Lib.arrow({}, Lib.vec4d), retvec:gettype())
test.aeq( retvec(), {1,2,3,4})


local plexl retid3()
  return { {1,0,0}, {0,1,0}, {0,0,1} }
end

local plexl retid3d()
  return Lib.mat3d({ {1,0,0}, {0,1,0}, {0,0,1} })
end

test.eq(Lib.arrow({}, Lib.matrix(Lib.int32, 3,3)), retid3:gettype())
test.eq(Lib.arrow({}, Lib.mat3d), retid3d:gettype())

test.rec_aeq( retid3(), {{1,0,0},{0,1,0},{0,0,1}})
test.rec_aeq( retid3d(), {{1,0,0},{0,1,0},{0,0,1}})

------------------------------------------------------------------------------

------------------------------------------------------------------------------

local plexl getmat_entry( m : Lib.mat2d, i:Lib.int32, j:Lib.int32 )
  return m[i,j]
end

test.eq(Lib.arrow({Lib.mat2d, Lib.int32, Lib.int32},Lib.double),
                  getmat_entry:gettype())
test.eq(getmat_entry({{1,2},{3,4}}, 0, 0), 1)
test.eq(getmat_entry({{1,2},{3,4}}, 0, 1), 2)
test.eq(getmat_entry({{1,2},{3,4}}, 1, 0), 3)
test.eq(getmat_entry({{1,2},{3,4}}, 1, 1), 4)

local plexl slicecol( m : Lib.mat2d, i:Lib.int32 )
  return { m[0,i], m[1,i] }
end

test.eq(Lib.arrow({Lib.mat2d, Lib.int32},Lib.vec2d), slicecol:gettype())
test.aeq(slicecol({{1,2},{3,4}}, 0), {1,3})
test.aeq(slicecol({{1,2},{3,4}}, 1), {2,4})


------------------------------------------------------------------------------

------------------------------------------------------------------------------
