local test  = require 'tests.test'

import 'tenslang.tenslang'

local Lib   = require 'tenslang.tenslib'

------------------------------------------------------------------------------

local tensl retvec()
  return { 1.0,2.0,3.0,4.0 }
end

test.eq(Lib.arrow({}, Lib.vec4), retvec:gettype())
test.aeq( retvec(), {1,2,3,4})


local tensl retid3()
  return { {1,0,0}, {0,1,0}, {0,0,1} }
end

test.eq(Lib.arrow({}, Lib.matrix(Lib.num, 3,3)), retid3:gettype())

test.rec_aeq( retid3(), {{1,0,0},{0,1,0},{0,0,1}})

------------------------------------------------------------------------------

------------------------------------------------------------------------------

local tensl getmat_entry( m : Lib.mat2, i:Lib.num, j:Lib.num )
  return m[i,j]
end

test.eq(Lib.arrow({Lib.mat2, Lib.num, Lib.num},Lib.num),
                  getmat_entry:gettype())
test.eq(getmat_entry({{1,2},{3,4}}, 0, 0), 1)
test.eq(getmat_entry({{1,2},{3,4}}, 0, 1), 2)
test.eq(getmat_entry({{1,2},{3,4}}, 1, 0), 3)
test.eq(getmat_entry({{1,2},{3,4}}, 1, 1), 4)

local tensl slicecol( m : Lib.mat2, i:Lib.num )
  return { m[0,i], m[1,i] }
end

test.eq(Lib.arrow({Lib.mat2, Lib.num},Lib.vec2), slicecol:gettype())
test.aeq(slicecol({{1,2},{3,4}}, 0), {1,3})
test.aeq(slicecol({{1,2},{3,4}}, 1), {2,4})


------------------------------------------------------------------------------

------------------------------------------------------------------------------
