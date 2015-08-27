local test  = require 'tests.test'

import 'simplang.simplang'

local Lib   = require 'simplang.simplib'

------------------------------------------------------------------------------

local simpl arith()
  var x = 42.0
  var y : Lib.num = x
  Lib.assert(x-y == 0)
  Lib.assert(x+y == 84)

  x = 3.0
  y = 5.0
  Lib.assert(x*y == 15)
  Lib.assert(x/y == 3.0/5.0)
  Lib.assert(y/x == 5.0/3.0)

  Lib.assert((-x) * (-y) == x * y)
  Lib.assert(x <= y)
  Lib.assert(x < y)
  Lib.assert(y > x)
  Lib.assert(y >= x)
  Lib.assert(x ~= y)
end

arith()

local simpl booltests()
  var x = true
  var y : Lib.bool = x
  Lib.assert(x and y == true)
  Lib.assert(x or y == true)
  Lib.assert(x == y)

  x = true
  y = false
  Lib.assert( (not x and not y) == not (x or y) )
  Lib.assert(x ~= y)
end

booltests()

------------------------------------------------------------------------------

------------------------------------------------------------------------------
