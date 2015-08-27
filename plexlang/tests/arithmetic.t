local test  = require 'tests.test'

import 'plexlang.plexlang'

local Lib   = require 'plexlang.plexlib'

------------------------------------------------------------------------------

local plexl int32arith()
  var x = 42
  var y : Lib.int32 = x
  Lib.assert(x-y == 0)
  Lib.assert(x+y == 84)

  x = 3
  y = 5
  Lib.assert(x*y == 15)
  Lib.assert(x/y == 0)
  Lib.assert(y/x == 1)

  Lib.assert((-x) * (-y) == x * y)
  Lib.assert(x <= y)
  Lib.assert(x < y)
  Lib.assert(y > x)
  Lib.assert(y >= x)
  Lib.assert(x ~= y)
end

int32arith()

local plexl uint64arith()
  var x : Lib.uint64 = 42
  var y : Lib.uint64 = x
  Lib.assert(x-y == 0)
  Lib.assert(x+y == 84)

  x = 3
  y = 5
  Lib.assert(x*y == 15)
  Lib.assert(x/y == 0)
  Lib.assert(y/x == 1)

  Lib.assert((-x) * (-y) == x * y)
  Lib.assert(x <= y)
  Lib.assert(x < y)
  Lib.assert(y > x)
  Lib.assert(y >= x)
  Lib.assert(x ~= y)

  Lib.assert(-x > 0) -- weird but true cause unsigned
end

uint64arith()

local plexl floatarith()
  var x = 42.0f
  var y : Lib.float = x
  Lib.assert(x-y == 0f)
  Lib.assert(x+y == 84f)

  x = 3.0f
  y = 5.0f
  Lib.assert(x*y == 15f)
  Lib.assert(x/y == 3.0f/5.0f)
  Lib.assert(y/x == 5.0f/3.0f)

  Lib.assert((-x) * (-y) == x * y)
  Lib.assert(x <= y)
  Lib.assert(x < y)
  Lib.assert(y > x)
  Lib.assert(y >= x)
  Lib.assert(x ~= y)
end

floatarith()

local plexl doublearith()
  var x = 42.0
  var y : Lib.double = x
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

doublearith()

local plexl coercearith()
  var x = 42
  var y : Lib.double = x
  Lib.assert(x-y == 0)
  Lib.assert(x+y == 84)

  x = 3
  y = 5
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

coercearith()

local plexl booltests()
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

--int32arith:printstats()
--uint64arith:printstats()
--floatarith:printstats()
--doublearith:printstats()

------------------------------------------------------------------------------

------------------------------------------------------------------------------
