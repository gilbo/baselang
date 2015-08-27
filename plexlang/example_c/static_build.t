
import 'plexlang.plexlang'

local PL = require 'plexlang.plexlib'

local plexl getanswer()
  return 21 + 21
end

local plexl signedfrac( x : PL.double )
  return x - PL.double(PL.int32(x))
end

local plexl len2( x : PL.vec3d )
  return +[i] x[i]*x[i]
end

local plexl diag3( x : PL.double )
  return {{x,0,0},{0,x,0},{0,0,x}}
end

-- here, we save out the functions
PL.compiletofile("static_func.o","static_func.h",{
  getanswer   = getanswer,
  signedfrac  = signedfrac,
  len2        = len2,
  diag3       = diag3,
})

