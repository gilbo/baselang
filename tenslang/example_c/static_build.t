
import 'tenslang.tenslang'

local TL = require 'tenslang.tenslib'

local round = TL.extern('round',TL.arrow(TL.num,TL.num),
  terra(x : double) : double
    return double(int(x))
  end)

local tensl getanswer()
  return 21 + 21
end

local tensl signedfrac( x : TL.num )
  return x - round(x)
end

local tensl len2( x : TL.vec3 )
  return +[i] x[i]*x[i]
end

local tensl diag3( x : TL.num )
  return {{x,0,0},{0,x,0},{0,0,x}}
end

-- here, we save out the functions
TL.compiletofile("static_func.o","static_func.h",{
  getanswer   = getanswer,
  signedfrac  = signedfrac,
  len2        = len2,
  diag3       = diag3,
})

