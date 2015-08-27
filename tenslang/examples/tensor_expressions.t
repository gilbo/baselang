
import 'tenslang.tenslang'
local TL = require 'tenslang.tenslib'

local cmath = terralib.includecstring [[#include "math.h"]]

local sqrt = TL.extern('sqrt', TL.arrow(TL.num, TL.num), cmath.sqrt)

-------------------------------------------------------------------------------

local tensl dot2d( x:TL.vec2, y:TL.vec2 )
  return +[i] x[i] * y[i]
end

local tensl vec2len( x:TL.vec2 )
  return sqrt( +[i] x[i]*x[i] )
end

local tensl matvec3mult( m:TL.mat3, x:TL.vec3 )
  return :[i] +[j] m[i,j] * x[j]
end

local tensl transpose2( m:TL.mat2 )
  return :[i,j] m[j,i]
end

local tensl qform4( A:TL.mat4, x:TL.vec4 )
  return +[i,j] x[i] * A[i,j] * x[j]
end

local tensl innerprod3( x:TL.vec3, A:TL.mat3, y:TL.vec3)
  return +[i,j] x[i] * A[i,j] * y[j]
end

local tensl outerprod3( x:TL.vec3, y:TL.vec3 )
  return :[i,j] x[i] * y[j]
end

local tensl matmat3mult( A:TL.mat3, B:TL.mat3 )
  return :[i,j] +[k] A[i,k] * B[k,j]
end

local tensl trace2( M:TL.mat2 )
  return +[i] M[i,i]
end

local tensl frobenius3( M:TL.mat3 )
  return +[i,j] M[i,j] * M[i,j]
end

-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
