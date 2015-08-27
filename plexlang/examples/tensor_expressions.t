
import 'plexlang.plexlang'
local PL = require 'plexlang.plexlib'

local cmath = terralib.includecstring [[#include "math.h"]]

local sqrt = PL.extern('sqrt', PL.arrow(PL.double, PL.double), cmath.sqrt)

-------------------------------------------------------------------------------

local plexl dot2d( x:PL.vec2d, y:PL.vec2d )
  return +[i] x[i] * y[i]
end

local plexl vec2dlen( x:PL.vec2d )
  return sqrt( +[i] x[i]*x[i] )
end

local plexl matvec3dmult( m:PL.mat3d, x:PL.vec3d )
  return :[i] +[j] m[i,j] * x[j]
end

local plexl transpose2f( m:PL.mat2f )
  return :[i,j] m[j,i]
end

local plexl qform4d( A:PL.mat4d, x:PL.vec4d )
  return +[i,j] x[i] * A[i,j] * x[j]
end

local plexl innerprod3d( x:PL.vec3d, A:PL.mat3d, y:PL.vec3d)
  return +[i,j] x[i] * A[i,j] * y[j]
end

local plexl outerprod3i( x:PL.vec3i, y:PL.vec3i )
  return :[i,j] x[i] * y[j]
end

local plexl matmat3dmult( A:PL.mat3d, B:PL.mat3d )
  return :[i,j] +[k] A[i,k] * B[k,j]
end

local plexl trace2f( M:PL.mat2f )
  return +[i] M[i,i]
end

local plexl frobenius3d( M:PL.mat3d )
  return +[i,j] M[i,j] * M[i,j]
end

-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
