
import 'plexlang.plexlang'
local PL = require 'plexlang.plexlib'

local cmath = terralib.includecstring [[#include "math.h"]]
local sqrt  = PL.extern('sqrt', PL.arrow(PL.double, PL.double), cmath.sqrt)

local maximpl = rawget(cmath,'fmax') or terra( a:double, b:double)
  if a > b then return a else return b end
end
local max   = PL.extern('max', PL.arrow({PL.double,PL.double},PL.double),
                        maximpl)

local double  = PL.double
local vec2d   = PL.vec2d

-------------------------------------------------------------------------------

-- helper functions
local plexl len2d( x : vec2d ) return sqrt(+[i] x[i]*x[i]) end

local EPS = 1e-5

-------------------------------------------------------------------------------

-- example geometric constraints

local plexl bzSample(
  t:double, -- this is constant
  p0:vec2d, p1:vec2d, p2:vec2d, p3:vec2d,
  samp:vec2d
)
  -- These can be cached because they're a function of a constant
  var p   = {p0,p1,p2,p3}
  var t1  = 1.0-t
  var c   = { t1*t1*t1, 3*t1*t1*t, 3*t1*t*t, t*t*t }

  var interp = +[i] :[j] c[i]*p[i,j]
  var diff   = interp - samp

  return diff
end

-- mid must lie on the bisector line
local plexl bisector( p0:vec2d, p1:vec2d, mid:vec2d )
  var ebase = p1-p0
  var e0    = p0-mid
  var e1    = p1-mid

  var projdiff = (+[i] ebase[i] * e0[i]) + (+[i] ebase[i] * e1[i])
  var lenbase  = max(len2d(ebase), EPS) -- limit divisor

  return projdiff / lenbase
end

local plexl colinear( p0:vec2d, p1:vec2d, p2:vec2d )
  var e01 = p1-p0
  var e02 = p2-p0
  return e01[0] * e02[1] - e01[1] * e02[0]
end

local plexl triheight( base0:vec2d, base1:vec2d, apex:vec2d, h:double )
  var b     = base1-base0
  var e0    = apex-base0
  var e1    = apex-base1

  var eavg  = e0 + e1
  var bunit = (1.0/max(len2d(b), EPS)) * b

  var det   = bunit[0]*eavg[1] - bunit[1]*eavg[0]
  var diff  = det - h
end

local plexl equalpt( p0:vec2d, p1:vec2d )
  return p0-p1
end

local plexl equalnum( a:double, b:double )
  return a-b
end

local plexl oncircle( pt:vec2d, center:vec2d, radius:double )
  var c_pt  = pt-center
  var ptlen = max(len2d(c_pt), EPS)
  return ptlen - radius
end





