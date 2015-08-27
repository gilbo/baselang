
import 'tenslang.tenslang'
local TL = require 'tenslang.tenslib'

local cmath = terralib.includecstring [[#include "math.h"]]
local sqrt  = TL.extern('sqrt', TL.arrow(TL.num, TL.num), cmath.sqrt)

local maximpl = rawget(cmath,'fmax') or terra( a:double, b:double)
  if a > b then return a else return b end
end
local max   = TL.extern('max', TL.arrow({TL.num,TL.num},TL.num),
                        maximpl)

local num   = TL.num
local vec2  = TL.vec2

-------------------------------------------------------------------------------

-- helper functions
local tensl len2d( x : vec2 ) return sqrt(+[i] x[i]*x[i]) end

local EPS = 1e-5

-------------------------------------------------------------------------------

-- example geometric constraints

local tensl bzSample(
  t:num, -- this is constant
  p0:vec2, p1:vec2, p2:vec2, p3:vec2,
  samp:vec2
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
local tensl bisector( p0:vec2, p1:vec2, mid:vec2 )
  var ebase = p1-p0
  var e0    = p0-mid
  var e1    = p1-mid

  var projdiff = (+[i] ebase[i] * e0[i]) + (+[i] ebase[i] * e1[i])
  var lenbase  = max(len2d(ebase), EPS) -- limit divisor

  return projdiff / lenbase
end

local tensl colinear( p0:vec2, p1:vec2, p2:vec2 )
  var e01 = p1-p0
  var e02 = p2-p0
  return e01[0] * e02[1] - e01[1] * e02[0]
end

local tensl triheight( base0:vec2, base1:vec2, apex:vec2, h:num )
  var b     = base1-base0
  var e0    = apex-base0
  var e1    = apex-base1

  var eavg  = e0 + e1
  var bunit = (1.0/max(len2d(b), EPS)) * b

  var det   = bunit[0]*eavg[1] - bunit[1]*eavg[0]
  var diff  = det - h
end

local tensl equalpt( p0:vec2, p1:vec2 )
  return p0-p1
end

local tensl equalnum( a:num, b:num )
  return a-b
end

local tensl oncircle( pt:vec2, center:vec2, radius:num )
  var c_pt  = pt-center
  var ptlen = max(len2d(c_pt), EPS)
  return ptlen - radius
end





