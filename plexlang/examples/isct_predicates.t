
import 'plexlang.plexlang'
local PL = require 'plexlang.plexlib'

local cmath = terralib.includecstring [[#include "math.h"]]

local sqrt = PL.extern('sqrt', PL.arrow(PL.double, PL.double), cmath.sqrt)
local abs = PL.extern('ind', PL.arrow(PL.double, PL.double), cmath.fabs)
local sgn = PL.extern('sgn', PL.arrow(PL.double,PL.double),
  terra( x : double )
    if x < 0.0 then return -1.0 else return 1.0 end
  end)
local ind = PL.extern('ind', PL.arrow(PL.bool,PL.double),
  terra( test : bool )
    if test then return 1.0 else return 0.0 end
  end)

-------------------------------------------------------------------------------

local plexl orient2d( p0:PL.vec2d, p1:PL.vec2d, p2:PL.vec2d )
  var e01 = p1 - p0
  var e02 = p2 - p0
  return e01[0]*e02[1] - e01[1]*e02[0]
end

local plexl edge_edge_isct2d(
  e0p0 : PL.vec2d,   e0p1 : PL.vec2d,
  e1p0 : PL.vec2d,   e1p1 : PL.vec2d
)
  var t00 = orient2d(e0p0, e1p0, e1p1)
  var t01 = orient2d(e0p1, e1p0, e1p1)
  var t10 = orient2d(e1p0, e0p0, e0p1)
  var t11 = orient2d(e1p1, e0p0, e0p1)

  -- boolean intersection test
  var e0crosses = t00*t01 <= 0.0
  var e1crosses = t10*t11 <= 0.0
  var isisct    = e0crosses and e1crosses

  -- intersection coordinate computation
  var b00       = abs(t00)
  var b01       = abs(t01)
  var b10       = abs(t10)
  var b11       = abs(t11)
  var quadarea  = b00+b01+b10+b11
  var div       = 1.0/quadarea
  -- this is only valid if there actually is an intersection
  var isctpt    = div * (b00*e0p0 + b01*e0p1 + b10*e1p0 + b11*e1p1)

  -- one obvious limitation of the current Plexlang is that
  -- we can't early exit this function
  return isisct, isctpt
end

-------------------------------------------------------------------------------

local plexl orient3d( p0:PL.vec3d, p1:PL.vec3d, p2:PL.vec3d, p3:PL.vec3d )
  var e01 = p1 - p0
  var e02 = p2 - p0
  var e03 = p3 - p0
  -- now a 3x3 determinant corresponding to dot(cross(e01,e02), e03)
  return   (e01[0]*e02[1] - e01[1]*e02[0])*e03[2]
         + (e01[1]*e02[2] - e01[2]*e02[1])*e03[0]
         + (e01[2]*e02[0] - e01[0]*e02[2])*e03[1]
end

local plexl edge_tri_isct3d(
  ep0 : PL.vec3d,   ep1 : PL.vec3d,
  tp0 : PL.vec3d,   tp1 : PL.vec3d,   tp2 : PL.vec3d
)
  var ve0 = orient3d(tp0, tp1, tp2, ep0)
  var ve1 = orient3d(tp0, tp1, tp2, ep1)

  var vt0 = orient3d(ep0, ep1, tp0, tp1)
  var vt1 = orient3d(ep0, ep1, tp1, tp2)
  var vt2 = orient3d(ep0, ep1, tp2, tp0)

  -- boolean intersection test
  var edge_cross  = ve0*ve1 <= 0.0
  var s0  = sgn(vt0)
  var s1  = sgn(vt1)
  var s2  = sgn(vt2)
  var in_triangle = (s0 == s1) and (s1 == s2)
  var is_isct = edge_cross and in_triangle

  -- intersection coordinate computation
  var b0      = abs(ve0)
  var b1      = abs(ve1)
  var div     = 1.0 / (b0 + b1)
  var param   = (div * b1)
  var isctpt  = (div * b0) * ep0 + param * ep1

  return is_isct, param, isctpt
end

-------------------------------------------------------------------------------

local function near0(val)
  return math.abs(val) < 1e-10
end

print('isct2d tests')
do
  local t0, p0 = edge_edge_isct2d({-1,0},{1,0},  {0,-1},{0,1})
  print(t0, unpack(p0))
  assert(t0 and p0[1] == 0 and p0[2] == 0)

  local t1, p1 = edge_edge_isct2d({2,0},{1,0},  {0,-1},{0,1})
  print(t1, unpack(p1))
  assert(not t1)

  local t2, p2 = edge_edge_isct2d({0.794,0.698},{0.590,0.753},
                                  {0.661,0.339},{0.735,0.426})
  print(t2,unpack(p2))
  assert(not t2)

  local t3, p3 = edge_edge_isct2d({0.794,0.339},{0.590,0.753},
                                  {0.661,0.698},{0.735,0.426})
  print(t3,unpack(p3))
  local d3 = {p3[1] - 0.67488459681313, p3[2] - 0.60891067117335 }
  assert( t3 and near0(d3[1]) and near0(d3[2]) )
end


print('isct3d tests')
do
  local t0, b0, p0 =
    edge_tri_isct3d({0,0,-1},{0,0,1}, {1,-1,0},{0,1,0},{-1,-1,0})
  print(t0, b0, unpack(p0))
  assert(t0 and b0 == 0.5 and p0[1] == 0 and p0[2] == 0 and p0[3] == 0)

  local t1, b1, p1 =
    edge_tri_isct3d({0,0,2},{0,0,1}, {1,-1,0},{0,1,0},{-1,-1,0})
  print(t1, b1, unpack(p1))
  assert(not t1)

  local t2, b2, p2 =
    edge_tri_isct3d({0,0,-1},{0,0,1}, {1,-1,0},{0,-2,0},{-1,-1,0})
  print(t2, b2, unpack(p2))
  assert(not t2)

  local t3, b3, p3 =
    edge_tri_isct3d({0.568, 0.114, 0.054},{0.974, 0.705, 0.175},
      {0.974, 0.276, 0.354},{0.472, 0.679, 0.575},{0.796, 0.451, 0.182})
  print(t3, b3, unpack(p3))
  assert(not t3)

  local t4, b4, p4 =
    edge_tri_isct3d({0.568, 0.276, 0.054},{0.974, 0.679, 0.575},
      {0.974, 0.114, 0.354},{0.472, 0.705, 0.175},{0.796, 0.451, 0.182})
  print(t4, b4, unpack(p4))
  assert(t4 and near0(b4 - 0.62332036897281) and
          near0(p4[1] - 0.82106806980296) and
          near0(p4[2] - 0.52719810869604) and
          near0(p4[3] - 0.37874991223483))
end


