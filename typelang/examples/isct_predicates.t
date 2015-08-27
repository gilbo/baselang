
import 'typelang.typelang'
local TL = require 'typelang.typelib'

local cmath = terralib.includecstring [[#include "math.h"]]

local sqrt = TL.extern('sqrt', TL.arrow(TL.double, TL.double), cmath.sqrt)
local abs = TL.extern('ind', TL.arrow(TL.double, TL.double), cmath.fabs)
local sgn = TL.extern('sgn', TL.arrow(TL.double,TL.double),
  terra( x : double )
    if x < 0.0 then return -1.0 else return 1.0 end
  end)
local ind = TL.extern('ind', TL.arrow(TL.bool,TL.double),
  terra( test : bool )
    if test then return 1.0 else return 0.0 end
  end)

-------------------------------------------------------------------------------

local double = TL.double

-------------------------------------------------------------------------------

local typel orient2d(
  p0x:double, p0y:double, p1x:double, p1y:double, p2x:double, p2y:double
)
  var e01x = p1x - p0x
  var e01y = p1y - p0y
  var e02x = p2x - p0x
  var e02y = p2y - p0y
  return e01x*e02y - e01y*e02x
end

local typel edge_edge_isct2d(
  e0p0x : double,   e0p0y : double,   e0p1x : double,   e0p1y : double,
  e1p0x : double,   e1p0y : double,   e1p1x : double,   e1p1y : double
)
  var t00 = orient2d(e0p0x, e0p0y, e1p0x, e1p0y, e1p1x, e1p1y)
  var t01 = orient2d(e0p1x, e0p1y, e1p0x, e1p0y, e1p1x, e1p1y)
  var t10 = orient2d(e1p0x, e1p0y, e0p0x, e0p0y, e0p1x, e0p1y)
  var t11 = orient2d(e1p1x, e1p1y, e0p0x, e0p0y, e0p1x, e0p1y)

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
  var isctx     = div * (b00*e0p0x + b01*e0p1x + b10*e1p0x + b11*e1p1x)
  var iscty     = div * (b00*e0p0y + b01*e0p1y + b10*e1p0y + b11*e1p1y)

  -- one obvious limitation of the current Typelang is that
  -- we can't early exit this function
  return isisct, isctx, iscty
end

-------------------------------------------------------------------------------

local typel orient3d(
  p0x:double, p0y:double, p0z:double,
  p1x:double, p1y:double, p1z:double,
  p2x:double, p2y:double, p2z:double,
  p3x:double, p3y:double, p3z:double
)
  var e01x = p1x - p0x
  var e02x = p2x - p0x
  var e03x = p3x - p0x
  var e01y = p1y - p0y
  var e02y = p2y - p0y
  var e03y = p3y - p0y
  var e01z = p1z - p0z
  var e02z = p2z - p0z
  var e03z = p3z - p0z
  -- now a 3x3 determinant corresponding to dot(cross(e01,e02), e03)
  return   (e01x*e02y - e01y*e02x)*e03z
         + (e01y*e02z - e01z*e02y)*e03x
         + (e01z*e02x - e01x*e02z)*e03y
end

local typel edge_tri_isct3d(
  ep0x : double,  ep0y : double,  ep0z : double,
  ep1x : double,  ep1y : double,  ep1z : double,
  tp0x : double,  tp0y : double,  tp0z : double,
  tp1x : double,  tp1y : double,  tp1z : double,
  tp2x : double,  tp2y : double,  tp2z : double
)
  var ve0 = orient3d(tp0x, tp0y, tp0z,  tp1x, tp1y, tp1z,
                     tp2x, tp2y, tp2z,  ep0x, ep0y, ep0z)
  var ve1 = orient3d(tp0x, tp0y, tp0z,  tp1x, tp1y, tp1z,
                     tp2x, tp2y, tp2z,  ep1x, ep1y, ep1z)
  var vt0 = orient3d(ep0x, ep0y, ep0z,  ep1x, ep1y, ep1z,
                     tp0x, tp0y, tp0z,  tp1x, tp1y, tp1z)
  var vt1 = orient3d(ep0x, ep0y, ep0z,  ep1x, ep1y, ep1z,
                     tp1x, tp1y, tp1z,  tp2x, tp2y, tp2z)
  var vt2 = orient3d(ep0x, ep0y, ep0z,  ep1x, ep1y, ep1z,
                     tp2x, tp2y, tp2z,  tp0x, tp0y, tp0z)

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
  var isctx   = (div * b0) * ep0x + param * ep1x
  var iscty   = (div * b0) * ep0y + param * ep1y
  var isctz   = (div * b0) * ep0z + param * ep1z

  return is_isct, param, isctx, iscty, isctz
end

-------------------------------------------------------------------------------

local function near0(val)
  return math.abs(val) < 1e-10
end

print('isct2d tests')
do
  local t0, p0x, p0y = edge_edge_isct2d(-1,0, 1,0,  0,-1, 0,1)
  print(t0, p0x, p0y)
  assert(t0 and p0x == 0 and p0y == 0)

  local t1, p1x, p1y = edge_edge_isct2d(2,0, 1,0,  0,-1, 0,1)
  print(t1, p1x, p1y)
  assert(not t1)

  local t2, p2x, p2y = edge_edge_isct2d(0.794,0.698, 0.590,0.753,
                                        0.661,0.339, 0.735,0.426)
  print(t2, p2x, p2y)
  assert(not t2)

  local t3, p3x, p3y = edge_edge_isct2d(0.794,0.339, 0.590,0.753,
                                        0.661,0.698, 0.735,0.426)
  print(t3, p3x, p3y)
  assert( t3 and near0(p3x - 0.67488459681313)
             and near0(p3y - 0.60891067117335) )
end


print('isct3d tests')
do
  local t0, b0, p0x, p0y, p0z =
    edge_tri_isct3d(0,0,-1, 0,0,1,    1,-1,0, 0,1,0, -1,-1,0)
  print(t0, b0, p0x, p0y, p0z)
  assert(t0 and b0 == 0.5 and p0x == 0 and p0y == 0 and p0z == 0)

  local t1, b1, p1x, p1y, p1z =
    edge_tri_isct3d(0,0,2, 0,0,1,   1,-1,0, 0,1,0, -1,-1,0)
  print(t1, b1, p1x, p1y, p1z)
  assert(not t1)

  local t2, b2, p2x, p2y, p2z =
    edge_tri_isct3d(0,0,-1, 0,0,1,   1,-1,0, 0,-2,0, -1,-1,0)
  print(t2, b2, p2x, p2y, p2z)
  assert(not t2)

  local t3, b3, p3x, p3y, p3z =
    edge_tri_isct3d(0.568, 0.114, 0.054,   0.974, 0.705, 0.175,
        0.974, 0.276, 0.354,   0.472, 0.679, 0.575,   0.796, 0.451, 0.182)
  print(t3, b3, p3x, p3y, p3z)
  assert(not t3)

  local t4, b4, p4x, p4y, p4z =
    edge_tri_isct3d(0.568, 0.276, 0.054,   0.974, 0.679, 0.575,
        0.974, 0.114, 0.354,   0.472, 0.705, 0.175,   0.796, 0.451, 0.182)
  print(t4, b4, p4x, p4y, p4z)
  assert(t4 and near0(b4 - 0.62332036897281) and
          near0(p4x - 0.82106806980296) and
          near0(p4y - 0.52719810869604) and
          near0(p4z - 0.37874991223483))
end


