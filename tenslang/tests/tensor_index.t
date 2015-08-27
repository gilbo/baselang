local test  = require 'tests.test'

import 'tenslang.tenslang'

local Lib   = require 'tenslang.tenslib'

------------------------------------------------------------------------------

local idmat3 = Lib.Constant(Lib.mat3, {{1,0,0},{0,1,0},{0,0,1}})
local idmat2 = Lib.Constant(Lib.mat2, {{1,0},{0,1}})

local tensl tarith()
  var x = {1.0,2.0,3.0}
  var y = {0.2,0.4,0.6}

  -- scaling
  var s1 = :[i] 0.2*x[i]
  Lib.assert(s1 == 0.2*x)

  -- vec vec dot
  var dot = +[i] x[i] * y[i]
  Lib.assert(dot == 0.2 + 0.8 + 1.8)

  -- mat vec mult
  var multx = :[i] +[j] idmat3[i,j] * x[j]
  var multy = :[i] +[j] idmat3[i,j] * y[j]
  Lib.assert(multx == x)
  Lib.assert(multy == y)

  var M = {{1,2,3},{4,5,6}}
  -- transpose
  var Mt = :[i,j] M[j,i]
  Lib.assert(Mt == {{1,4},{2,5},{3,6}})

  -- inner product
  var p = {1,2}
  var inner = +[i,j] p[i] * M[i,j] * x[j]
  var innerT = +[i,j] x[i] * Mt[i,j] * p[j]
  Lib.assert(inner == innerT)
  Lib.assert(inner == 78)

  -- triple product? (can't do without shuffle)

  -- outer product
  var b1 = { 1,1}
  var b2 = {-1,1}
  var prod1 = (:[i,j] b1[i]*b1[j]) + (:[i,j] b2[i]*b2[j])
  var prod2 = :[i,j] b1[i]*b1[j] + b2[i]*b2[j]
  Lib.assert(prod1 == prod2)
  Lib.assert(prod2 == 2.0*idmat2)

  -- mat mat mult
  var mmult = :[i,j] +[k] idmat2[i,k] * M[k,j]
  Lib.assert(mmult == M)

  -- trace
  var outerX = :[i,j] x[i]*x[j]
  var traceX = +[i] outerX[i,i]
  var normX  = +[i] x[i]*x[i]
  Lib.assert(traceX == normX)

  var A = :[i,j] +[k] Mt[i,k] * M[k,j]

  -- matrix 2-norm
  var frobenius = +[i,j] A[i,j]*A[i,j]

  -- collapse columns
  var v = +[i] :[j] M[j,i]
  Lib.assert(v == {6,15})

  -- replicate matrix in a funny way
  var R = :[i] :[j] A[i,j]
  Lib.assert(R == A)
end

tarith()

local tensl indexintoexpr()
  var x = {1.0,2.0,3.0}
  var y = {0.2,0.4,0.6}

  return (x + 5*y)[1]
end

test.eq(indexintoexpr(), 4)


