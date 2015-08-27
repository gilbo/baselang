
import 'typelang.typelang'

local TL = require 'typelang.typelib'

local typel getanswer()
  return 21 + 21
end

local typel signedfrac( x : TL.double )
  return x - TL.double(TL.int32(x))
end

-- here, we save out the functions
TL.compiletofile("static_func.o","static_func.h",{
  getanswer   = getanswer,
  signedfrac  = signedfrac,
})

