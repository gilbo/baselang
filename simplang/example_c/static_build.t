
import 'simplang.simplang'

local SL = require 'simplang.simplib'

local round = SL.extern('round',SL.arrow(SL.num,SL.num),
  terra(x : double) : double
    return double(int(x))
  end)

local simpl getanswer()
  return 21 + 21
end

local simpl signedfrac( x : SL.num )
  return x - round(x)
end

-- here, we save out the functions
SL.compiletofile("static_func.o","static_func.h",{
  getanswer   = getanswer,
  signedfrac  = signedfrac,
})

