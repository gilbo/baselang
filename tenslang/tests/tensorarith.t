local test  = require 'tests.test'

import 'tenslang.tenslang'

local Lib   = require 'tenslang.tenslib'

------------------------------------------------------------------------------

local tensl darith()
  var id = {{1.0,0},{0,1}}
  Lib.assert(1.5*id == {{1.5,0},{0,1.5}})

  var ones = {{1,1},{1,1}}
  Lib.assert( id + ones == {{2,1},{1,2}} )

  Lib.assert(id/2 == {{0.5,0},{0,0.5}})
  Lib.assert(-id ~= id)
  Lib.assert(-id == {{-1,0},{0,-1}})

  var abool = {true,true,false,false}
  var bbool = {true,false,true,false}
  Lib.assert( (not abool and not bbool) == not(abool or bbool) )
  Lib.assert( not not abool == abool)
  Lib.assert( (abool and bbool) == {true, false, false, false})
  Lib.assert( (abool or bbool) == {true, true, true, false})
end

darith()
