local B = {}
package.loaded["tenslang.src.builtins"] = B

local A = require 'tenslang.src.ast'
local T = require 'tenslang.src.types'

local LangLib = require 'tenslang.tenslib'

local C = require 'tenslang.src.c'

-------------------------------------------------------------------------------
--[[                           Builtin Prototype                           ]]--
-------------------------------------------------------------------------------

local Builtin = {}
Builtin.__index = Builtin

local function isbuiltin(obj) return getmetatable(obj) == Builtin end
B.isbuiltin = isbuiltin

-- only define builtins in this file
local function NewBuiltin(name, typ, genfunc)
  assert(type(name) == 'string')
  assert(T.istype(typ) and typ:isarrow())

  local bi = setmetatable({
    name     = name,
    type     = typ,
    genfunc  = genfunc,
  }, Builtin)

  return bi
end

-------------------------------------------------------------------------------
--[[               Generic Built-in mechanism for Terra code               ]]--
-------------------------------------------------------------------------------

function B.extern(name, typesig, func)
  return NewBuiltin(name, typesig, function(callast, ...)
    local args = {...}
    return `func( [args] )
  end)
end

-------------------------------------------------------------------------------
--[[                           Specific Builtins                           ]]--
-------------------------------------------------------------------------------

local function terror_body (file, line)
  local prelude = file..':'..tostring(line)..': Assert Failed\n'
  return quote do
    C.fprintf(C.stderr, prelude)
    C.exit(1)
  end end
end

B.assert = NewBuiltin('assert', T.arrow(T.bool,{}),
  function(callast, test)
    return quote
      if not test then
        [terror_body(callast.filename, callast.linenumber)]
      end
    end
  end)

-- type could be difficult here?
--B.print = NewBuiltin('print', T.arrow())













