local Lib = {}
package.loaded["plexlang.plexlib"] = Lib

-------------------------------------------------------------------------------
-- Policy:
--    This file is responsible for the Lua <--> Plexlang interface
--  as exposed via the plexlib object.
--    As such, it's generally preferrable to define most functions
--  elsewhere in the compiler and then just export them here. Though that's
--  hardly a strict policy.
--    However, it's definitely a bad idea to define functions in this file
--  that are only for use internally by the compiler.
-------------------------------------------------------------------------------

local T = require 'plexlang.src.types'
local B = require 'plexlang.src.builtins'
local F = require 'plexlang.src.functions'

-------------------------------------------------------------------------------

local function deep_array_copy(tbl)
  if type(tbl) ~= 'table' then return tbl
  else
    local cp = {}
    for i=1,#tbl do cp[i] = deep_array_copy(tbl[i]) end
    return cp
  end
end

local ConstantProto = {}
ConstantProto.__index = ConstantProto

function Lib.isconstant(obj) return getmetatable(obj) == ConstantProto end

function Lib.Constant(typ, val)
  if not T.istype(typ) or not typ:isvalue() then
    error('first argument to Constant must be a value type', 2)
  end
  if not T.checkluaval(val, typ) then
    error('second argument to Constant must be a value of the given type', 2)
  end

  local c = setmetatable({
    _type   = typ,
    _value  = deep_array_copy(val),
  }, ConstantProto)
  return c
end

function ConstantProto:get()
  return deep_array_copy(self._value)
end

function ConstantProto:gettype()
  return self._type
end

-------------------------------------------------------------------------------

-- selectively expose some parts of the type system to the user
for _,name in ipairs({
  "int32",
  "uint64",
  "bool",
  "float",
  "double",

  "tensor",
  "matrix",
  "vector",
  "arrow",

  "istype",
}) do
  Lib[name] = T[name]
end
-- vec and mat shorthands
for _,tchar in ipairs({ 'f', 'd', 'i', 'b' }) do
  for i=2,4 do
    local n = tostring(i)
    Lib['vec'..n..tchar] = T['vec'..n..tchar]
    Lib['mat'..n..tchar] = T['mat'..n..tchar]
    for j=2,4 do
      local m = tostring(j)
      Lib['mat'..n..'x'..m..tchar] = T['mat'..n..'x'..m..tchar]
    end
  end
end

-------------------------------------------------------------------------------

-- selectively expose some of the built-ins
Lib.assert      = B.assert

-- generic way for clients to extend the language with external
-- functions as a kind of custom built-in
Lib.extern      = B.extern

-------------------------------------------------------------------------------

Lib.isfunction      = F.isfunction
Lib.compiletofile   = F.compiletofile




