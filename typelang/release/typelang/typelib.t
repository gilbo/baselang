local Lib = {}
package.loaded["typelang.typelib"] = Lib

-------------------------------------------------------------------------------
-- Policy:
--    This file is responsible for the Lua <--> Typelang interface
--  as exposed via the typelib object.
--    As such, it's generally preferrable to define most functions
--  elsewhere in the compiler and then just export them here. Though that's
--  hardly a strict policy.
--    However, it's definitely a bad idea to define functions in this file
--  that are only for use internally by the compiler.
-------------------------------------------------------------------------------

local T = require 'typelang.src.types'
local B = require 'typelang.src.builtins'
local F = require 'typelang.src.functions'

-------------------------------------------------------------------------------

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
    _value  = val,
  }, ConstantProto)
  return c
end

function ConstantProto:get()
  return self._value
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

  "arrow",

  "istype",
}) do
  Lib[name] = T[name]
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




