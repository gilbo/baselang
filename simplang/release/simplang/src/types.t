local T = {}
package.loaded["simplang.src.types"] = T


local function is_pos_int_val(val)
  return  type(val) == 'number' and
          math.floor(val) == val and
          val > 0
end

-------------------------------------------------------------------------------
--[[                            Type Prototype                             ]]--
-------------------------------------------------------------------------------

local Type    = {}
Type.__index  = Type

local function NewType(kindname)
  return setmetatable({ _kind = kindname }, Type)
end

local function istype(obj)
  return getmetatable(obj) == Type
end
T.istype = istype

-------------------------------------------------------------------------------
--[[                   Primitives and Type Constructors                    ]]--
-------------------------------------------------------------------------------

T.num                   = NewType('primitive')
T.num._terra_type       = double
T.num._primitive_name   = 'num'

T.bool                  = NewType('primitive')
T.bool._terra_type      = bool
T.bool._primitive_name  = 'bool'

local primitives = { T.num, T.bool }

-------------------------------------------------------------------------------

-- We also add a few types for internal use in the compiler
-- These types should not be exposed to the programmer
T.error     = NewType('error')

-- The internal type is used to convert arbitrary values into unique types
local internal_cache = {}
function T.internal(obj)
  if internal_cache[obj] then return internal_cache[obj] end

  -- otherwise
  local newtyp = NewType('internal')
  newtyp.value = obj
  internal_cache[obj] = newtyp
  return newtyp
end

-------------------------------------------------------------------------------

local tuple_cache = {}
function T.tuple(typelist)
  if not terralib.israwlist(typelist) then
    error('invalid arg to tuple(), expecting a list', 2)
  end
  local typs = {}
  local hashstr = ''
  for i,typ in ipairs(typelist) do
    if not istype(typ) or not typ:isvalue() then
      error('invalid type at tuple position '..i..': '..
            'expected value type', 2)
    end
    typs[i] = typ
    hashstr = hashstr..tostring(typ)
  end

  -- cache lookup
  local bucket = tuple_cache[hashstr]
  if not bucket then
    bucket = {}
    tuple_cache[hashstr] = bucket
  end
  for i,tuptyp in ipairs(bucket) do
    -- check for structural equality; try to disprove
    local match = true
    if #tuptyp._types ~= #typs then match = false end
    for i=1,#typs do
      if tuptyp._types[i] ~= typs[i] then match = false end
    end
    if match then return tuptyp end
  end

  -- lookup failed, build tuple type
  local tupletype = NewType('tuple')
  tupletype._types = typs
  table.insert(bucket, tupletype)
  return tupletype
end

-- function types
local arrow_cache = {}
function T.arrow(argtypes, rettypes)
  if terralib.israwlist(argtypes) then argtypes = T.tuple(argtypes) end
  if terralib.israwlist(rettypes) then rettypes = T.tuple(rettypes) end
  if not istype(argtypes) or not istype(rettypes) then
    error('invalid arg; expecting 2 types or lists of types', 2)
  end
  if not argtypes:istuple() then argtypes = T.tuple{argtypes} end
  if not rettypes:istuple() then rettypes = T.tuple{rettypes} end

  -- cache lookup
  local subcache = arrow_cache[argtypes]
  if not subcache then
    subcache = {}
    arrow_cache[argtypes] = subcache
  end
  local lookup = subcache[rettypes]
  if lookup then return lookup end

  -- if we made it here then we didn't find what we were looking for
  -- in the bucket, so go ahead and build it
  local arrow     = NewType('arrow')
  arrow._argtype  = argtypes
  arrow._rettype  = rettypes
  subcache[rettypes] = arrow
  return arrow
  -- Terra type? (none)
end

-------------------------------------------------------------------------------
--[[                             Type Methods                              ]]--
-------------------------------------------------------------------------------

-- identify which class the type is in
function Type:isprimitive()
  return self._kind == 'primitive'
end
function Type:iserror()
  return self._kind == 'error'
end
function Type:isinternal()
  return self._kind == 'internal'
end
function Type:istuple()
  return self._kind == 'tuple'
end
function Type:isarrow()
  return self._kind == 'arrow'
end

-- define values in the language's type system
function Type:isvalue()
  return self:isprimitive()
end

-------------------------------------------------------------------------------

function Type:isnumeric()
  return self:isvalue() and self:terratype():isarithmetic()
end

function Type:islogical()
  return self:isvalue() and self:terratype() == bool
end

-------------------------------------------------------------------------------

local struct emptyTerraType {}
function Type:terratype()
  if      self._terra_type  then return self._terra_type
  elseif  self:iserror()    then return emptyTerraType
  elseif  self:isinternal() then return emptyTerraType
  end
  error(':terratype() is not implemented for '..tostring(self), 2)
end

function Type:argtypes()
  if self:isarrow() then
    local args = {}
    for i,atyp in ipairs(self._argtype._types) do args[i] = atyp end
    return args
  else
    error(':argtypes() only implemented for arrow types', 2)
  end
end

function Type:rettypes()
  if self:isarrow() then
    local rets = {}
    for i,rtyp in ipairs(self._rettype._types) do rets[i] = rtyp end
    return rets
  else
    error(':rettypes() only implemented for arrow types', 2)
  end
end

function Type:unpacktuple()
  if self:istuple() then
    return self._types
  else
    error(':unpacktuple() only implemented for tuple types', 2)
  end
end

-------------------------------------------------------------------------------
--[[                          Stringifying Types                           ]]--
-------------------------------------------------------------------------------

function Type:__tostring()
  if     self:isprimitive() then return self._primitive_name
  elseif self:istuple()     then
    local str = '{'
    if #self._types > 0 then
      str = str..tostring(self._types[1]) end
    for i=2,#self._types do
      str = str..','..tostring(self._types[i]) end
    str = str..'}'
    return str
  elseif self:isarrow()     then
    return tostring(self._argtype) .. '->' .. tostring(self._rettype)
  elseif self:isinternal()  then return 'internal('..tostring(self.value)..')'
  elseif self:iserror()     then return 'error'
  end
  error('tostring() was not implemented for this type!', 2)
end

-------------------------------------------------------------------------------
--[[                       Terra <-> Lua Conversions                       ]]--
-------------------------------------------------------------------------------

local function terratoluaval(tval, typ)
  if typ:isprimitive() then
    if typ:isnumeric() then
      return tonumber(tval)
    elseif typ:islogical() then     -- this can be tricky unfortunately
      if type(tval) == 'cdata' then
        return not (tval == 0)
      else
        return tval
      end
    end
  end

  error('INTERNAL: Should not be trying to convert values of type '..
        tostring(typ)..' from Terra to Lua')
end

-- should run this dynamic typechecking on un-safe lua values coming in
local function checkluaval(lval, typ)
  if typ:isprimitive() then
    return (typ:isnumeric() and type(lval) == 'number') or
           (typ:islogical() and type(lval) == 'boolean')
  end

  return false
end

-- not typechecked; see above
local function luatoterraval(lval, typ)
  if typ:isprimitive() then
    return lval -- luajit/terra will handle correctly
  end
end

T.checkluaval   = checkluaval
T.luatoterraval = luatoterraval
T.terratoluaval = terratoluaval

