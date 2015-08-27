local T = {}
package.loaded["typelang.src.types"] = T

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

-- lift some of Terra's primitive types
local primitive_names = {
  "int32",
  "uint64",
  "bool",
  "float",
  "double",
}
local primitives = {}
local terra_to_primitive_table = {}
for i,pname in ipairs(primitive_names) do
  local ptype             = NewType('primitive')
  ptype._terra_type       = _G[pname]
  ptype._primitive_name   = pname

  assert(terralib.types.istype(ptype._terra_type))

  primitives[i] = ptype
  terra_to_primitive_table[ptype._terra_type] = ptype

  T[pname] = ptype  -- EXPOSE FROM THE MODULE
end

function T.fromterratype(ttype)
  return terra_to_primitive_table[ttype]
end

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

function Type:isintegral()
  return self:isvalue() and self:terratype():isintegral()
end

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
--[[                        Subtyping and Coercion                         ]]--
-------------------------------------------------------------------------------

-- current primitive type lattice
-- int32 < double
-- float < double
-- int32 < uint64
-- NOTE: it is not generally safe to convert int32 values to floats

-- first populate the table with errors and identity along the diagonal
local primitive_coerce = {}
for i,p in ipairs(primitives) do
  primitive_coerce[p] = {}
  for j,q in ipairs(primitives) do primitive_coerce[p][q] = T.error end
  primitive_coerce[p][p] = p
end
-- then, write in the transitive closure of the primitive relations above
primitive_coerce[T.int32][T.double] = T.double
primitive_coerce[T.float][T.double] = T.double
primitive_coerce[T.int32][T.uint64] = T.uint64

-- Then construct the primitive join table, similarly
local primitive_join = {}
for i,p in ipairs(primitives) do
  primitive_join[p] = {}
  for j,q in ipairs(primitives) do primitive_join[p][q] = T.error end
  primitive_join[p][p] = p
end
-- symmetrizing the previous coercion rules
primitive_join[T.int32 ][T.double]  = T.double
primitive_join[T.double][T.int32 ]  = T.double
primitive_join[T.float ][T.double]  = T.double
primitive_join[T.double][T.float ]  = T.double
primitive_join[T.int32 ][T.uint64]  = T.uint64
primitive_join[T.uint64][T.int32 ]  = T.uint64
-- and fill in the non-trivial observation about int32 and float
primitive_join[T.int32 ][T.float ]  = T.double
primitive_join[T.float ][T.int32 ]  = T.double

-- the partial order defined by coercion
function Type:iscoercableto(target)
  if not istype(target) then return false end

  -- quick exit for equality
  if self == target then return true end
  local source = self

  -- tuples
  if source:istuple() and target:istuple() then
    if #source._types ~= #target._types then return false end
    for i=1,#source._types do
      if not source._types[i]:iscoercableto(target._types[i]) then
        return false end end
    return true
  end

  -- now we should have primitives
  if source:isprimitive() and target:isprimitive() then
    return primitive_coerce[source][target] ~= T.error
  end

  -- catch-all: coercion fails
  return false
end

-- returns error if the coercion is not valid
function Type:coerceto(target)
  if self:iscoercableto(target) then return target
                                else return T.error end
end

function Type:join(rhs)
  if not istype(rhs) then return T.error end
  local lhs = self

  -- quick exit for equality
  if lhs == rhs then return lhs end

  -- tuples
  if lhs:istuple() and rhs:istuple() then
    if #lhs._types ~= #rhs._types then return T.error end

    local jtyps = {}
    for i=1,#lhs._types do
      jtyps[i] = lhs._types[i]:join(rhs._types[i])
      if jtyps[i] == T.error then return T.error end
    end
    return T.tuple(jtyps)
  end

  if lhs:isprimitive() and rhs:isprimitive() then
    return primitive_join[lhs][rhs]
  end

  -- catch-all: failure
  return T.error
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

