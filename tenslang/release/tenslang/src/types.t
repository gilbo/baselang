local T = {}
package.loaded["tenslang.src.types"] = T


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

-- Tensor types generalize
-- vectors, matrices, ...
local tensortype_cache = {}
local function tensortype_helper (basetyp, dims, errdepth)
  -- argument promotion
  if type(dims) == 'number' then dims = {dims} end

  -- argument checking
  if not istype(basetyp) or not basetyp:isscalar() then
    error("invalid (first) type argument to tensortype constructor", errdepth)
  end
  for i=1,#dims do
    if not is_pos_int_val(dims[i]) then
      error("invalid dimension for tensortype: dimension "..tostring(i)..
            " was not a positive integer", errdepth)
    end
  end

  -- cache lookup
  local cache = tensortype_cache
  for i=1,#dims do
    if not cache[dims[i]] then cache[dims[i]] = {} end -- populate on demand
    cache = cache[dims[i]]
  end
  if cache[basetyp] then return cache[basetyp] end

  -- otherwise construct the new type
  local tensor        = NewType('tensor')
  tensor.dims         = {}
  tensor.strides      = {} -- this assumes a row-major layout
  tensor._base_type   = basetyp
  local bttype        = basetyp:terratype()
  local tname         = 'tensor_'..tostring(bttype)
  local nentries      = 1
  for i=1,#dims do -- iterate
    tensor.dims[i]    = dims[i]
    tname = tname..'_'..tostring(dims[i])
    -- iterate this part backwards to build strides
    local d = #dims + 1 - i
    tensor.strides[d] = nentries
    nentries = nentries * dims[d]
  end
  tensor._n_entries   = nentries
  tensor._terra_type  = struct { d : bttype[nentries] }
  tensor._terra_type.metamethods.__typename = function(self)
    return tname
  end

  -- store the type in the cache
  cache[basetyp] = tensor
  return tensor
end

function T.tensor(basetyp, ...)
  return tensortype_helper(basetyp, {...}, 3)
end
function T.matrix(basetyp, N, M)
  return tensortype_helper(basetyp, {N,M}, 3)
end
function T.vector(basetyp, N)
  return tensortype_helper(basetyp, {N}, 3)
end

local tensorindex_cache = {}
local tensorindex_nil = NewType('tensorindex')
tensorindex_nil._terra_type = int32
function T.tensorindex(n)
  if n == nil then return tensorindex_nil end
  if not is_pos_int_val(n) then
    error('tensorindex() type constructor '..
          'expects nil or a positive integer', 2)
  end

  local lookup          = tensorindex_cache[n]
  if lookup then return lookup end

  local ti              = NewType('tensorindex')
  ti.range              = n
  ti._terra_type        = tensorindex_nil._terra_type
  tensorindex_cache[n]  = ti
  return ti
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
function Type:istensor()
  return self._kind == 'tensor'
end
function Type:iserror()
  return self._kind == 'error'
end
function Type:isinternal()
  return self._kind == 'internal'
end
function Type:istensorindex()
  return self._kind == 'tensorindex'
end
function Type:istuple()
  return self._kind == 'tuple'
end
function Type:isarrow()
  return self._kind == 'arrow'
end

function Type:isscalar()
  return self:isprimitive() -- given the current types
end
function Type:isvector()
  return self:istensor() and #self.dims == 1
end
function Type:ismatrix()
  return self:istensor() and #self.dims == 2
end

-- define values in the language's type system
function Type:isvalue()
  return self:isprimitive() or
         ( self:istensor() and self:basetype():isprimitive() )
end

-------------------------------------------------------------------------------

function Type:isnumeric()
  return self:isvalue() and self:terrabasetype():isarithmetic()
end

function Type:islogical()
  return self:isvalue() and self:terrabasetype() == bool
end

function Type:isunknowntensorindex()
  return self:istensorindex() and self.range == nil
end

-------------------------------------------------------------------------------

function Type:basetype()
  if self:istensor() then return self._base_type end
  if self:isscalar() then return self end
  error(':basetype() is not implemented for '..tostring(self), 2)
end

local struct emptyTerraType {}
function Type:terratype()
  if      self._terra_type  then return self._terra_type
  elseif  self:iserror()    then return emptyTerraType
  elseif  self:isinternal() then return emptyTerraType
  end
  error(':terratype() is not implemented for '..tostring(self), 2)
end

function Type:terrabasetype()
  return self:basetype():terratype()
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
  elseif self:istensor()    then
    local bstr = tostring(self._base_type)
    if #self.dims == 1 then
      return 'vector('..bstr..','..tostring(self.dims[1])..')'
    elseif #self.dims == 2 then
      return 'matrix('..bstr..','..tostring(self.dims[1])..
                              ','..tostring(self.dims[2])..')'
    else
      local str = 'tensor('..bstr
      for i=1,#self.dims do str = str .. ',' .. tostring(self.dims[i]) end
      return str .. ')'
    end
  elseif self:istensorindex() then
    if self.range then return 'tensorindex('..tostring(self.range)..')'
                  else return 'tensorindex(?)' end
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
--[[                         Structural Comparison                         ]]--
-------------------------------------------------------------------------------

-- check if two types are tensors with matching dimensions
function Type:dimsmatch(rhs)
  local ltensor = self:istensor()
  local rtensor = rhs:istensor()
  if not ltensor and not rtensor then return true end
  if ltensor ~= rtensor then return false end
  if #self.dims ~= #rhs.dims then return false end
  for i = 1, #self.dims do
    if self.dims[i] ~= rhs.dims[i] then return false end
  end
  return true
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

  elseif typ:istensor() then
    local basetyp = typ:basetype()
    local dims    = typ.dims
    local strides = typ.strides
    -- recursive conversion of tensors
    local function tensorconvert(d, offset)
      -- base case
      if d > #dims then
        return terratoluaval(tval.d[offset], basetyp)
      -- recursive
      else
        local arr = {}
        for k=1,dims[d] do
          arr[k] = tensorconvert(d+1, offset)
          offset = offset + strides[d]
        end
        return arr
      end
    end
    return tensorconvert(1,0)
  end

  error('INTERNAL: Should not be trying to convert values of type '..
        tostring(typ)..' from Terra to Lua')
end

-- should run this dynamic typechecking on un-safe lua values coming in
local function checkluaval(lval, typ)
  if typ:isprimitive() then
    return (typ:isnumeric() and type(lval) == 'number') or
           (typ:islogical() and type(lval) == 'boolean')

  elseif typ:istensor() then
    local basetyp = typ:basetype()
    local dims    = typ.dims
    local function checktensor(subval, d)
      if d > #dims then
        return checkluaval(subval, basetyp)
      else
        if type(subval) ~= 'table' or #subval ~= dims[d] then
          return false end
        for k=1,dims[d] do
          if not checktensor(subval[k], d+1) then return false end
        end
        return true
      end
    end
    return checktensor(lval,1)

  end

  return false
end

-- not typechecked; see above
local function luatoterraval(lval, typ)
  if typ:isprimitive() then
    return lval -- luajit/terra will handle correctly

  elseif typ:istensor() then
    local tval = terralib.new(typ:terratype())

    local basetyp = typ:basetype()
    local dims    = typ.dims
    local strides = typ.strides
    local function tensorconvert(subval, d, offset)
      if d > #dims then
        -- this is an inlining optimization WARNING only works because of
        -- the other branch for primitives being trivial
        tval.d[offset] = subval
        -- this is the generic form
        --tval.d[offset] = luatoterraval(subval, basetyp)
      else
        for k=1,dims[d] do
          tensorconvert(subval[k], d+1, offset)
          offset = offset + strides[d]
        end
      end
    end
    tensorconvert(lval,1,0)

    return tval
  end
end

T.checkluaval   = checkluaval
T.luatoterraval = luatoterraval
T.terratoluaval = terratoluaval

-------------------------------------------------------------------------------
--[[                             More Aliasing                             ]]--
-------------------------------------------------------------------------------

for n=2,4 do
  -- common vector aliases
  local vecname = 'vec'..tostring(n)
  T[vecname]      = T.vector(T.num,  n)
  T[vecname..'b'] = T.vector(T.bool,   n)

  -- matrix aliases
  for m=2,4 do
    local matname = 'mat'..tostring(n)..'x'..tostring(m)
    T[matname]      = T.matrix(T.num,  n, m)
    T[matname..'b'] = T.matrix(T.bool,   n, m)
  end

  -- square matrix aliases
  local shortname = 'mat'..tostring(n)
  local fullname  = 'mat'..tostring(n)..'x'..tostring(n)
  T[shortname]      = T[fullname]
  T[shortname..'b'] = T[fullname..'b']
end


