
local Exports = {}
package.loaded["plexlang.src.shape"] = Exports


local function hasproto(obj, proto)
  local mt = getmetatable(obj)
  while mt do
    if mt == proto then return true end
    mt = getmetatable(mt)
  end
  return false
end

----------------------------
--[[ Shapes for Objects ]]--
----------------------------

local Shape               = {}
Shape.__index             = Shape

function Shape:match(val) error('unimplemented') end
function Shape:isproto(obj) return hasproto(obj, self) end

local function NewShape(data, proto)
  local newshape = setmetatable(data or {}, proto or Shape)
  newshape.__index = newshape
  return newshape
end


------------------------
--[[ Default Shapes ]]--
------------------------

local Val = {
  ['string'] = NewShape(),
  ['number'] = NewShape(),
  ['bool']   = NewShape(),
}
Exports.string  = Val.string
Exports.number  = Val.number
Exports.bool    = Val.bool

local function islonguint(val)
  return type(val) == 'cdata' and terralib.typeof(val) == uint64
end

function Val.string:match(val) return type(val) == 'string'   end
function Val.number:match(val) return type(val) == 'number'
                                   or islonguint(val)         end
function Val.bool:match(val)   return type(val) == 'boolean'  end



----------------------------
--[[ Shape Constructors ]]--
----------------------------

local compileShape -- forward declaration

local RecordShapeProto  = NewShape()
local function RecordShape(tbl, order)
  -- first, check that everything in the order is in the table
  local orderset  = {}
  local keylist   = {}
  for i,k in ipairs(order) do
    keylist[i]  = k
    orderset[k] = true
    assert(tbl[k], 'key '..tostring(k)..' missing from shape')
  end
  -- then copy keys, get a shape, and check that they're in the order
  local shapetbl = {}
  for k,v in pairs(tbl) do
    assert(type(k) == 'string', 'record shapes expects only string keys')
    assert(orderset[k], 'key '..k..' must be specified in order')
    shapetbl[k] = compileShape(v)
  end
  return NewShape({
    subshapes = shapetbl,
    _keylist  = keylist,
  }, RecordShapeProto)
end
function RecordShapeProto:match(val)
  if type(val) ~= 'table' then return false end
  -- check that declared fields match
  for k,subshape in pairs(self.subshapes) do
    if not subshape:match(val[k]) then return false end
  end
  -- check that there are no undeclared fields
  for k,_ in pairs(val) do
    if not self.subshapes[k] then return false end
  end
  return true
end
function RecordShapeProto:keys()
  local i=0
  local ks = self._keylist
  return function()
    i = i+1
    return ks[i]
  end
end

local ListShapeProto    = NewShape()
local function ListShape(lstbl)
  local subshape = compileShape(lstbl[1])
  return NewShape({ subshape = subshape }, ListShapeProto)
end
function ListShapeProto:match(val)
  if not terralib.israwlist(val) then return false end
  for _,subval in ipairs(val) do
    if not self.subshape:match(subval) then return false end
  end
  return true
end

local MaybeProto = NewShape()
local function Maybe(subshape)
  subshape = compileShape(subshape)
  return NewShape({ subshape = subshape }, MaybeProto)
end
function MaybeProto:match(val)
  return val == nil or self.subshape:match(val)
end



------------------------
--[[ Shape Creation ]]--
------------------------




local SHAPE_REGISTRY = {
  ['string'] = Val.string,
  ['number'] = Val.number,
  ['bool']   = Val.bool,
}
local function registerShape(name, obj)
  assert(Shape:isproto(obj), 'only shapes may be registered')
  assert(type(name) == 'string', 'shapes must be registered with string keys')
  SHAPE_REGISTRY[name] = obj
  return obj
end
compileShape = function(obj, order)
  if Shape:isproto(obj) then
    return obj
  elseif type(obj) == 'string' then
    local lookup = SHAPE_REGISTRY[obj]
    assert(lookup, 'Could not find shape "'..obj..'" in registry')
    return lookup
  elseif type(obj) == 'table' then
    if terralib.israwlist(obj) and #obj == 1 then
      return ListShape(obj)
    else
      return RecordShape(obj, order)
    end
  else
    error(false, 'tried to compile invalid shape')
  end
end

Exports.CompileShape  = compileShape
Exports.RegisterShape = registerShape
Exports.Maybe         = Maybe
Exports.NewShape      = NewShape

function Exports.islistshape(shape)
  return ListShapeProto:isproto(shape)
end
function Exports.isrecordshape(shape)
  return RecordShapeProto:isproto(shape)
end
function Exports.ismaybeshape(shape)
  return MaybeProto:isproto(shape)
end



