local A = {}
package.loaded["simplang.src.ast"] = A


-------------------------------------------------------------------------------
--  AST definition
--
--      This file has a number of parts, some of which implement some
--    meta-programming that (unsurprisingly) can take a while to grok.
--    If you're opening up this file for the first time, start at the
--    bottom.  It defines all of the different kinds of AST nodes used
--    everywhere else in the compiler.  Odds are that's all you need
--    from this file on a first read-through.
--
--      If you want to better understand the mechanisms here, then
--    there are two major non-obvious parts.  The first is "Shapes"
--    which are used b/c Lua is dynamically typed.  They provide meta-data
--    about what fields we expect to find in a given AST.  Unfortunately,
--    the shape system doesn't have a clear design goal right now.  It
--    might help catch some bugs, but it doesn't give very clear guarantees
--    about how it should be used and what is allowed/disallowed.
--      The second major non-obvious part is using Lua to meta-program
--    Lua code itself.  NewCopyPass() and NewInertPass() use strings
--    to construct specialized Lua functions for a given compiler pass.
--    (e.g. specialization, typechecking, codegeneration)  The application
--    of this pattern can be seen in other files.  In particular, note
--    that some of the lighter-weight passes like specialization or
--    alpha-renaming (hidden in the typechecker) can be defined very
--    concisely in this way, since most of the kinds of AST node have
--    trivial, default implementations; which are supplied by the
--    pass-generation functions.
--
-------------------------------------------------------------------------------

local T = require 'simplang.src.types'

local function hasproto(obj, proto)
  local mt = getmetatable(obj)
  while mt do
    if mt == proto then return true end
    mt = getmetatable(mt)
  end
  return false
end

local Shapes = require 'simplang.src.shape'
local Maybe  = Shapes.Maybe

-- Register a primitive shape for terra type objects
local TerraTypeShape = Shapes.NewShape()
function TerraTypeShape:match(val)
  return terralib.types.istype(val)
end
Shapes.RegisterShape('ttype', TerraTypeShape)

-- another for native type objects
local TypeShape = Shapes.NewShape()
function TypeShape:match(val)
  return T.istype(val)
end
Shapes.RegisterShape('type', TypeShape)

-- another for generic lua tables in case we don't really want to fuss
local TableShape = Shapes.NewShape()
function TableShape:match(val)
  return type(val) == 'table'
end
Shapes.RegisterShape('table', TableShape)

-----------------
--[[ Symbols ]]--
-----------------

local Symbol              = {}
Symbol.__index            = Symbol
A.Symbol                  = Symbol

local sym_id_count = 0
function A.NewSymbol(name)
  sym_id_count = sym_id_count + 1
  name = name or '_'..tostring(sym_id_count)
  return setmetatable({ namestr=name, id=sym_id_count }, Symbol)
end
function A.issymbol(obj)
  return getmetatable(obj) == Symbol
end
function Symbol:UniqueCopy()
  return A.NewSymbol(self.namestr)
end
function Symbol:__tostring()
  return self.namestr
end
function Symbol:uniquestr()
  return self.namestr ..'($'.. self.id ..')'
end


-- add value shape for symbol
local SymbolShape = Shapes.NewShape()
function SymbolShape:match(val)
  -- symbols can be replaced by strings
  return type(val) == 'string' or A.issymbol(val)
end
Shapes.RegisterShape('symbol', SymbolShape)


-------------------------
--[[ AST Declaration ]]--
-------------------------

local AST                 = { _is_an_ast_node = true }
AST.__index               = AST
A.AST                     = AST

function A.isast(obj)
  return hasproto(obj, AST)
end
local isast = A.isast

function AST:isproto(obj)
  return hasproto(obj, self)
end

local ALL_KINDS           = {}

local function shape_err(shape, val, depth)
  -- ok, the shape didn't match.  Why?
  local msg = 'Could not create AST because of bad shape:\n'
  for k,subshape in pairs(shape.subshapes) do
    if not subshape:match(val[k]) then
      msg = msg .. "  entry for '"..k.."' had bad shape\n"
    end
  end
  for k,_ in pairs(val) do
    if not shape.subshapes[k] then
      msg = msg .. "  value had entry for non-shape-key '"..k.."'\n"
    end
  end
  error(msg, (depth or 1) + 1)
end
function AST:NewKind(name, order, shape)
  local newkind = setmetatable({
    _kind = name,
    _shape = shape and Shapes.CompileShape(shape, order),
  }, self)
  if newkind._shape then
    assert(Shapes.isrecordshape(newkind._shape))
    newkind._shape.errmsgfor = shape_err
  end
  rawset(newkind, '__index', newkind)
  -- DISABLING IMMUTABILITY FOR NOW
  --    Would be good to have some other kind of typed Lua
  --if not newkind:isAbstract() then
  --  rawset(newkind, '__newindex',
  --          function() error("Assignment Not Allowed",2) end)
  --end

  ALL_KINDS[name] = newkind
  A[name]         = newkind
  return newkind
end

function AST:isAbstract()   return not self._shape    end


local ASTShape = Shapes.NewShape()
local function astShapeStub(name)
  local kind  = A[name]
  local shape = Shapes.NewShape({ _ast_kind = kind },ASTShape)
  Shapes.RegisterShape(name, shape)
  return shape
end
function ASTShape:match(val)
  return self._ast_kind:isproto(val)
end


-------------------------
--[[ AST Methods ]]--
-------------------------

function AST:New(values, lexer, token)
  if self:isAbstract() then error('cannot create new abstract AST', 2) end
  if not self._shape:match(values) then self._shape:errmsgfor(values, 2) end

  if lexer and not token then token = lexer:cur() end
  local rawtable = {
    linenumber    = token and token.linenumber or 0,
    filename      = lexer and lexer.source or '',
    offset        = token and token.offset or 0,
  }
  for k,v in pairs(values) do   rawtable[k] = v   end
  return setmetatable(rawtable, self)
end

local function copy_location(dst, src)
  dst.linenumber  = src.linenumber
  dst.filename    = src.filename
  dst.offset      = src.offset
end

function AST:NewFrom(srcnode, values)
  if self:isAbstract() then error('cannot create new abstract AST', 2) end
  if not self._shape:match(values) then self._shape:errmsgfor(values, 2) end

  local rawtable = {}
  copy_location(rawtable, srcnode)
  for k,v in pairs(values) do   rawtable[k] = v   end
  return setmetatable(rawtable, self)
end

-- useful for extending; not checked for shape
function AST:SetVals(vals)
  for k,v in pairs(vals) do self[k] = v end
  return self
end

-- handy for internal use
function AST:clone()
  local copytable = {}
  for k,v in pairs(self) do copytable[k] = v end
  return setmetatable(copytable, getmetatable(self))
end

-------------------------------
--[[ Recursive AST Methods ]]--
-------------------------------

local indent_delta = '  '

-- Generic Printing Function
local function printhelper(indent, val)
  if val == nil then
    print(indent..'$nil')
  elseif type(val) == 'table' then
    if A.issymbol(val) then
      print(indent..val:uniquestr())
    elseif T.istype(val) then
      print(indent..tostring(val))
    elseif A.isast(val) then
      val:printpretty(indent)
    elseif terralib.israwlist(val) then
      if #val == 0 then print(indent..'{}')
      else
        print(indent..'{')
        for _,subval in ipairs(val) do
          printhelper(indent..indent_delta, subval)
        end
        print(indent..'}')
      end
    else
      error('unexpected type of value to print in printpretty')
    end
  else
    print(indent..tostring(val))
  end
end
function AST:printpretty(indent)
  indent = indent or ''
  -- print kind and shape
  -- ___ KIND : (key1, key2, ...)
  -- _____ sub1
  -- _____ sub2 ...

  -- Assemble the first line
  local linestr = indent .. self._kind .. ' : ('
  local iter1 = false
  for k in self._shape:keys() do 
    if iter1 then linestr = linestr .. ', '
             else iter1 = true end
    linestr = linestr .. k
  end
  linestr = linestr..')'
  print(linestr)

  indent = indent .. indent_delta
  -- handle printing different shapes
  for k in self._shape:keys() do
    local val       = self[k]
    printhelper(indent, val)
  end
end


-------------------------------
-- count ast depth and size

function AST:depth()
  local maxdepth = 0
  for k in self._shape:keys() do
    local val = self[k]
    if isast(val) then
      maxdepth = math.max(maxdepth, val:depth())
    elseif terralib.israwlist(val) then
      for _,subval in ipairs(val) do
        maxdepth = math.max(maxdepth, subval:depth())
      end
    end
  end
  return maxdepth + 1
end

function AST:size()
  local size = 0
  for k in self._shape:keys() do
    local val = self[k]
    if isast(val) then
      size = size + val:size()
    elseif terralib.israwlist(val) then
      for _,subval in ipairs(val) do
        size = size + subval:size()
      end
    end
  end
  return size + 1
end


------------------------------------
--[[ Metaprogrammed AST Methods ]]--
------------------------------------

-- passname       is a string naming the new pass
-- copymembers    is a list of keys whose data should be copied
-- defaultvals    is a table of keys and default values to set on
--                  each new AST generated by a default function
function A.NewCopyPass(args)
  local passname    = args.passname
  local copymembers = args.copymembers or {}
  local defaultvals = args.defaultvals or {}
  assert(type(passname) == 'string')
  assert(terralib.israwlist(copymembers))
  assert(type(defaultvals) == 'table')
  -- we incrementally build a blacklist to check for possible
  -- key name conflicts
  local blacklist_storage = { linenumber=true, filename=true, offset=true }
  local function blacklist(name, do_not_write)
    if blacklist_storage[name] then
      error('Name Conflict: the key '..name..' is being overloaded')
    end
    if not do_not_write then blacklist_storage[name] = true end
  end

  -- We'll accumulate all the meta-programmed Lua code into this string
  local metastr = ""

  -- prepare logic to handle copying fields that are the same for all Kinds
  local uniformcopy = ""
  for _,k in ipairs(copymembers) do
    assert(type(k) == 'string')
    blacklist(k)
    uniformcopy = uniformcopy..
      "  copy."..k.." = self."..k.."\n"
  end
  for k,_ in pairs(defaultvals) do
    assert(type(k) == 'string')
    blacklist(k)
    uniformcopy = uniformcopy..
      "  copy."..k.." = D."..k.."\n" -- D is bound to defaultvals
  end

  -- now generate a function for each non-abstract Kind
  for name,kind in pairs(ALL_KINDS) do
  if not kind:isAbstract() then
    local recursions  = ""
    local copy_fields = ""

    -- handle copying the basic shape
    for k in kind._shape:keys() do
      blacklist(k, true)
      local subshape = kind._shape.subshapes[k]
      if Shapes.islistshape(subshape) then
        recursions = recursions..
        "  local "..k.."copy = {}\n"
        if ASTShape:isproto(subshape.subshape) then
          recursions = recursions..
          "  for i,v in ipairs(self."..k..") do\n"..
          "    "..k.."copy[i] = v:"..passname.."(ctxt)\n"..
          "  end\n"
        else
          recursions = recursions..
          "  for i,v in ipairs(self."..k..") do\n"..
          "    "..k.."copy[i] = v\n"..
          "  end\n"
        end
      elseif Shapes.ismaybeshape(subshape) then
        if ASTShape:isproto(subshape.subshape) then
          recursions = recursions..
          "  local "..k.."copy = self."..k.." and "..
                                "self."..k..":"..passname.."(ctxt)\n"
        else
          recursions = recursions..
          "  local "..k.."copy = self."..k.."\n"
        end
      else
        if ASTShape:isproto(subshape) then
          recursions = recursions..
          "  local "..k.."copy = self."..k..":"..passname.."(ctxt)\n"
        else
          recursions = recursions..
          "  local "..k.."copy = self."..k.."\n"
        end
      end

      -- for filling out the :NewFrom(self,{...})
      copy_fields = copy_fields..
      "    "..k.." = "..k.."copy,\n"
    end

    -- assemble the function
    metastr = metastr..
      "function A."..name..":"..passname.."(ctxt)\n"..
      recursions..
      "  local copy = A."..name..":NewFrom(self, {\n"..
      copy_fields..
      "  })\n"..
      uniformcopy..
      "  return copy\n"..
      "end\n"
  end
  end

  if args.verbose then
    print(metastr)
  end

  local setup_func = assert(loadstring(metastr))
  setfenv(setup_func, { A = A, D = defaultvals, ipairs = ipairs })
  setup_func()
end


function A.NewInertPass(args)
  local passname    = args.passname
  assert(type(passname) == 'string')

  -- We'll accumulate all the meta-programmed Lua code into this string
  local metastr = ""

  -- now generate a function for each non-abstract Kind
  for name,kind in pairs(ALL_KINDS) do
  if not kind:isAbstract() then
    local recursions  = ""

    -- handle copying the basic shape
    for k in kind._shape:keys() do
      local subshape = kind._shape.subshapes[k]
      if Shapes.islistshape(subshape) and
         ASTShape:isproto(subshape.subshape)
      then
        recursions = recursions..
        "  for i,v in ipairs(self."..k..") do\n"..
        "    v:"..passname.."(ctxt)\n"..
        "  end\n"
      elseif Shapes.ismaybeshape(subshape) and
             ASTShape:isproto(subshape.subshape)
      then
        recursions = recursions..
        "  if self."..k.." then self."..k..":"..passname.."(ctxt) end\n"
      elseif ASTShape:isproto(subshape) then
        recursions = recursions..
        "  self."..k..":"..passname.."(ctxt)\n"
      end
    end

    -- assemble the function
    metastr = metastr..
      "function A."..name..":"..passname.."(ctxt)\n"..
      recursions..
      "end\n"
  end
  end

  if args.verbose then
    print(metastr)
  end

  local setup_func = assert(loadstring(metastr))
  setfenv(setup_func, { A = A, ipairs = ipairs })
  setup_func()
end





-------------------------
--[[ AST Nodes ]]--
-------------------------

-- super-classes of AST nodes
AST:NewKind('Expression')
AST:NewKind('Statement')
local ExprShape = astShapeStub('Expression')
-- make expression also allow Lua expressions
function ExprShape:match(val)
  return A.Expression:isproto(val) or
          type(val) == 'function'
end
astShapeStub('Statement')

-- stub for detecting type annotations
local AnnotationShape = Shapes.NewShape()
function AnnotationShape:match(val)
  return type(val) == 'function' or T.istype(val)
end
Shapes.RegisterShape('annotation', AnnotationShape)



-- non-expression/statement nodes
--AST:NewKind('RecordEntry', {'name','expr'}
--                           { name='symbol', expr='Expression' })
AST:NewKind('ArgDecl', {'name', 'annotation'},
                       { name='symbol', annotation=Maybe('annotation') })
astShapeStub('ArgDecl')

AST:NewKind('Function', {'name', 'args', 'body', 'rets'}, {
  name = 'string',
  args = {'ArgDecl'},
  body = {'Statement'},
  rets = {'Expression'},
})

-- Statements
A.Statement:NewKind('DoStmt',   {'body'}, { body={'Statement'} })
A.Statement:NewKind('ExprStmt', {'expr'}, { expr='Expression' })
A.Statement:NewKind('Assignment', {'lvalues', 'rvalues'},
                                { lvalues={'Expression'},
                                  rvalues={'Expression'},  })
A.Statement:NewKind('DeclStmt', {'name', 'annotation', 'expr'}, {
  name        = 'symbol',
  annotation  = Maybe('annotation'),
  expr        = Maybe('Expression'),
})

-- Expressions
A.Expression:NewKind('UnaryOp', {'op', 'expr'},
                                { op='string', expr='Expression' })
A.Expression:NewKind('BinaryOp', {'op', 'lhs', 'rhs'},
                                 { op='string',
                                   lhs='Expression', rhs='Expression' })
A.Expression:NewKind('Name',    {'value'}, { value='symbol' })
A.Expression:NewKind('Number',  {'value'}, { value='number' })
A.Expression:NewKind('String',  {'value'}, { value='string' })
A.Expression:NewKind('Bool',    {'value'}, { value='bool' })
--A.Expression:NewKind('List',    {'exprs'}, { exprs={'Expression'} })
--A.Expression:NewKind('Record', {'entries'}, { entries={'RecordEntry'} })
-- "Lookup" handles base.arg and base[arg] accesses
A.Expression:NewKind('Lookup',  {'base', 'arg'},
                                { base='Expression', arg='Expression' })
-- "Call" handles base(arg1,arg2,...)
A.Expression:NewKind('Call',    {'base', 'args'},
                                { base='Expression', args={'Expression'} })
A.Expression:NewKind('Let',     {'block', 'expr'},
                                { block={'Statement'}, expr='Expression' })

A.Expression:NewKind('Cast',    {'expr'}, { expr = 'Expression' })
A.Expression:NewKind('LuaObj',  {}, {}) -- all data is stored in the type


-- The following control flow constructs are omitted for simplicity.
-- They are relatively simple to add back in to the language, but
-- may be useful to omit when the DSL is focused on the specification of
-- feed-forward pipelines, straight-line code, hardware circuits, or
-- other contexts where we may want fewer or non-standard control flow
-- constructs.

-- If
-- While
-- ForLoop
-- Break
-- Repeat





-----------------------------------
--[[ AST Method Specialization ]]--
-----------------------------------


-- specialization of tree printing
local function printprettyvalue(self, indent)
  indent = indent or ''
  print(indent..self._kind..' : '..tostring(self.value))
end
A.Number.printpretty  = printprettyvalue
A.Bool.printpretty    = printprettyvalue
A.String.printpretty  = printprettyvalue
-- print name symbols if there
function A.Name.printpretty(self, indent)
  indent = indent or ''
  local namestr = tostring(self.value)
  if A.issymbol(self.value) then namestr = self.value:uniquestr() end
  print(indent..self._kind..' : '..namestr)
end


