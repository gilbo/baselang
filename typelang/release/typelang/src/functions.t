local F = {}
package.loaded["typelang.src.functions"] = F

-------------------------------------------------------------------------------
-- Functions
--    is the file where we define the language's notion of a function
-------------------------------------------------------------------------------

local A = require 'typelang.src.ast'
local T = require 'typelang.src.types'

local TC = require 'typelang.src.typechecker'
local CG = require 'typelang.src.codegen'

local Stats = require 'typelang.src.stats'

-------------------------------------------------------------------------------
--[[                          Function Definition                          ]]--
-------------------------------------------------------------------------------

local Func = {}
Func.__index = Func

local function isfunction(obj) return getmetatable(obj) == Func end
F.isfunction = isfunction

function F.NewFunction(params)
  local decl_ast = assert(params.decl_ast, 'forgot decl_ast arg')

  local tctimer       = Stats.NewTimer(decl_ast.name .. '_typecheck_time')

  -- let's typecheck on definition for now
  tctimer:start()
  local typed_ast = TC.typecheck(decl_ast)
  tctimer:stop()

  local compiletimer  = Stats.NewTimer(typed_ast.name .. '_compilation_time')

  local fn = setmetatable({
    _decl_ast       = decl_ast,
    _typed_ast      = typed_ast,
    _func_type      = typed_ast.type,
    _compiled_func  = nil,

    _name             = typed_ast.name,
    _compile_timer    = compiletimer,
    _typecheck_timer  = tctimer,
  }, Func)
  return fn
end

function Func:gettype() return self._func_type  end
function Func:getname() return self._name       end

function Func:iscompiled()
  return nil ~= self._compiled_func
end

function Func:compile()
  if self:iscompiled() then return end

  self._compile_timer:start()
  self._compiled_func = CG.codegen(self._typed_ast)
  self._compile_timer:stop()
  self._compiled_func:compile() -- do Terra compilation
end

function Func:__call(...)
  self:compile()

  -- convert input
  local rawargs   = {...}
  local targs     = {}
  local argtypes  = self._func_type:argtypes()
  if #rawargs ~= #argtypes then
    error('function call to '..self._name..'() expected '..
          tostring(#argtypes)..' arguments, but got '..tostring(#rawargs), 2)
  end
  for i,a in ipairs(rawargs) do
    if not T.checkluaval(a, argtypes[i]) then
      error('argument #'..tostring(i)..' cannot be safely converted to '..
            'a value of type '..tostring(argtypes[i]), 2)
    end
    targs[i] = T.luatoterraval(a, argtypes[i])
  end

  -- perform function execution
  local rawret = self._compiled_func(unpack(targs))

  -- possibly convert output
  local rettypes = self._func_type:rettypes()
  if #rettypes == 0 then
    return nil
  elseif #rettypes == 1 then
    return T.terratoluaval(rawret, rettypes[1])
  else
    local retvals = {}
    for i,typ in ipairs(rettypes) do
      retvals[i] = T.terratoluaval(rawret['_'..tostring(i-1)], typ)
    end
    return unpack(retvals)
  end
end

function Func:_INTERNAL_getterrafunc()
  self:compile()
  return self._compiled_func
end

-------------------------------------------------------------------------------
--[[                              Diagnostics                              ]]--
-------------------------------------------------------------------------------

-- cheating right now on these by just plumbing through
-- to Terra without thought; e.g. stats should also measure
-- the time our own compiler takes
function Func:printstats()
  local tfunc = self:_INTERNAL_getterrafunc()

  -- unsafe way to get the stats:
  local terrastats = tfunc:getdefinitions()[1].stats
  print(self._name, 'definition', self._func_type)
  print('','typechecking',self._typecheck_timer:getsum())
  print('','compilation',self._compile_timer:getsum())
  print('','terra stats')
  for k,v in pairs(terrastats) do
    print('','',k,v*1.0e3)
  end
end

function Func:printpretty()
  -- should implement our own pretty print...?
  self:compile()
  self._compiled_func:printpretty()
end

-------------------------------------------------------------------------------
--[[                        Static Code Generation                         ]]--
-------------------------------------------------------------------------------

local function valtyp_string(typ)
      if typ == T.int32   then return 'int32_t'
  elseif typ == T.uint64  then return 'uint64_t'
  elseif typ == T.float   then return 'float'
  elseif typ == T.double  then return 'double'
  elseif typ == T.bool    then return 'bool'
  else error('INTERNAL: unexpected type: '..tostring(typ)) end
end

local function generate_signature(fname, func)
  local sig   = func:gettype()
  local atyps = sig:argtypes()
  local rtyps = sig:rettypes()
  assert(#rtyps <= 1)

  local retstr = ''
  local argstr = ''

  -- define the arguments
  for arg_i,typ in ipairs(atyps) do
    if arg_i ~= 1 then argstr = argstr..', ' end
    argstr = argstr..valtyp_string(typ)
  end
  if #atyps > 0 then argstr = ' '..argstr..' ' end

  -- define the return type
  if #rtyps > 0 then
    retstr = valtyp_string(rtyps[1])
  else
    retstr = 'void'
  end

  local defstr = retstr..' '..fname..'('..argstr..');'
  return defstr
end

local function generate_header(headerfile, func_defs)
  local stdincludes =
    '#include "stdbool.h"\n'..
    '#include "stdint.h"\n'

  -- uppercase the name and replace every non alpha-numeric character
  -- with an underscore.  Finally, prefix and suffix underscores
  local protection_variable = headerfile:upper():gsub('[^%w]','_')
  protection_variable = '_'..protection_variable..'_'

  local fdefs = ''
  for _,s in ipairs(func_defs)  do fdefs = fdefs..s..'\n' end

  local filecontents =
  '#ifndef '..protection_variable..'\n'..
  '#define '..protection_variable..'\n'..
  '\n'..
  stdincludes..
  '\n'..
  fdefs..
  '\n'..
  '#endif\n'

  return filecontents
end

function F.compiletofile(objfile, headerfile, ftable)
  local terra_ftable    = {}

  local c_func_defs     = {}

  for key,func in pairs(ftable) do
    if not type(key) == 'string' then
      error('can only use string keys; '..
            'they will be translated to function names', 2)
    end
    if not isfunction(func) then
      error('must supply Typelang functions as table values: '..
            tostring(key), 2)
    end

    if #func:gettype():rettypes() > 1 then
      error('cannot compile a C function from a function with multiple '..
            'return values: '..tostring(key), 2)
    end

    table.insert(c_func_defs, generate_signature(key, func))

    terra_ftable[key] = func:_INTERNAL_getterrafunc()
  end

  -- generate and write the header file
  if headerfile then
    local headercontents =
      generate_header(headerfile, c_func_defs)
    local outfile, err, errcode = io.open(headerfile, "w")
    if not outfile then
      error("Error while trying to write header file '"..headerfile.."':\n"..
            err, 2)
    end -- otherwise...
    outfile:write(headercontents)
    outfile:close()
  end

  -- ask Terra to generate the corresponding object file
  terralib.saveobj(objfile, terra_ftable)
end




