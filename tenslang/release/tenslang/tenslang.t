-- File to be imported, defining the language
-- put import 'tenslang.tenslang' at the top of files

-- shim in the coverage analysis
--require 'tenslang.src.coverage'

local P           = require 'tenslang.src.parser'
local Specializer = require 'tenslang.src.specializer'
local F           = require 'tenslang.src.functions'
local Lib         = require 'tenslang.tenslib'
-- other passes and things?

local function handleStatement(self, lexer)
  local ast, assigntuple = P.ParseStatement(lexer)
  local constructor = function(env_fn)
    if Lib._UNIT_TEST_PARSER then
      return ast
    elseif Lib._UNIT_TEST_SPECIALIZER then
      return Specializer.specialize(ast, env_fn())
    else
      local decl_ast = Specializer.specialize(ast, env_fn())
      return F.NewFunction { decl_ast = decl_ast }
    end
  end
  return constructor, assigntuple
end

local function handleExpression(self, lexer)
  local ast = P.ParseExpression(lexer)
  local constructor = function(env_fn)
    if Lib._UNIT_TEST_PARSER then
      return ast
    elseif Lib._UNIT_TEST_SPECIALIZER then
      return Specializer.specialize(ast, env_fn())
    else
      local decl_ast = Specializer.specialize(ast, env_fn())
      return F.NewFunction { decl_ast = decl_ast }
    end
  end
  return constructor
end


local tenslanguage = {
  name          = 'tenslang',
  entrypoints   = {'tensl'},
  keywords      = {
    '_', -- always good to reserve the underscore for language use
    'var',
  },

  expression      = handleExpression,
  statement       = handleStatement,
  localstatement  = handleStatement,
}

return tenslanguage
