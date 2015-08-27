-- File to be imported, defining the language
-- put import 'plexlang.plexlang' at the top of files

-- shim in the coverage analysis
--require 'plexlang.src.coverage'

local P           = require 'plexlang.src.parser'
local Specializer = require 'plexlang.src.specializer'
local F           = require 'plexlang.src.functions'
local Lib         = require 'plexlang.plexlib'
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


local plexlanguage = {
  name          = 'plexlang',
  entrypoints   = {'plexl'},
  keywords      = {
    '_', -- always good to reserve the underscore for language use
    'var',
  },

  expression      = handleExpression,
  statement       = handleStatement,
  localstatement  = handleStatement,
}

return plexlanguage
