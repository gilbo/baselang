-- File to be imported, defining the language
-- put import 'simplang.simplang' at the top of files

-- shim in the coverage analysis
--require 'simplang.src.coverage'

local P           = require 'simplang.src.parser'
local Specializer = require 'simplang.src.specializer'
local F           = require 'simplang.src.functions'
local Lib         = require 'simplang.simplib'
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


local simplanguage = {
  name          = 'simplang',
  entrypoints   = {'simpl'},
  keywords      = {
    '_', -- always good to reserve the underscore for language use
    'var',
  },

  expression      = handleExpression,
  statement       = handleStatement,
  localstatement  = handleStatement,
}

return simplanguage
