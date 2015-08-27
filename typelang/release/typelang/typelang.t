-- File to be imported, defining the language
-- put import 'typelang.typelang' at the top of files

-- shim in the coverage analysis
--require 'typelang.src.coverage'

local P           = require 'typelang.src.parser'
local Specializer = require 'typelang.src.specializer'
local F           = require 'typelang.src.functions'
local Lib         = require 'typelang.typelib'
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


local typelanguage = {
  name          = 'typelang',
  entrypoints   = {'typel'},
  keywords      = {
    '_', -- always good to reserve the underscore for language use
    'var',
  },

  expression      = handleExpression,
  statement       = handleStatement,
  localstatement  = handleStatement,
}

return typelanguage
