local P = {}
package.loaded["tenslang.src.parser"] = P

local T     = require 'tenslang.src.types'
local A     = require 'tenslang.src.ast'
local pratt = require 'tenslang.src.pratt'

local Lang  = {}

function P.ParseExpression(lexer)
  return pratt.Parse(Lang, lexer, 'expression_entry')
end

function P.ParseStatement(lexer)
  return pratt.Parse(Lang, lexer, 'statement_entry')
end

-------------------------------------------------------------------------------
--[[                         Top Level Constructs                          ]]--
-------------------------------------------------------------------------------

local LANG_NAME = 'tensl'

function Lang.expression_entry(P)
  local code_type
  local entry = P:expect(LANG_NAME)
  -- only one type for now
  code_type = 'function'

  -- TODO: Support quotes/let-expressions

  -- this must be an anonymous function
  if code_type == 'function' then
    local fname = P:genAnonName()
    return P:function_decl(fname, entry)
  else
    assert(false)
  end
end

function Lang.statement_entry(P)
  local code_type
  local entry = P:expect(LANG_NAME)
  -- only one type for now
  code_type = 'function'

  if code_type == 'function' then
    local fname, nametuple = P:functionName()
    return P:function_decl(fname, entry), nametuple
  else
    assert(false)
  end
end


function Lang.function_decl(P, fname, anchortoken)
  -- parse args
  local args        = {}
  local openparens  = P:expect('(')
  if not P:matches(')') then
    repeat
      local nametoken   = P:expect(P.name)
      local annotation  = nil
      if P:nextif(':') then
        annotation = P:luaexpr()
      end
      table.insert(args,
        A.ArgDecl:New({
          name        = nametoken.value,
          annotation  = annotation,
        }, P, nametoken))
    until not P:nextif(',')
  end
  P:expectmatch(')', '(', openparens.linenumber)

  -- parse body
  local body = P:block()
  -- parse optional return statement
  local rets  = {}
  if P:nextif('return') then
    rets[1] = P:expr()
    while P:nextif(',') do
      table.insert(rets, P:expr())
    end
  end

  -- closing
  P:expect('end')

  return A.Function:New({
    name = fname,
    args = args,
    body = body,
    rets = rets,
  },P,anchortoken)
end

-------------------------------------------------------------------------------
--[[                              Statements                               ]]--
-------------------------------------------------------------------------------

-- returns a list of statements, potentially none
local block_terminators = {
  ['end'] = true,
  --['else'] = true,
  --['elseif'] = true,
  --['until'] = true,
  --['break'] = true,
  --['in'] = true, -- for let-quotes
  ['return'] = true, -- for functions
}
function Lang.block(P)
  local stmts = {}
  while not block_terminators[P:cur().type] do
    table.insert(stmts, P:statement())
  end
  return stmts
end
function Lang.statement(P)
  local firsttoken = P:cur()

  if P:nextif('do') then
    local body = P:block()
    P:expect('end')
    return A.DoStmt:New({ body=body }, P, firsttoken)

  elseif P:nextif('var') then
    local namestr     = P:expect(P.name).value
    local annotation  = nil
    local expr        = nil
    if P:nextif(':') then
      annotation = P:luaexpr()
    end
    if P:nextif('=') then
      expr = P:expr()
    end
    return A.DeclStmt:New({
      name  = namestr,
      annotation  = annotation,
      expr        = expr,
    }, P, firsttoken)

  else
    local exprs = { P:expr() }
    while P:nextif(',') do table.insert(exprs, P:expr()) end

    if P:nextif('=') then
      local rvalues = { P:expr() }
      while P:nextif(',') do table.insert(rvalues, P:expr()) end
      return A.Assignment:New({
        lvalues = exprs, rvalues = rvalues
      }, P, firsttoken)
    else
      if #exprs ~= 1 then
        P:error('unexpected comma separated expressions')
      end
      return A.ExprStmt:New({ expr = exprs[1] }, P, firsttoken)
    end

  end
end

-------------------------------------------------------------------------------
--[[                             Expressions                               ]]--
-------------------------------------------------------------------------------

local unary_precedence = 5
local function unary(P)
  local optoken = P:next()
  local expr    = P:expr(unary_precedence)
  return A.UnaryOp:New({ op = optoken.type, expr = expr }, P, optoken)
end

local function leftbinary(P, lhs)
  local optoken = P:next()
  local rhs     = P:expr(optoken.type)
  return A.BinaryOp:New({ op = optoken.type, lhs=lhs, rhs=rhs }, P, optoken)
end

local function rightbinary(P, lhs)
  local optoken = P:next()
  local rhs     = P:expr(optoken.type, "right")
  return A.BinaryOp:New({ op = optoken.type, lhs=lhs, rhs=rhs }, P, optoken)
end

local tensorprecedence = 0
local function tensormap(P)
  local maptoken = P:expect(':')
  P:expect('[')
  local names   = { P:expect(P.name).value }
  while P:nextif(',') do
    table.insert( names, P:expect(P.name).value )
  end
  P:expect(']')
  local expr = P:expr(tensorprecedence)

  return A.TensorMap:New({ names=names, expr=expr }, P, maptoken)
end
local function tensorfold(P)
  local optoken = P:next()
  P:expect('[')
  local names   = { P:expect(P.name).value }
  while P:nextif(',') do
    table.insert( names, P:expect(P.name).value )
  end
  P:expect(']')
  local expr = P:expr(tensorprecedence)
  
  return A.TensorFold:New({ names=names, op=optoken.type, expr=expr },
                          P, optoken)
end

local function sqaccess(P, lhs)
  local openbracket = P:expect('[')
  local exprlist    = { P:expr() }
  while P:nextif(',') do
    table.insert(exprlist, P:expr())
  end
  P:expectmatch(']', '[', openbracket.linenumber)
  return A.Lookup:New({ base=lhs, args=exprlist }, P, openbracket)
end

local function dotaccess(P, lhs)
  local dot         = P:expect('.')
  local nametoken   = P:expect(P.name)
  local strnode     = A.String:New({ value = nametoken.value }, P, nametoken)
  return A.Lookup:New({ base=lhs, args={strnode} }, P, dot)
end

local function callexpr(P, lhs)
  local openparens  = P:expect('(')
  local arglist     = {}
  if not P:matches(')') then
    repeat
      table.insert(arglist, P:expr())
    until not P:nextif(',')
  end
  P:expectmatch(')','(', openparens.linenumber)
  return A.Call:New({ base = lhs, args = arglist }, P, openparens)
end

Lang.expr = pratt.Pratt()
:prefix("-",    unary)
:prefix("not",  unary)
:prefix(":",    tensormap)
:prefix("+",    tensorfold)
:prefix("*",    tensorfold)
:infix("or",  1, leftbinary)
:infix("and", 1, leftbinary)

:infix("<",   2, leftbinary)
:infix(">",   2, leftbinary)
:infix("<=",  2, leftbinary)
:infix(">=",  2, leftbinary)
:infix("==",  2, leftbinary)
:infix("~=",  2, leftbinary)

:infix("+",   3, leftbinary)
:infix("-",   3, leftbinary)
:infix("*",   4, leftbinary)
:infix('/',   4, leftbinary)

--:infix('^',   6, rightbinary)
:infix('[',   7, sqaccess)
:infix('.',   8, dotaccess)
:infix('(',   9, callexpr)
:prefix(pratt.default, function(P) return P:simpleexpr() end)


function Lang.simpleexpr(P)
  local starttoken = P:cur()

  if P:matches(P.name) then
    local name = P:next().value
    P:ref(name) -- register this name as a possible up-value
    return A.Name:New({ value=name }, P, starttoken)

  elseif P:matches(P.string) then
    local strtoken = P:next()
    return A.String:New({ value=strtoken.value }, P, strtoken)

  elseif P:matches(P.number) then
    local numtoken = P:next()
    return A.Number:New({
      value     = numtoken.value,
    }, P, numtoken)

  elseif P:matches('true') or P:matches('false') then
    local val = (P:next().type == 'true')
    return A.Bool:New({ value = val }, P, starttoken)

  elseif P:nextif('(') then
    local subexpr = P:expr()
    P:expectmatch(')', '(', starttoken.linenumber)
    return subexpr

  elseif P:nextif('{') then
    local exprs = {}
    if not P:matches('}') then
      repeat
        table.insert(exprs, P:expr())
      until not P:nextif(',')
    end
    P:expectmatch('}','{',starttoken.linenumber)
    return A.List:New({ exprs = exprs }, P, starttoken)

  else
    P:error('unexpected symbol when trying to parse expression')
  end
end



-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------


function Lang.genAnonName(P)
  local anonname = "anon_"..tostring(P.source)..
                       "_"..tostring(P:cur().linenumber)
  anonname = string.gsub(anonname, "[^%a%d_]","_")
  return anonname
end

function Lang.functionName(P)
  local namestr   = P:expect(P.name).value
  local nametuple = { namestr }
  while P:nextif('.') do
    local nextname = P:expect(P.name).value
    table.insert(nametuple, nextname)
    namestr = namestr..'.'..nextname
  end
  return namestr, { nametuple }
end



