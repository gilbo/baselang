-- This function provides support routines to the rest of the tests
-- Do not run it as a test.

local A     = require 'simplang.src.ast'

local test = {}

function test.eq(a,b)
  if a ~= b then
    error(tostring(a) .. " ~= "  .. tostring(b),2)
  end
end
function test.neq(a,b)
  if a == b then
    error(tostring(a) .. " == "  .. tostring(b),2)
  end
end

function test.seteq(a,b)
  for id,_ in pairs(a) do
    if not b[id] then
      error('Element ' .. tostring(id) .. ' of the left-set was not found '..
            'in the right-set', 2)
    end
  end
  for id,_ in pairs(b) do
    if not a[id] then
      error('Element ' .. tostring(id) .. ' of the right-set was not found '..
            'in the left-set', 2)
    end
  end
end

function test.aeq(a, b)
  if #a ~= #b then error("Arrays are not of equal length", 2) end
  for i = 1, #a do
    if a[i] ~= b[i] then error("Element " .. tostring(i) .. " of arrays do not match (" .. tostring(a[i]) .. ", " .. tostring(b[i]) .. ")", 2) end
  end
end

function test.rec_aeq(a, b, idxstr)
  idxstr = idxstr or ''
  if type(a) ~= 'table' or type(b) ~= 'table' then
    if a ~= b then error("Element (index"..idxstr.." ) "..
                         "of arrays does not match "..
                         "( "..tostring(a).." vs. "..tostring(b).." )",
                         2) end
  else
    if #a ~= #b then error("(Sub-)arrays (index "..idxstr.." ) "..
                           "do not have matching length "..
                           "( "..tostring(#a).." vs. "..tostring(#b).." )",
                           2) end
    -- recurse
    for i=1,#a do
      test.rec_aeq(a[i],b[i], idxstr..' '..tostring(i))
    end
  end
end

-- for diagnosing
function test.rec_aprint(a, reccall)
  if type(a) ~= 'table' then
    if reccall then return tostring(a)
               else print(a) end
  else
    local str = '{'
    for i=1,#a do
      if i > 1 then str = str .. ',' end
      str = str .. test.rec_aprint(a[i], true)
    end
    str = str .. '}'
    if reccall then return str
               else print(str) end
  end
end

local zero_diff = .0000005
function test.fuzzy_aeq (a, b)
  if #a ~= #b then error("Arrays are not of equal length", 2) end
  for i = 1, #a do
    local d = a[i] - b[i]
    if d < 0 then d = -d end
    if d > zero_diff then error("Element " .. tostring(i) .. " of arrays do not match (" .. tostring(a[i]) .. ", " .. tostring(b[i]) .. ")", 2) end
  end
end
function test.fuzzy_eq (a, b)
  local d = a - b
  if d < 0 then d = -d end
  if d > zero_diff then error(tostring(a) .. " ~= " .. tostring(b),2) end
end

-- This code is based off tests/coverage.t from the terra project
function test.fail(fn, match)
  local msg = ''
  local function handler ( errobj )
    msg = tostring(errobj) .. '\n' .. debug.traceback()
  end
  local success = xpcall(fn, handler)
  if success then
    error("Expected function to fail, but it succeeded.", 2)
  elseif not string.match(msg,match) then
    error("Function did not produce the expected error: " .. msg, 2)
  end
end



-- define equality testing for ASTs
local function asteq_prim(lhs, rhs, fields, prefix)
  prefix = prefix or 'root'
  if A.isast(lhs) then
    if not A.isast(rhs) then
      return 'left was ast and right was not at path: '..prefix
    end
    if lhs._kind ~= rhs._kind then
      return 'left and right ast kinds do not match: '..lhs._kind..
             ' vs. '..rhs._kind..' at path '..prefix
    end
    local shape = lhs._shape
    for k in shape:keys() do
      local errmsg = asteq_prim(lhs[k], rhs[k], fields, prefix..'.'..k)
      if errmsg then return errmsg end
    end
    for _,k in ipairs(fields) do
      local errmsg = asteq_prim(lhs[k], rhs[k], fields, prefix..'.'..k)
      if errmsg then return errmsg end
    end

  elseif terralib.israwlist(lhs) then
    if not terralib.israwlist(rhs) then
      return 'left was list and right was not at path: '..prefix
    end
    if #lhs ~= #rhs then
      return 'left and right lists have different lengths: '..
             tostring(lhs)..' vs. '..tostring(rhs)..' at path '..prefix
    end
    for i=1,#lhs do
      local errmsg = asteq_prim(lhs[i], rhs[i], fields,
                                prefix..'['..tostring(i)..']')
      if errmsg then return errmsg end
    end

  elseif lhs ~= rhs then
    return 'left and right values are not equal: '..
           tostring(lhs)..' vs. '..tostring(rhs)..' at path '..prefix
  end
end
function test.asteq(lhs,rhs,fields)
  local errmsg = asteq_prim(lhs,rhs,fields or {})
  if errmsg then
    print('two ASTs are not equal, printing left, then right AST')
    lhs:printpretty()
    rhs:printpretty()
    print('specifically, we got the following error')
    print(errmsg)
    error('Two preceding ASTs should have matched', 2)
  end
end



return test