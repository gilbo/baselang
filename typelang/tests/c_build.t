local test  = require 'tests.test'

import 'typelang.typelang'

local Lib   = require 'typelang.typelib'

------------------------------------------------------------------------------

-- This test is more complicated because it needs to invoke a Makefile build


print("** ENSURE DIRECTORY IS CLEAN")
assert(os.execute('cd example_c; make clean') == 0)
print("** DO BUILD")
assert(os.execute('cd example_c; make') == 0)
print("** BUILD COMPLETED")

-- now we need to check that the build produced working executables
-- with the correct output

local embedded = assert(io.popen('./example_c/embedded 2>&1', 'r'))
local embedded_output = embedded:read('*all')
test.eq(embedded_output,[[
[string "<string>"]:1: causing an intentional error
42
]])
-- print anyway if test didn't fail
print()
print(embedded_output)
embedded:close()

local static_call = assert(io.popen('./example_c/static_call 2>&1', 'r'))
local static_output = static_call:read('*all')
test.eq(static_output,[[
answer should be 42;                  was 42
signedfrac 3.2 should yield 0.2;      was 0.200000
signedfrac -2.3 should yield -0.3;    was -0.300000
]])
-- print anyway if test didn't fail
print()
print(static_output)
static_call:close()



print("** CLEANING UP")
assert(os.execute('cd example_c; make clean') == 0)
print("** CLEANED UP")