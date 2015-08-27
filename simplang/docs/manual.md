% Simplang Manual


# Simplang is...

Simplang is a demonstration language using the Lua/Terra compiler
infrastructure.  It lacks certain common features like if-then-else,
loops, and recursive functions, making it appropriately narrow for
the specification of straight-line code / circuits.


# Getting Started (Running Code)

We assume a passing familiarity with the [Lua/Terra ecosystem](http://terralang.org/) in this document.

## Hello, 42!

Since Simplang doesn't support string values, let's do some arithmetic instead

```
import 'simplang.simplang'

local simpl getanswer()
  return 21 + 21
end

print(getanswer())
```

Save this in a file `hello42.t`.  Then, execute the command `terra hello42.t` to run the program through Terra to print out `42`.  Doing so will explode in your face.  Ow. (see below)

## Simple Ways to Get Simplang Working

The problem we just ran into is that Terra needs to know where the simplang [`release/`](../release) directory is before it "imports" the language.  Here are three simple ways to do that.  (note that we can't solve this problem inside of the `hello42.t` script because `import` statements are early evaluated before any of the rest of the script starts executing.

### Simplang Launcher Script (Fast Start)

The simplest solution is to just use the provided executable launcher script.  You can find it at [`bin/simpl`](../bin/simpl).  To run `hello42.t`, we just type

```./bin/simpl hello42.t```

or any other valid path to the simpl script from wherever you happen to be.  For instance, you can add the [`bin/`](../bin) directory to your command line path to make `simpl` easily accessible wherever you find yourself in your system.

### Adding Simplang to `TERRA_PATH`

If you work with a lot of Terra DSLs, you might find it simpler to just add simplang's location to an environment variable.  You can do this by extending `TERRA_PATH` with `release/?.t`.  See the Terra documentation for more details.

### Custom Launcher Script

If you eventually feel like you want more control of the startup process so that you can better integrate simplang into your tool or together with other Terra DSLs, you can always just use your own launch script and modify the `package.terrapath` variable directly there.  If you're going down this route, take a look at [`bin/simpl`](../bin/simpl) for a skeleton example.


## Using Simplang / Terra from `C` code

For a more detailed description of ways of using Simplang from host C code, see [`example_c/README.md`](../example_c/README.md) and example code.

There are two ways to use Simplang from C code.  The first way is to embed a Lua/Terra interpreter in the C program.  If you go this route, then everything except for setting up the `package.terrapath` variable with the [`release/`](../release) directory is detailed in the Terra and Lua docs.

The second way is to use Simplang scripts as code generators during your build process.  The key to this process is the `SL.compiletofile(...)` function, which is described [later on](#static-function-compilation).


# Getting Started (Writing Code)

## Simplib
In the following we assume that the Simplang standard library has been
included already using the line
```
local SL = require 'simplang.simplib'
```

## Functions

Simplang allows programmers to write simple straight-line code functions.  We can define one of these functions either anonymously, inline, or in a statement

```
-- define a function as a global symbol in Lua
simpl foo()
  return 42
end

-- define a function as a local symbol
local simpl bar()
  return 42 + 1
end

-- define a function anonymously and assign to a variable
local baz = simpl()
  return 42 + 2
end
```

Functions can be given arguments, but those arguments must be typed

```
local simpl scale_and_add_one( s : SL.num, x : SL.num )
  return s*x + 1
end
```

(notice that we need to look up Simplang types in the Simplang standard library)

Simplang functions can also return multiple values.

```
local simpl some_pi()
  return 3, 1, 4, 1
end

local a, b, c, d = some_pi()
print(a,b,c,d) -- will print    3   1   4   1
```

While Simplang lacks fully-recursive functions, you can call functions from
other functions.

```
local simpl square( x : SL.num )
  return x * x
end
local simpl quad( x : SL.num )
  return square(square(x))
end
assert(quad(2) == 16)
```

## Types, Literals, Constants, Values

### Types
Simplang has two primitive types
```
SL.num SL.bool
```

### Literals and Constants
Numeric literals can be written into Simplang code as you would expect.

Aside from literals, constant values can also be computed in Lua and then
captured by a Simplang function definition.  However, to allow programmers to be more explicit, we also provide Lua-level constants.

```
local answer  = SL.Constant(SL.num, 42)

local simpl foo()
  return answer
end
```

### Casting

Simplang supports explicit typecasting.  This can be used to cast between boolean and numeric values.

To perform an explicit typecast, you just use a type as if it were a function.
```
local simpl tobool( x : SL.num )
  return SL.bool(x)
end
```


## Variables and Assignment

Aside from function arguments, Simplang allows for the declaration of
variables.  A variable must either be explicitly typed or assigned
an initial value (from which the type can be inferred).

```
local simpl foo()
  var x : SL.num
  var y = 0.0
  var z : SL.num = 1.0
  return y + z
end
```

Assignments to variables are also allowed, including to multiple variables at once.  This can be used to capture multiple return values from a function call.

```
local simpl square_triple(x : SL.num)
  var square : SL.num
  var triple : SL.num
  var a = x * x
  square, triple = a, a*x
  return square, triple
end

local simpl foo()
  var x : SL.num
  var y : SL.num
  x,y = square_triple(3)
end
```

However, the current version of Simplang does not support the declaration and assignment of multiple variables at the same time.

## Control Flow

The scope of variables can be limited with do blocks, though other kinds of control flow are omitted from the language.

```
local simpl shadow()
  var x = 3
  do
    var x = 5
  end
  return x
end

assert(shadow() == 3)
```


## Primitive Expressions

Simplang supports the following kinds of operations between primitive expressions.

```
local simpl arithmetic_plus()
  var x  = 12
  var y  = 32
  var bt = true
  var bf = false

  var negx    = -x
  var notb    = not bt

  var bor     = bt or bf
  var band    = bt and bf

  var lt      = x < y
  var gt      = x > y
  var lte     = x <= y
  var gte     = x >= y
  var eq      = x == y
  var neq     = x ~= y    -- notice Lua style not-equal syntax

  var sum     = x + y
  var diff    = x - y
  var prod    = x * y
  var quot    = x / y

  var parens  = x * (x + y)
end
```

### Built-in functions

There is a built-in assertion function `SL.assert()` that's useful for writing testing code.  If the argument evaluates to false, then the entire program will terminate.

```
local simpl runtest()
  var answer = 42
  SL.assert(answer / 2 == 21)
end
```

### External Functions

You can extend Simplang with custom external functions, provided they follow
a Terra calling convention (e.g. C functions).  To do this, the `SL.extern()` library function is provided.

For example, suppose you want to use a sqrt function.  You could import the C math library using Terra and then bind the function in using `SL.extern()`
```
local cmath = terralib.includecstring [[#include "math.h"]]
local sqrt = SL.extern('sqrt', SL.arrow(SL.num, SL.num), cmath.sqrt)
```
The first argument to `SL.extern` is the function name for debugging purposes.
The second argument is the function's type, and the third argument is the implementation of the function.

For reference, you should assume that the `SL.num` type is represented with the C/Terra type `double`.

WARNING: In general, Simplang assumes that functions imported in this way are "referentially transparent" in the sense that they should be indistinguishable from a deterministic state-free function _from the perspective of Simplang_.  This allows that the implementation of an external function could still use side-effects or randomness to produce the same result more efficiently, or to record profiling information, etc.




# Simplang Lua API

Since Simplang is embedded in Lua, the functions, constants and types are all represented as Lua objects/tables.  In order to help Lua scripts work with these objects, we provide a set of functions that one can introspect with.


## Constant Introspection

To test whether a Lua value is a Simplang constant
```
SL.isconstant( obj )
```

To get the value of the constant
```
local val = constant:get()
```

To get the type of the constant
```
local typ = constant:gettype()
```


## Function Introspection

To test whether a Lua value is a Simplang function
```
SL.isfunction( obj )
```

To test whether a Simplang function has been compiled yet
```
func:iscompiled()
```

To force compilation without calling the function
```
func:compile()
```

To get a string containing the function's declared name
```
local name = func:getname()
```

To get the type of the function
```
local typ = func:gettype()
```

Note that functions have a special `SL.arrow(...)` type that cannot be used as a value inside of Simplang programs.  However, arrow types can be constructed by passing a Lua list of argument types and Lua list of return types to the arrow constructor.  e.g. `SL.arrow({SL.num},{SL.bool, SL.num})` is a function type for a function that takes one numeric argument and returns a boolean and numeric argument.  A Simplang function that neither takes any arguments, nor returns any values has type `SL.arrow({},{})`.



## Type Introspection

To test whether a Lua value is a Simplang type
```
SL.istype( obj )
```

If an object is a Simplang type, then you can test for what kind of type it is with the following functions
```
typ:isvalue()
  typ:isprimitive() -- all primitives are values

typ:isarrow()
```

Simplang ensures that all types are resolved to the same object, which means it's safe to compare two types using an equality check
```
assert(SL.arrow({},{}) == SL.arrow({},{}))
```

Any value type can also be tested for what sub-class of value it is using the following tests.
```
valtyp:isnumeric()
valtyp:islogical()
```

The argument and return lists for arrow types can be extracted using the following two functions
```
local argtyps = arrowtyp:argtypes()
local rettyps = arrowtyp:rettypes()
```

## Static Function Compilation

To compile out a set of Simplang functions into an object file
```
SL.compiletofile(object_filename, header_filename, {
  fname1 = function1,
  fname2 = function2,
  ...
})
```
`object_filename` and `header_filename` are strings with filesystem paths specifying where the results should be placed.  If `header_filename` is `nil` then no header will be generated.  The third argument is a table of functions to be exposed as visible symbols in the object file.  The table keys allow you to use an alternate function name when exporting/compiling in this way.

See the [`example_c/`](../example_c) directory for an example of `SL.compiletofile()` use.



