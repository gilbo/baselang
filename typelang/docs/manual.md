% Typelang Manual


# Typelang is...

Typelang is a demonstration language using the Lua/Terra compiler
infrastructure.  It lacks certain common features like if-then-else,
loops, and recursive functions, making it appropriately narrow for
the specification of straight-line code / circuits.  However, the language
does include one moderately complicated feature:

- a rich primitive type system, including both
  single and double precision floating point numbers, as well as signed
  32-bit integers and unsigned 64-bit integers.  The type system also
  produces implicit coercions between these types when maintenance of
  precision can be guaranteed.



# Getting Started (Running Code)

We assume a passing familiarity with the [Lua/Terra ecosystem](http://terralang.org/) in this document.

## Hello, 42!

Since Typelang doesn't support string values, let's do some arithmetic instead

```
import 'typelang.typelang'

local typel getanswer()
  return 21 + 21
end

print(getanswer())
```

Save this in a file `hello42.t`.  Then, execute the command `terra hello42.t` to run the program through Terra to print out `42`.  Doing so will explode in your face.  Ow. (see below)

## Simple Ways to Get Typelang Working

The problem we just ran into is that Terra needs to know where the Typelang [`release/`](../release) directory is before it "imports" the language.  Here are three simple ways to do that.  (note that we can't solve this problem inside of the `hello42.t` script because `import` statements are early evaluated before any of the rest of the script starts executing.

### Typelang Launcher Script (Fast Start)

The simplest solution is to just use the provided executable launcher script.  You can find it at [`bin/typel`](../bin/typel).  To run `hello42.t`, we just type

```./bin/typel hello42.t```

or any other valid path to the typel script from wherever you happen to be.  For instance, you can add the [`bin/`](../bin) directory to your command line path to make `typel` easily accessible wherever you find yourself in your system.

### Adding Typelang to `TERRA_PATH`

If you work with a lot of Terra DSLs, you might find it simpler to just add Typelang's location to an environment variable.  You can do this by extending `TERRA_PATH` with `release/?.t`.  See the Terra documentation for more details.

### Custom Launcher Script

If you eventually feel like you want more control of the startup process so that you can better integrate Typelang into your tool or together with other Terra DSLs, you can always just use your own launch script and modify the `package.terrapath` variable directly there.  If you're going down this route, take a look at [`bin/typel`](../bin/typel) for a skeleton example.


## Using Typelang / Terra from `C` code

For a more detailed description of ways of using Typelang from host C code, see [`example_c/README.md`](../example_c/README.md) and example code.

There are two ways to use Typelang from C code.  The first way is to embed a Lua/Terra interpreter in the C program.  If you go this route, then everything except for setting up the `package.terrapath` variable with the [`release/`](../release) directory is detailed in the Terra and Lua docs.

The second way is to use Typelang scripts as code generators during your build process.  The key to this process is the `TL.compiletofile(...)` function, which is described [later on](#static-function-compilation).


# Getting Started (Writing Code)

## Typelib
In the following we assume that the Typelang standard library has been
included already using the line
```
local TL = require 'typelang.typelib'
```

## Functions

Typelang allows programmers to write simple straight-line code functions.  We can define one of these functions either anonymously, inline, or in a statement

```
-- define a function as a global symbol in Lua
typel foo()
  return 42
end

-- define a function as a local symbol
local typel bar()
  return 42 + 1
end

-- define a function anonymously and assign to a variable
local baz = typel()
  return 42 + 2
end
```

Functions can be given arguments, but those arguments must be typed

```
local typel scale_and_add_one( s : TL.double, x : TL.double )
  return s*x + 1.0
end
```

(notice that we need to look up Typelang types in the Typelang standard library)

Typelang functions can also return multiple values.

```
local typel some_pi()
  return 3, 1, 4, 1
end

local a, b, c, d = some_pi()
print(a,b,c,d) -- will print    3   1   4   1
```

While Typelang lacks fully-recursive functions, you can call functions from
other functions.

```
local typel square( x : TL.double )
  return x * x
end
local typel quad( x : TL.double )
  return square(square(x))
end
assert(quad(2) == 16)
```

## Types, Literals, Constants, Values

### Types
Typelang has the following primitive types
```
TL.int32 TL.uint64 TL.bool TL.float TL.double
```

### Literals and Constants
Numeric literals can be written into Typelang code as you would expect; with the expected types.  For instance, here are 4 variants of 0:

```
0    -- inferred as TL.int32
0.0  -- inferred as TL.double
0f
0.0f -- both inferred as TL.float
0UL
0ULL -- both inferred as TL.uint64
```

However, notice that `0L` or `0U` will produce errors because the `int64` and `uint32` types are not supported in Typelang.

Aside from literals, constant values can also be computed in Lua and then
captured by a Typelang function definition.  Typelang will attempt to infer a reasonable type, but since Lua numbers are a very general type, this may be unsatisfactory.  To help with this issue and improve code documentation, we provide Lua-level constants.

```
local biganswer = TL.Constant(TL.uint64, 42)

local typel foo()
  return biganswer
end
```

### Casting and Coercion

Typelang supports explicit typecasting as well as coercion.  The semantics of these are very similar to C and Terra.

In order to perform an explicit typecast, you just use a type as if it were a function.
```
local typel frac( x : TL.double )
  return x - TL.double( TL.int32(x) )
end
```

In addition to these explicit casts, Typelang will insert implicit casts, also called coercions (approximately) whenever doing so would not incur any loss of precision, but would avoid an otherwise annoying type-error.  Specifically, the coercion rules are that

* `TL.float` may be coerced to `TL.double`
* `TL.int32` may be coerced to `TL.double`
* `TL.int32` may be coerced to `TL.uint64` (This is the only one that's not strictly safe)


## Variables and Assignment

Aside from function arguments, Typelang allows for the declaration of
variables.  A variable must either be explicitly typed or assigned
an initial value (from which the type can be inferred).

```
local typel foo()
  var x : TL.double
  var y = 0.0
  var z : TL.double = 1.0
  return y + z
end
```

Assignments to variables are also allowed, including to multiple variables at once.  This can be used to capture multiple return values from a function call.

```
local typel square_triple(x : TL.double)
  var square : TL.double
  var triple : TL.double
  var a = x * x
  square, triple = a, a*x
  return square, triple
end

local typel foo()
  var x : TL.double
  var y : TL.double
  x,y = square_triple(3)
end
```

However, the current version of Typelang does not support the declaration and assignment of multiple variables at the same time.

## Control Flow

The scope of variables can be limited with do blocks, though other kinds of control flow are omitted from the language.

```
local typel shadow()
  var x = 3
  do
    var x = 5
  end
  return x
end

assert(shadow() == 3)
```


## Primitive Expressions

Typelang supports the following kinds of operations between primitive expressions.

```
local typel arithmetic_plus()
  var x  = 12.0
  var y  = 32.0
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

There is a built-in assertion function `TL.assert()` that's useful for writing testing code.  If the argument evaluates to false, then the entire program will terminate.

```
local typel runtest()
  var answer = 42
  TL.assert(answer / 2 == 21)
end
```

### External Functions

You can extend Typelang with custom external functions, provided they follow
a Terra calling convention (e.g. C functions).  To do this, the `TL.extern()` library function is provided.  

For example, suppose you want to use a sqrt function.  You could import the C math library using Terra and then bind the function in using `TL.extern()`
```
local cmath = terralib.includecstring [[#include "math.h"]]
local sqrt = TL.extern('sqrt', TL.arrow(TL.double, TL.double), cmath.sqrt)
```
The first argument to `TL.extern` is the function name for debugging purposes.
The second argument is the function's type, and the third argument is the implementation of the function.

WARNING: In general, Typelang assumes that functions imported in this way are "referentially transparent" in the sense that they should be indistinguishable from a deterministic state-free function _from the perspective of Typelang_.  This allows that the implementation of an external function could still use side-effects or randomness to produce the same result more efficiently, or to record profiling information, etc.





# Typelang Lua API

Since Typelang is embedded in Lua, the functions, constants and types are all represented as Lua objects/tables.  In order to help Lua scripts work with these objects, we provide a set of functions that one can introspect with.


## Constant Introspection

To test whether a Lua value is a Typelang constant
```
TL.isconstant( obj )
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

To test whether a Lua value is a Typelang function
```
TL.isfunction( obj )
```

To test whether a Typelang function has been compiled yet
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

Note that functions have a special `TL.arrow(...)` type that cannot be used as a value inside of Typelang programs.  However, arrow types can be constructed by passing a Lua list of argument types and Lua list of return types to the arrow constructor.  e.g. `TL.arrow({TL.int32},{TL.bool, TL.double})` is a function type for a function that takes one `int32` argument and returns a `bool` and `double` as return values.  A Typelang function that neither takes any arguments, nor returns any values has type `TL.arrow({},{})`.



## Type Introspection

To test whether a Lua value is a Typelang type
```
TL.istype( obj )
```

If an object is a Typelang type, then you can test for what kind of type it is with the following functions
```
typ:isvalue()
  typ:isprimitive() -- all primitives are values

typ:isarrow()
```

Typelang ensures that all types are resolved to the same object, which means it's safe to compare two types using an equality check
```
assert(TL.arrow({},{}) == TL.arrow({},{}))
```

You can test what sub-class of value a type is using the following tests.
```
valtyp:isintegral()
valtyp:isnumeric()
valtyp:islogical()
```

The argument and return lists for arrow types can be extracted using the following two functions
```
local argtyps = arrowtyp:argtypes()
local rettyps = arrowtyp:rettypes()
```

## Static Function Compilation

To compile out a set of Typelang functions into an object file
```
TL.compiletofile(object_filename, header_filename, {
  fname1 = function1,
  fname2 = function2,
  ...
})
```
`object_filename` and `header_filename` are strings with filesystem paths specifying where the results should be placed.  If `header_filename` is `nil` then no header will be generated.  The third argument is a table of functions to be exposed as visible symbols in the object file.  The table keys allow you to use an alternate function name when exporting/compiling in this way.

See the [`example_c/`](../example_c) directory for an example of `TL.compiletofile()` use.



