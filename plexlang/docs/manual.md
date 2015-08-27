% Plexlang Manual


# Plexlang is...

Plexlang is a demonstration language using the Lua/Terra compiler
infrastructure.  It lacks certain common features like if-then-else,
loops, and recursive functions, making it appropriately narrow for
the specification of straight-line code / circuits.  However, the language
does include 2 interesting features:

- a modestly rich primitive type system, including both
  single and double precision floating point numbers, as well as signed
  32-bit integers and unsigned 64-bit integers.  The type system also
  produces implicit coercions between these types when maintenance of
  precision can be guaranteed.

- a tensor-type constructor with fixed dimensions, suitable for encoding
  small vectors and matrices.  These tensor types are integrated with
  the various primitive types and various primitive binary operators,
  as well as having a special tensor map/fold syntax derived from
  mathematical expressions.


# Getting Started (Running Code)

We assume a passing familiarity with the [Lua/Terra ecosystem](http://terralang.org/) in this document.

## Hello, 42!

Since Plexlang doesn't support string values, let's do some arithmetic instead

```
import 'plexlang.plexlang'

local plexl getanswer()
  return 21 + 21
end

print(getanswer())
```

Save this in a file `hello42.t`.  Then, execute the command `terra hello42.t` to run the program through Terra to print out `42`.  Doing so will explode in your face.  Ow. (see below)

## Simple Ways to Get Plexlang Working

The problem we just ran into is that Terra needs to know where the plexlang [`release/`](../release) directory is before it "imports" the language.  Here are three simple ways to do that.  (note that we can't solve this problem inside of the `hello42.t` script because `import` statements are early evaluated before any of the rest of the script starts executing.

### Plexlang Launcher Script (Fast Start)

The simplest solution is to just use the provided executable launcher script.  You can find it at [`bin/plexl`](../bin/plexl).  To run `hello42.t`, we just type

```./bin/plexl hello42.t```

or any other valid path to the plexl script from wherever you happen to be.  For instance, you can add the [`bin/`](../bin) directory to your command line path to make `plexl` easily accessible wherever you find yourself in your system.

### Adding Plexlang to `TERRA_PATH`

If you work with a lot of Terra DSLs, you might find it simpler to just add plexlang's location to an environment variable.  You can do this by extending `TERRA_PATH` with `release/?.t`.  See the Terra documentation for more details.

### Custom Launcher Script

If you eventually feel like you want more control of the startup process so that you can better integrate plexlang into your tool or together with other Terra DSLs, you can always just use your own launch script and modify the `package.terrapath` variable directly there.  If you're going down this route, take a look at [`bin/plexl`](../bin/plexl) for a skeleton example.


## Using Plexlang / Terra from `C` code

For a more detailed description of ways of using Plexlang from host C code, see [`example_c/README.md`](../example_c/README.md) and example code.

There are two ways to use Plexlang from C code.  The first way is to embed a Lua/Terra interpreter in the C program.  If you go this route, then everything except for setting up the `package.terrapath` variable with the [`release/`](../release) directory is detailed in the Terra and Lua docs.

The second way is to use Plexlang scripts as code generators during your build process.  The key to this process is the `PL.compiletofile(...)` function, which is described [later on](#static-function-compilation).


# Getting Started (Writing Code)

## Plexlib
In the following we assume that the Plexlang standard library has been
included already using the line
```
local PL = require 'plexlang.plexlib'
```

## Functions

Plexlang allows programmers to write simple straight-line code functions.  We can define one of these functions either anonymously, inline, or in a statement

```
-- define a function as a global symbol in Lua
plexl foo()
  return 42
end

-- define a function as a local symbol
local plexl bar()
  return 42 + 1
end

-- define a function anonymously and assign to a variable
local baz = plexl()
  return 42 + 2
end
```

Functions can be given arguments, but those arguments must be typed

```
local plexl scale_and_add_one( s : PL.double, x : PL.double )
  return s*x + 1.0
end
```

(notice that we need to look up Plexlang types in the Plexlang standard library)

Plexlang functions can also return multiple values.

```
local plexl some_pi()
  return 3, 1, 4, 1
end

local a, b, c, d = some_pi()
print(a,b,c,d) -- will print    3   1   4   1
```

While Plexlang lacks fully-recursive functions, you can call functions from
other functions.

```
local plexl square( x : PL.double )
  return x * x
end
local plexl quad( x : PL.double )
  return square(square(x))
end
assert(quad(2) == 16)
```

## Types, Literals, Constants, Values

### Types
Plexlang has the following primitive types
```
PL.int32 PL.uint64 PL.bool PL.float PL.double
```

In addition to primitive types, Plexlang values can also be tensor-typed.
(For the unfamiliar, tensors are simply a generalization of vectors and
matrices)  Many common tensor types are given explicitly aliases.
For instance,
```
PL.vec3d    -- a vector of 3 doubles
PL.mat2f    -- a 2x2 matrix of floats
PL.mat2x3i  -- a 2x3 matrix of int32s
```

We can also construct vector and matrix types using the more general forms.
```
PL.vector(primitive_type, n)
PL.matrix(primitive_type, n, m)
```
These are just special cases of the more general tensor constructor.
```
PL.tensor(primitive_type, ...)
```

Notice that all tensor types must have a constant fixed dimension.  Tensors are intended to encode small, constant sized data that show up regularly
when dealing with geometry, simulation and graphical data. (e.g. points, colors, linear/affine transformations, stress matrices, etc.)  Trying to use these types to store large vectors of data will result in very bad performance in almost all cases.

### Literals and Constants
Numeric literals can be written into Plexlang code as you would expect; with the expected types.  For instance, here are 4 variants of 0:

```
0    -- inferred as PL.int32
0.0  -- inferred as PL.double
0f
0.0f -- both inferred as PL.float
0UL
0ULL -- both inferred as PL.uint64
```

However, notice that `0L` or `0U` will produce errors because the `int64` and `uint32` types are not supported in Plexlang.

You can also write tensor literals into code
```
{1,2,3} -- a PL.vec3i
{{1.0,0,0},{0,1,0},{0,0,1}} -- a PL.mat3d
```

Aside from literals, constant values can also be computed in Lua and then
captured by a Plexlang function definition.  Plexlang will attempt to infer a reasonable type, but since Lua numbers are a very general type, this may be unsatisfactory.  To help with this issue and improve code documentation, we provide Lua-level constants.

```
local biganswer = PL.Constant(PL.uint64, 42)
local idmat3f   = PL.Constant(PL.float, {{1,0,0},{0,1,0},{0,0,1}})

local plexl foo()
  return idmat3f[0,0], biganswer
end
```

### Casting and Coercion

Plexlang supports explicit typecasting as well as coercion.  The semantics of these are very similar to C and Terra.

In order to perform an explicit typecast, you just use a type as if it were a function.
```
local plexl frac( x : PL.double )
  return x - PL.double( PL.int32(x) )
end

local plexl fracmat( x : PL.mat3d )
  return x - PL.mat3d( PL.mat3i(x) )
end
```

In addition to these explicit casts, Plexlang will insert implicit casts, also called coercions (approximately) whenever doing so would not incur any loss of precision, but would avoid an otherwise annoying type-error.  Specifically, the coercion rules are that

* `PL.float` may be coerced to `PL.double`
* `PL.int32` may be coerced to `PL.double`
* `PL.int32` may be coerced to `PL.uint64` (This is the only one that's not strictly safe)
* If `X` may be coerced to `Y`, then `PL.tensor(X,...)` may be coerced to `PL.tensor(Y,...)` provided that the dimensions of the two tensors match.



## Variables and Assignment

Aside from function arguments, Plexlang allows for the declaration of
variables.  A variable must either be explicitly typed or assigned
an initial value (from which the type can be inferred).

```
local plexl foo()
  var x : PL.double
  var y = 0.0
  var z : PL.double = 1.0
  return y + z
end
```

Assignments to variables are also allowed, including to multiple variables at once.  This can be used to capture multiple return values from a function call.

```
local plexl square_triple(x : PL.double)
  var square : PL.double
  var triple : PL.double
  var a = x * x
  square, triple = a, a*x
  return square, triple
end

local plexl foo()
  var x : PL.double
  var y : PL.double
  x,y = square_triple(3)
end
```

However, the current version of Plexlang does not support the declaration and assignment of multiple variables at the same time.

## Control Flow

The scope of variables can be limited with do blocks, though other kinds of control flow are omitted from the language.

```
local plexl shadow()
  var x = 3
  do
    var x = 5
  end
  return x
end

assert(shadow() == 3)
```


## Primitive Expressions

Plexlang supports the following kinds of operations between primitive expressions.

```
local plexl arithmetic_plus()
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

There is a built-in assertion function `PL.assert()` that's useful for writing testing code.  If the argument evaluates to false, then the entire program will terminate.

```
local plexl runtest()
  var answer = 42
  PL.assert(answer / 2 == 21)
end
```

### External Functions

You can extend Plexlang with custom external functions, provided they follow
a Terra calling convention (e.g. C functions).  To do this, the `PL.extern()` library function is provided.  

For example, suppose you want to use a sqrt function.  You could import the C math library using Terra and then bind the function in using `PL.extern()`
```
local cmath = terralib.includecstring [[#include "math.h"]]
local sqrt = PL.extern('sqrt', PL.arrow(PL.double, PL.double), cmath.sqrt)
```
The first argument to `PL.extern` is the function name for debugging purposes.
The second argument is the function's type, and the third argument is the implementation of the function.

WARNING: In general, Plexlang assumes that functions imported in this way are "referentially transparent" in the sense that they should be indistinguishable from a deterministic state-free function _from the perspective of Plexlang_.  This allows that the implementation of an external function could still use side-effects or randomness to produce the same result more efficiently, or to record profiling information, etc.


## Tensor Expressions

### Tensor-Indexing Expressions

Plexlang's most sophisticated feature is its tensor system.  Most notably,
this includes its special _tensor-indexing_ expressions.  To understand these more intuitively, consider the following expression for the matrix-vector dot product.

```
local plexl matvecprod( m : PL.mat3d, x : PL.vec3d )
  return :[i] +[j] m[i,j] * x[j]
end
```

In more words, this expression says "map over the index `i`, and within that sum over the index `j`, and within that take the product of the `i,j`-th entry of `m` with the `j`-th entry of `x`".

To make this even more intuitive, consider this expression for the dot product:
`+[i] x[i] * y[i]`
Imagine we replaced the `+[i]` with a big summation like a mathematician would write.  Then, we would have exactly the mathematical definition of the dot product.

To make the `:[i]` example clearer, consider the case of scaling a vector by `3`: `:[i] 3 * x[i]`.

As is apparent from the matrix-vector multiplication example, these two _tensor-indexing_ expressions can be combined to express more complicated forms.  In the following code-snippet, you can see how a wide variety of matrix-vector operations can all be expressed from these two simple constructions.

```
var dot_product   = +[i] x[i] * y[i]

var mat_vec_prod  = :[i] +[j] M[i,j] * x[j]

var transpose     = :[i,j] M[j,i]

var inner_product = +[i,j] x[i] * Q[i,j] * y[j]

var outer_product = :[i,j] x[i] * y[j]

var mat_mat_prod  = :[i,j] +[k] A[i,k] * B[k,j]

var trace         = +[i] M[i,i]

var frobenius     = +[i,j] M[i,j] * M[i,j]

var first_column  = :[i] M[i,0]

var sum_columns   = :[i] +[j] M[i,j]
```

Part of the magic of tensor-index expressions is that the appropriate range of the index variables (`i` in `:[i]` or `+[i]`) can be inferred by the type-checker.  In order for this to work, these _tensor-indicex_ variables are given a special type.  If you want to get their numeric value for computation with, you may need to explicitly cast the variable, at which point it can no longer be used to infer its dimension correctly.  Generally this is a minor detail.  The typechecker will complain if something is wrong.

In addition to the summation reduction, Plexlang also supports a multiplication reduction.


### Tensor-Construction Expressions

In addition to the above, tensors can also be constructed out of individual values in code, which always gives a way of more explicitly writing down a detailed expression.

```
var prefix_sum    = { x[0], x[0]+x[1], x[0]+x[1]+x[2] }

var cross_matix   = {{     0, -x[2],  x[1] },
                     {  x[2],     0, -x[0] },
                     { -x[1],  x[0],     0 }}

var repeat_column = {x,x,x}
```




# Plexlang Lua API

Since Plexlang is embedded in Lua, the functions, constants and types are all represented as Lua objects/tables.  In order to help Lua scripts work with these objects, we provide a set of functions that one can introspect with.


## Constant Introspection

To test whether a Lua value is a Plexlang constant
```
PL.isconstant( obj )
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

To test whether a Lua value is a Plexlang function
```
PL.isfunction( obj )
```

To test whether a Plexlang function has been compiled yet
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

Note that functions have a special `PL.arrow(...)` type that cannot be used as a value inside of Plexlang programs.  However, arrow types can be constructed by passing a Lua list of argument types and Lua list of return types to the arrow constructor.  e.g. `PL.arrow({PL.int32},{PL.mat3d, PL.vec3d})` is a function type for a function that takes one `int32` argument and returns a matrix and vector as return values.  A Plexlang function that neither takes any arguments, nor returns any values has type `PL.arrow({},{})`.



## Type Introspection

To test whether a Lua value is a Plexlang type
```
PL.istype( obj )
```

If an object is a Plexlang type, then you can test for what kind of type it is with the following functions
```
typ:isvalue() -- any primitive or tensor type
  typ:isprimitive()
  typ:istensor()
    typ:isvector()
    typ:ismatrix()

typ:isarrow()
```

Plexlang ensures that all types are resolved to the same object, which means it's safe to compare two types using an equality check
```
assert(PL.vec3d == PL.vector(PL.double, 3))
```

To get the underlying primitive type for a tensor-type
```
local primtyp = tensortyp:basetype()
```
If you have a tensor type, then you can also get a Lua list of its dimensions `tensortyp.dims`, from which you can compute how many values there are in what shape.  Plexlang lays these out in row-major order in memory and expects similar nesting throughout the language.

Any value type, primitive or tensor can also be tested for what sub-class of value it is using the following tests.
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

To compile out a set of Plexlang functions into an object file
```
PL.compiletofile(object_filename, header_filename, {
  fname1 = function1,
  fname2 = function2,
  ...
})
```
`object_filename` and `header_filename` are strings with filesystem paths specifying where the results should be placed.  If `header_filename` is `nil` then no header will be generated.  The third argument is a table of functions to be exposed as visible symbols in the object file.  The table keys allow you to use an alternate function name when exporting/compiling in this way.

See the [`example_c/`](../example_c) directory for an example of `PL.compiletofile()` use.



