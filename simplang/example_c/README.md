% C Examples

Thanks to being built on the Terra/Lua infrastructure, Simplang comes equipped with some easy ways to be used in new or existing C/C++ programs.  The examples in this directory demonstrate the two primary strategies for running DSL code from a host C program.


# Embedding an interpreter for Simplang in a C program

This strategy is entirely based on Lua/Terra facilities for embedding.  So it doesn't really involve Simplang specifically.  At program initialization, we build a Lua environment using

```
lua_State * L = luaL_newstate();
luaL_openlibs(L);
terra_init(L);
```

(more details and context are in `embedded_main.c`)

The only slight detail is that as always, the `package.terrapath` variable must be extended with the location of the simplang [`release/`](../release) directory.  For example, this can be accomplished by simply executing an appropriate string command.
```
const char *pathextend =
  "package.terrapath = package.terrapath..';release/?.t'";
if(terra_dostring(L, pathextend)) {
  fprintf(stderr, "%s\n", lua_tostring(L, -1));
  lua_close(L);
  exit(1);
}
```

To read more about how to manage the Lua/Terra interpreter from C, see the appropriate documentation, for instance the overview of the [Lua C API](http://www.lua.org/pil/24.html) and overview of the [Terra C API](http://terralang.org/api.html#c-api).



# Generating Object files for a static build process

This strategy is very similar to a built in Terra facility, but is part of the Simplang API proper.  Rather than embed a whole Lua/Terra interpreter into a project, we generate object files that can be statically compiled into the project.  This does mean that we have to give up on using any Lua meta-programming, but may be much easier to incoporate, since there are fewer runtime dependencies involved.

In this strategy, we execute a Lua/Terra Simplang script during the build process.  For instance, if we want to generate code for our hello42 function, we would use a build script like:

```
import 'simplang.simplang'
local SL = require 'simplang.simplib'

local simpl hello42() return 21 + 21 end

-- Save out the functions
SL.compiletofile("hello42.o","hello42.h",{ hello42 = hello42 })
```

This seems very much like our existing scripts, except we end it with a call to `SL.compiletofile(...)`.  This function call is passed the path for an object file to generate, path for a header file to generate (can be left `nil` to omit header file generation) and a table of Simplang functions you want to expose.

Running this script will produce a header file like the following

```
#ifndef _HELLO_42_H_
#define _HELLO_42_H_

#include "stdbool.h"
#include "stdint.h"


int32_t hello42();

#endif
```

Then, we can use this function from a C program like

```
#include <stdio.h>
#include "hello42.h"

int main(int argc, char ** argv) {
  int answer = hello42();
  printf("%d\n", answer);

  return 0;
}
```

If we compile this source together with the generated `hello42.o` object code, then we'll get a program that prints `42`.








