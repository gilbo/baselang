
// embedded_main.c
#include <stdio.h>
#include "terra/terra.h"

int main(int argc, char ** argv) {
  lua_State * L = luaL_newstate(); //create a plain lua state
  luaL_openlibs(L);                //initialize its libraries
  //initialize the terra state in lua
  terra_init(L);

  // we can execute terra scripts from within the C program now
  // First, let's extend the terrapath
  const char *pathextend =
  "package.terrapath = package.terrapath.."
  // extend the path so that we can run both in the example directory
  "';../release/?.t'.."
  // and in the directory one up which tests are run from
  "';release/?.t'";
  if(terra_dostring(L, pathextend)) {
    printf("pathextend failed\n");
    fprintf(stderr, "%s\n", lua_tostring(L, -1));
    lua_pop(L, 1);  // pop error message from the stack
    lua_close(L);
    return 1;
  }

  // then, we can go ahead and execute the hello42 program
  const char *scriptstring =
  "import 'tenslang.tenslang'\n"
  "\n"
  "tensl hello42() return 21 + 21 end\n"
  "\n"
  "print(hello42())\n"
  "assert(hello42() == 42)\n"
  "\n";
  if(terra_dostring(L, scriptstring)) {
    printf("script failed\n");
    fprintf(stderr, "%s\n", lua_tostring(L, -1));
    lua_pop(L, 1);  // pop error message from the stack
  }

  // finally, let's check to make sure that error reporting
  // is working ok
  const char *errscript = "error('causing an intentional error')";
  if(terra_dostring(L, errscript)) {
    fprintf(stderr, "%s\n", lua_tostring(L, -1));
    lua_pop(L, 1);  // pop error message from the stack
  }
  
  lua_close(L);
  return 0;
}