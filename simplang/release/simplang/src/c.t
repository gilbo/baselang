

local c_blob = terralib.includecstring [[
#include "stdlib.h"
#include "stdio.h"

FILE * __get__stdout() { return stdout; }
FILE * __get__stdin()  { return stdin; }
FILE * __get__stderr() { return stderr; }

]]

rawset(c_blob, 'stdout', c_blob.__get__stdout())
rawset(c_blob, 'stdin',  c_blob.__get__stdin())
rawset(c_blob, 'stderr', c_blob.__get__stderr())

package.loaded["simplang.src.c"] = c_blob
return c_blob
