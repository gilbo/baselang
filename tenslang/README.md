
# Tenslang

Tenslang is a demonstration language using the Lua/Terra compiler
infrastructure.

## Quick Setup

Add the [`bin/`](bin) directory to your `PATH` environment variable, so that the [`bin/tensl`](bin/tensl) script is accessible.  To run a Tenslang script named `hello42.t`, just execute, 
```tensl hello42.t```
Here's a listing for `hello42.t` that you can try out.  It should print `42` to the console.
```
import 'tenslang.tenslang'

local tensl getanswer() return 21 + 21 end

print(getanswer())
```

## More Details

See the [full manual](docs/manual.md) for more information.

## Examples

See the [examples](examples) directory for example uses of Tenslang.  This is a good way to get a few ideas about how to proceed once you've got some code running.

## Tests

You can run the testing suite by executing
```./runtests```
