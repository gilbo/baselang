This file provides a getting started guide for developers looking to fork one of the Baselang libraries for use as skeleton code.  It provides an overview of the language collection, and some advice on how to effectively develop the languages contained in it.

# Collection of Languages

The Baselang collection consists of 4 languages designed to articulate
a small space of language designs along two dimensions.  In all cases, the languages omit control flow and recursive functions, making them suitable only for expressing straight-line code.  Nonetheless, they include a full complement of tests, coverage analysis, interoperability examples and documentation.  This tradeoff makes the Baselang languages suitable for quick forking and development into more sophisticated languages--complete with enough infrastructure to grow without collapsing under their own weight.

The four languages form a lattice, with Simplang being the least feature rich, and Plexlang the most.  The two dimensions of variation are (1) the extension from a unified number type to a richer collection of integers and floating point types, and (2) the addition of a tensor type constructor, along with special tensor expressions.

Because these changes to the type system are relatively _cross-cutting_, they hopefully give a good example of how to elaborate on the basic architecture.  Both features also involve a non-trivial amount of code, so if richer types are needed, you can fork a more sophisticated language to save yourself some development time and trouble.

* Simplang - completely stripped down language
* Typelang - language with more sophisticated primitive type system
* Tenslang - language with more sophisticated tensor type system
* Plexlang - language with both tensors and rich primitive types

## Forking a language

To fork a language, recursively copy any of the four language sub-directories.  Once you've done this, you'll need to do a cross-cutting refactoring of the copied code base.  Everywhere that the previous language name occurs needs to be searched for and substituted with your new language name.  Here's a quick guide to helping you make these substitutions.

In the following we use `Xxxxlang` as a shorthand for `Simplang`, `Typelang`, `Tenslang`, and `Plexlang` respectively.  We use `xxxxl` as shorthand for `simpl`, `typel`, `tensl` and `plexl` respectively.

* rename files and directories: `bin/xxxxl`, `release/xxxxlang`, `release/xxxxlang/xxxxlang.t`, and `release/xxxxlang/xxxxlib.t`

When searching and replacing in files, you'll have to find and replace occurrences of `xxxxl` and `xxxxlang`.  Less obviously, you may want to find all uses of `XL` (`SL`, `TL`, or `PL` respectively) and replace them with your choice of convention for the standard library.  You can find these by searching for `local XL` or `XL.` to reduce false positives

* search and replace within text files: `bin/xxxxl`, `runtests`, `covanalysis`

* All the files in `example_c/` except `static_call_main.c`

* All the files in `example/` and `tests/`

* everything in the `release/` directory.

* search and replace in documentation files: `README.md`, `example_c.md`, `docs/buildhtml`, `docs/manual.md`.  Note that `docs/manual.html` can be regenerated from the markdown file using Pandoc, so editing it manually is not necessary.

### Why is this all manual?

It doesn't hurt to take at least one quick look over everything, though I'm open to someone writing a script to handle this step.





# Getting Started (as a Developer)

When you get started working with any of these languages, I recommend you go down the following checklist to get your bearings.

1. Make sure you can run all the tests and that they're all passing.
2. Make sure you know how to modify and re-build the documentation.
3. Add a simple, boring feature to the language as your first task


## Testing

To run the tests for a langauge, you simply need to run the script
```./runtests```
This is a good way to check for anything you missed during refactoring.

The compilers come with a number of tests already written.  However, I  recommend that you actively add new tests with each new feature or major change, and actively maintain the existing tests.

When writing new tests, you can include `tests/test.lua` to get access to a shared set of utility functions for writing tests.  You can also add the comment `--DISABLE-TEST` as the first line of any test to tell the `runtests` script to skip it.  This way you can disable tests you want to delay fixing rather than deleting them altogether.

### Coverage Testing

While tests are always valuable, they're especially necessary here since these compilers are written in Lua, an interpreted, dynamically typed language.  Without a good set of tests, it's easy to accumulate lots of unexecuted, unreliable code in the compiler.

To help manage this issue, we provide a test coverage analysis system integrated into the compiler.  This coverage analysis uses Lua's `debug` facilities, which makes it very simple, if slightly imperfect.  To turn on coverage analysis, uncomment the line
```require 'xxxxlang.src.coverage'```
near the top of the language file `release/xxxxlang/xxxxlang.t`.  After uncommenting the line, run the test suite and make sure to recomment the line.

Running tests with coverage enabled will generate a `coverage_src` directory with copies of all the files from `release/xxxxlang/src/`, with line counts prepended to every line.  Non-trivial lines that have a 0 count are marked with hashes `#####:` to indicate that the line is not currently being covered.  Some of these lines won't be covered for various acceptable reasons. (i.e. they're lines of terra code or represent sanity check assertions that should be impossible to trigger.)

If you're interested in tweaking the coverage analysis, all the important details of collecting the coverage data can be found in `release/xxxxlang/src/coverage.t` and all of the details of how the report files are generated can be found in the `covanalysis` script.  In particular, the `covanalysis` script defines a set of line patterns which are ignored as non-executable, regardless of what their line count is.

You can find a number of discussions online about the merits and dangers of coverage metrics.  While high coverage will not ensure that the test suite is of good quality, it is very useful for ensuring that code is executed at least once.  In a dynamic language like Lua, this can help eliminate a range of otherwise trivial bugs.





## Documentation and Example Code

Documentation files exist in 3 places.  The main source of documentation is `docs/manual.md`, which gives a detailed description of the language and its API.  Second, there's a `README.md`, which should help a new user get up and running as quickly as possible, along with directing them towards the full manual.  This `README.md` file will likely be the first documentation a new user encounters, so it's important for it to remain relatively brief and to the point.  Lastly, there are special instructions at `example_c/README.md` that provide more detailed information on how the language can be compiled into C code.  If you choose to stop supporting this functionality, make sure to remove the `example_c` directory.

The main `docs/manual.md` file is also available in HTML format.  In order to keep these in sync, we provide a `docs/buildhtml` script that constructs the HTML manual automatically from the Markdown document.  In order to do so, you need to install the Pandoc utility.

Besides the documentation, new users are likely to turn to your example code to get a sense of how to use the langauge.  For this reason it's especially important that you keep your example code (in `examples/`) up to date, working, and that you make sure the code is representative of the Language's intended uses.  To help with this, notice that we can create test files like `tests/isct_predicates.t` in order to invoke examples as part of our test suite.  This is the most reliable way to make sure example code remains up-to-date.



## Adding a basic feature to the language

These languages were intentionally left without some obvious, useful features.  One benefit of this choice is that there are plenty of small projects new developers can do to learn the code base better.  A full list of such projects is provided at the end of this document.  We repeat a few of the easier "first-steps" style projects here.

* Let expressions
  - There's a bunch of machinery in the compiler for let expressions, but
    they're not exposed as a user-accessible bit of syntax.  Expose them,
    but then also make sure that they're working correctly by writing
    appropriate tests.  What kind of tests do you need to write?

* Control Flow
  - Add in basic while-loops and if-then-else statements
  - Add in for-loops.  How should they work?
  - Add in repeat-while loops

* Type coercions
  - Add in the uint32 and int64 types for completeness.  How should
    type coercions work?  Should any of the existing coercions be
    removed?


# Deployment and Distribution

The cardinal rule of software distribution is that you should do almost anything in your power to make it easier for more people to reliably, quickly and easily get started using your code.  The README files, examples, tests, and scripts included with these languages are all intended to help with this issue.

However, the current documentation is largely written assuming some familiarity with Lua/Terra.  If you're developing your language for wide deployment to people who don't care that much about programming in Lua/Terra, then it might be worth considering how to substantially lower the barrier to getting set up in the first place.  For instance, you might want to package the entire language, along with a Lua/Terra environment as a single C-library or pair of C-libraries.  As an example, this would be a good idea if you use Lua/Terra to implement a SQL client or database.



# Compiler Architecture and Design

Go through some kind of a walk-through here of how things are set up.















# Wish List

While I tried to cover most of the major issues with developing and deploying large-ish Lua/Terra DSLs here, there are a few topics that are noticably absent.

At the time, my biggest regret is that these example languages do not include much in the way of explicit, carefully considered support for debugging.



# Suggested Project List


- extend the multiple-return-value/assignment feature:
  - multi-variable declaration statements would generally be useful.
    It would also be nice as a way to capture multiple return values
    from a function call.
    Design Q: Where should type annotations go?
    Design Q: Should you be able to capture some return values with
      an existing variable and some with new variable names?
      Again, think about balancing simplicity with utility.
  - consider the following
      local baselang retpair() return 0,1 end
      local baselang foo() return retpair() end
    It doesn't work right now, because the compiler tries to
    create a tuple of a tuple to return.  Can you fix this problem?
    Design Q: In what cases should you try to fix this problem?
      Can you balance simplicity with utility here?
- Type coercions
  - Add in the uint32 and int64 types for completeness.  How should
    type coercions work?  Should any of the existing coercions be
    removed?
- Control Flow
  - Add in basic while-loops and if-then-else statements
  - Add in for-loops.  How should they work?
  - Add in repeat-while loops
- Recursive Functions
  - Try adding in only self-recursive (rather than mutually recursive)
    funtions first.  It's a bit simpler
  - Can you add mutually recursive functions?
- Let expressions
  - There's a bunch of machinery in the compiler for let expressions, but
    they're not exposed as a user-accessible bit of syntax.  Expose them,
    but then also make sure that they're working correctly by writing
    appropriate tests.  What kind of tests do you need to write?
- Coverage
  - Turn on the code coverage analysis and see whether or not the tests
    are covering everything.  Probably not.  Can you use this information
    to improve the test suite?  Are there any bugs you found?
- New IR
  - The AST format has a lot of different node types, some of which
    are probably unnecessary.  Notice that after typechecking, we should
    have enough local information to flatten out the code such that
    blocking is unnecessary beyond control flow (if we added that).
    Specifically, notice that variable scoping/shadowing has been resolved
    directly into symbol names now.  Can you translate to a form that
    looks a little bit closer to SSA? (It can still be an AST, but try
    to heavily reduce the number of different AST kinds that are used)
- Generic/Polymorphic Functions
  - One approach to polymorphic functions is to simply delay typechecking
    until the function is used, at which point a variant of the function
    can be dispatched based on the supplied values.  This is relatively
    simple of a strategy in the current compiler infrastructure, and is
    recommended heavily.
  - Another approach is to include explicit polymorphism in the typesystem.
    This is the more traditional/academic way a programming language might
    handle things.  I recommend against it, but it might be interesting
    to work through if you want to kill some time.  I highly recommend
    some familiarity with System F or a comparable typesystem if you don't
    want to make a total mess of this.
- Structured Data
  - Add a type for structs of data.  How should structs be declared?
    How should data be accessed from within functions.  How should
    structured data be constructed within a function?
  - Consider adding full Algebraic Data Types in the ML/Haskell style.
    Would these be useful to you?  Is pattern matching a
    particularly helpful feature to add?


