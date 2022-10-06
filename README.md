# Sin's Scheme Compiler #

Hi! This is a scheme compiler that I originally wrote for my undergrad
Compilers Class: [CMSC 430](https://www.cs.umd.edu/class/fall2017/cmsc430/)
and then kept working on!

This is a mostly working compiler from some non-standards compliant Scheme to LLVM IR.
For a comprehensive idea on how this works (from scheme to llvm ir),
I would recommend you check the class webpage, or ask me to write a blog-post,
becuase that sounds like a fun post idea!

## Installing Dependencies ##

Start by installing the threading library in Racket:

```bash
$ raco pkg install threading
```

## Running The Compiler ##

(right now, we dont clear the `build/` folder, so make sure to clear it out once in a while...)

To compile a file `hello.sinscm` to some file `output.ll`:

`$ racket sinscm.rkt -i hello.sinscm -o output.ll -j scm -t llvm`

This .ll file will be the combination of the header and the scheme code.
So if you want to store the combined LLVM file, and compile it later,
use this option.

To compile that output file to an executable:

`$ racket sinscm.rkt -i output.ll -o hello.x -j llvm -t exe`

***Or*** you can combine the two steps:

`$ racket sinscm.rkt -i hello.sinscm -o hello.x -j scm -t exe`

(the CLI args are a bit wonky-named, sorry...)

Anyways, you can run the executable as with any:

`$ ./hello.x`

Enjoy!


## Testing ##

We also have a tests.rkt file, you can interact with as so:


`$ racket tests.rkt` will tell you the tests that you can run.

`$ racket tests.rkt TESTNAME` will run the test TESTNAME

`$ racket tests.rkt all` will run all tests.

`$ racket tests.rkt <phase>` will run tests for a specific phase.

There is currently something going on with the order tests,
making it consume about a Gig of memory for some reason,
it also takes forever to run. Good luck with that.

To add a passing test, add it to `tests/passing`.
This test must have the same output value when compiled vs interpreted.

To add a failing test, add it to `tests/failing`.
This test must raise in both compile and interpret mode.

Make sure that the test file-name ends in either `.scm` or `.sinscm`!

## Documentation ##

There is docs in the `docs/` folder,
currently there is a `runtime-error.md`, `primitives.md`, and a `language.md`,

`runtime-error.md` documents a subset of the runtime errors
that this language will give you.

`primitives.md` documents each primitive that this language supports.
It also provides documentation for the `Hash` object implemented in `docs/cpp/hash.h`

`language.md` provides a general guide of the language.

The documentation is hopefully good, and will provide examples of use and
some edge cases.

If you would like to contribute documentation feel free :)

I am pretty sure this only works on 64bit machines, so dont try on 32bit.

This was completed as a final project for my undergraduate compilers course.
[You can find the page here]([https://www.cs.umd.edu/class/fall2017/cmsc430/).
It was a really fun class, you should take take a look at it!

## License ##

This Compiler is licensed with the MIT license, so please feel free to use this,
and modify the code. If you do use this, please say hi :)

We currently use the Boehm-GC, and code by Thomas Gilray,
(particularly `utils.rkt` and `desugar.rkt`). I have provided license information in `LICENSE.md`


### Custom LLVM ###

***This is not needed! No Custom LLVM needed!!!***

But Im gonna leave this here in case I ever need to compile a custom LLVM :)

```
$ cd ~/code/llvm-project/
cmake -S llvm -B build -G Ninja -DLLVM_USE_SPLIT_DWARF=On -DLLVM_USE_LINKER=lld -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_BUILD_TYPE=Debug -DLLVM_APPEND_VC_REV=Off '-DCMAKE_CXX_FLAGS_DEBUG=-g -O1' '-DCMAKE_CXX_FLAGS_DEBUG=-g -O1' -DLLVM_ENABLE_PROJECTS=clang
cmake --build build
```


