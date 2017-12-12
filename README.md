## Sin's Scheme Compiler ##

Hi! This is a scheme compiler that I wrote for my Compilers Class:
[CMSC 430](https://www.cs.umd.edu/class/fall2017/cmsc430/)

This is a mostly working compiler from some non-standards compliant Scheme to LLVM IR.
For a comprehensive idea on how this works (from scheme to llvm ir),
I would recommend you check the class webpage, or ask me to write a blog-post,
becuase that sounds like a fun post idea!

## Running The Compiler ##

To compile a file `hello.sinscm` to some file `output.ll`:

`$ racket sinscm.rkt -i hello.sinscm -o output.ll`

The sinscm.rkt utility exists to create the LLVM IR code for you

To compile that to an executable, you can use clang as such:

`$ clang++ -o out.exe output.ll`

Now you have an executable that you should be able to run:

`$ ./out.exe`

Enjoy!

We also have a tests.rkt file, you can interact with as so:

`$ racket tests.rkt ` will tell you the tests that you can run.
`$ racket tests.rkt TESTNAME` will run the test TESTNAME
`$ racket tests.rkt all` will run all tests.

There is currently something going on with the order tests,
making it consume about a Gig of memory for some reason,
it also takes forever to run. Good luck with that.

If you would like to add a test, please add one to the `tests/` directory.
Test files end with `.scm` or `.sinscm`.


## Documentation ##

There is docs in the `docs/` folder,
describing the primitive functions that this implementation implements.
It also discusses other things such as language forms,
and how to form a correct Sin Scheme program.


## TODO: ##

`$ clang++ main.cpp /user/local/lib/libgc.a -pthread -o main`

Implement a class SchemeKey or something that checks the types tag and hashes a key.
Also must have a deep equality check.



This project doubles as a final project for the class,
but it has since been open sourced with permission from my professor
(see Extra Credit section
[here](https://www.cs.umd.edu/class/fall2017/cmsc430/final.html)).
Becuase this doubles as a project I must add this line:

> I, Davis Silverman, pledge on my honor that I have not given or received any
unauthorized assistance on this assignment.

## License ##

This project is currently unlicensed.

I did not write 100% of the code for this project,
some files are provided by my professor
(each file has a note describing who wrote it).
Because of this, I am not comfortable licensing this how I would want it.

There are better Scheme ecompilers anyways! Go use them instead!



## Other Libraries ##

This project uses external libraries listed here with their licenses:


