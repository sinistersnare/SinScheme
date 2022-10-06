

## immediate ##

* Port anf tests to proc for LLVM testing
* Test call/cc at low level!!!! FUCK!


## Medium term.

* WRITE A GARBAGE COLLECTOR LOL
* Allow datums in prim operations after anf? Not just symbols.
*   Could probably allow them in function calls too hmmm
*   Would reduce the # locals at least.
* Can we allow vararg lambdas? Right now, we are compiling everything into a 1-fixed-arg lambda, which shifts the cost of vararg lambdas onto fixed-arg lambdas. I think it would require a stack-slot to be added noting the # args passed, so we know dynamically where the locals start.
* Update documentation on compiler phases and primitives, and everything else...
	* More important now that we dont do CPS anymore... Docs are fairly stale!
* A way to auto-include LibGC without asking users to modify compiler.rkt?
* Would be nice to revamp the compiler interface.
	* Just default to scm->exe, keep flags the change. still require `-i` and `-o`.
* Rewrite runtime in Rust. Because Rust > C++, fite me.
	* Will require a Rust GC implementation.... FUN!
	* And possibly an extra step to integrate `cargo` instead of shelling out to clang++ in racket.
* Rewrite top-level ... ugh.
* Alter the `phases` test infra to do what it does now + compile it down to exe and run that too. There are a lot of tests for desugar that I would like to run on LLVM to see what happens.
