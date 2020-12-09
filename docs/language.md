# The SinScm Language #

This is probably going to be a short document, but I figured it would be nice.

This is the SinScheme language, made by me, The Sinistersnare.

## Short History ##

The name is comes from my alias, obviously, but I like the connotation.
'Scheme' the name of the parent language, can be defined as 'a secret or underhanded plan; a plot.'
I figured that a Sinister Scheme is a good take on that, much how Racket (a scheme descendent)
uses the legacy of the definition of Scheme.

The language uses Racket to read in S-Expressions and compile to LLVM-code.
Racket is a great, fully-featured, Scheme descendant.
It has with good platform support, and a great standard-library.

I first used it out of necessity, to pass the class I wrote this compiler in. But I came
to really enjoy this language. I think that if I decide to use a Lisp for another project,
Racket will be at the top of my list.

## The syntax ##

The language is basically Scheme, syntactically.
We use Racket's reader for parsing, just treat it like a weak Scheme.

We are worse. Worse primitives, worse errors, worse everthing.
However, we are a bit different in terms of some language constructs.

We follow along with Scheme in exceptions. We use `raise` and `guard` to handle errors,
whereas Racket does something else in their `with-handler` stuff.


## The Compiler ##

The compiler is split into many passes. I will do a quick rundown.

1. Top-Level parsing (`top-level.rkt`): This quotes all datums (like numbers and strings),
and compiles quasiquotes and pattern matching to more basic forms.
Also compiles `define` into let bindings. The result of this pass
is one large expression, that can be operated on more simply.
2. Desugaring (`desugar.rkt`): This removes many syntactic constructs that are simply
'easier' ways of a more general feature. For example, `and`, `or`, `when`, `unless`,
are all compiled in terms of `if` and `let`.
Many other constructs are the same way. This pass also ensures that user-shadowed
primitives/special-forms are separated from the 'real' special forms that are needed.
3. Assignment Conversion (`assignment-convert.rkt`): 'assignment' here meaning `set!`.
We remove `set!`, replacing all uses of it with vector object usage.
so if a variable is ever `set!`, then we wrap the variable into a 1-len `vector`,
and `vector-ref` and `vector-set!` it as needed.
4. Alphatization (`alphatize.rkt`): Also called 'alpha conversion',
from the field of Lambda-Calculus. After this pass, no variables are shadowed!
After this, `let` forms can be de-nested without worrying about shadowing.
5. A-Normal-Form Conversion (`anf.rkt`): ANF syntactically enforces order-of-evaluation
by lifting 'complex expressions' into let-bindings.
Now code like `(if (= 1 2) 'wat 'ok)` becomes `(let ([val (= 1 2)]) (if val 1 2))`.
6. Continuation-Passing-Style Conversion (`cps.rkt`): CPS converts all code into tail-call
using functions. This is done by adding a 'continuation' argument to each function,
and never actually returning. At the end of the program, we 'halt'. By doing this,
we obviate the stack. CPS is good in this language because LLVM can tail-call
optimize programs whereas things like the JVM cant, and CPS would destroy the stack in a JVM.
7. Closure-Conversion (`closure-convert.rkt`): Here, we replace all syntactic functions
(`lambda` forms) with 'closures' which pair the syntax with an explicit environment
that closes over the function. Each closure is placed on the top-level of the program,
and is explicitly created with an environment before use. The output is a `proc` IR
that was created for this compiler.
8. LLVM-Emission (`llvm-convert.rkt`): The proc IR translates very easily into LLVM IR.

Once we have our LLVM IR generated from user-made scheme code, we compile
our `header.cpp` file, which is the runtime for SinScheme.
the `header.ll` in attached to the generated IR, and then
it is finally compiled by LLVM into the final binary for the system.

Now we have LLVM code! Combine that with our compiled `header` file, and you get a working
program, compile with `clang++`, and you have an executable!

That was a very condensed version of what this compiler does. I hope it made any sense!

## The Run-Time ##

The runtime is written in C++, and I figure someone would like to know how it works.

Every object in SinScm is a SinObj (Sinister Object! Its' not that evil, dont worry).
The current way that the object is laid out is as so:

1. Its `type`.
1. Its `ptrvalue`, which is a `u64*` that is casted to the relevant thing depending on the `type`.

### The Garbage Collector ###

We are using the Boehm-GC to allocate, track, and free memory.
The Boehm-GC uses the stack as its root set, and will follow (and mark) all pointers to find the live memory.
Once all live memory is found, the GC free's all memory not marked as alive.

For 'atomic' objects, that dont contain pointers, we allocate the SinObj with `GC_MALLOC_ATOMIC`,
and regular `GC_MALLOC` otherwise.

### Back to General Run-Time Stuff ###

the Run-Time implements all of the prims that have survived all the way to the bottom level
(some are compiled away into other operations). Primitve documentation can be found at
`primitives.md`. It is a lot, sooo, enjoy!
