# The SinScm Language #

This is probably going to be a short document, but I figured it would be nice.

This is the Sin Scheme language, made by me, The Sinistersnare.

## Short History ##

The name is comes from my alias, obviously, but I like the connotation.
'Scheme' the name of the parent language, can be defined as 'a secret or underhanded plan; a plot.'
I figured that a Sinister Scheme is a good take off of that, much how Racket (a scheme descendent)
uses the legacy of the definition of Scheme.

The language uses Racket to read in S-Expressions and compile to LLVM-code.
Racket is a great, fully-featured, Scheme, with good support, and a good standard-library.
I chose to use it out of necessity, to pass the class I wrote this compiler in, but I came
to really enjoy this language. I think that if I decide to use a Lisp for another project,
Racket will be at the top of my list.

## The syntax ##

The language is basically Racket, syntactically, due to use using its Reader for parsing.
However, there is a different philosophy behind this language.

We are worse. Worse primitives, worse errors, worse everthing.
However, we are a bit different in terms of some language constructs.
We follow along with Scheme in exceptions. We use `raise` and `guard` to handle errors,
whereas Racket does something else (who knows really).


## The Compiler ##

The compiler is split into many passes. I will do a quick rundown.

1. Top-Level parsing. This quotes all datums (like numbers and strings),
and compiles quasiquotes and pattern matching to more basic forms.
Also compiles `define` into let bindings.
2. Desugaring. This will remove many syntactic constructs that can be implemented
in terms of other, simpler, constructs
This include the non `let` letforms. `and`, `or`, `raise`, promises, and many other constructs.
3. Assignment-Conversion. This pass removes `set!`, which is necessary for future passes/
4. Alphatization. This pass removes all shadowing, so each variable is uniquely named.
After this pass, we can de-nest all `let` forms without worry about shadowing issues.
5. Administrative-Normal-Form: This pass lifts all complex expressions into `let`-bindings.
Now code like `(if (= 1 2) 'wat 'ok)` becomes `(let ([val (= 1 2)]) (if val 1 2))`
By lifting all complex expressions, we create an explicit order of evaluation in the code.
6. Continuation-Passing-Style: This converts the ANF-code into a form that will never return.
All functions are converted into tail-calls that call other tail-call functions.
At the end of the chain, we `halt` with the final value of the program.
CPS is good in this language because LLVM can tail-call optimize programs
whereas things like the JVM cant, and CPS would destroy the stack in a JVM.
7. Closure-Conversion. We finally venture away from Lisp here! We turn the language into
a 'proc' language, which is still somewhat-lispy, but much more imperative.
We lift all lambdas to the top-level, and provide explicit environments for
free-variables to functions.
8. LLVM-Emission: Here, we perform a translation from our proc-language to LLVM.
The proc language is very nice here, as it is much easier to translate to the
imperative LLVM-IR than a more lispy syntax would be.
This pass is aided by the runtime, which resides at `src/cpp/header.cpp`
(probably, unless I change the name to '`prelude.cpp`' or something and forget to update this).

Now we have LLVM code! Combine that with our compiled `header` file, and you get a working
program, compile with `clang++`, and you have an executable!

That was a very condensed version of what this compiler does. I hope it made any sense!

## The Run-Time ##

The runtime is written in C++, and I figure someone would like to know how it works.

Every object in SinScm is a SinObj (Sinister Object!), its not that evil, dont worry.
The current way that the object is laid out is as so:

1. The `type` (SinType). This is any type that can be used in SinScheme.
From Closures to Vectors to Ints. It is a simple Enum.
2. A `value`. This is the storage for any atomic type that can be owned by SinScheme.
3. a `ptrvalue`, which exists because garbage collection is hard. letforms talk about that.

### The Garbage Collector ###

We are using the Boehm-GC to allocate, track, and free memory.
The Boehm-GC uses the stack as its root set, and will follow (and mark) all pointers to find the live memory.
Once all live memory is found, the GC free's all memory not marked as alive.

The issue is that, if we converted all pointers to regular atomic values, the GC would not be able
to follow any pointers, and would free things that are stil alive in the program!

I have a solution planned for the future, I will lay it out here:

Only have a `ptrvalue` member of the SinObj. Convert all atomic values to pointers when stored,
And allocate them with `GC_MALLOC_ATOMIC`, so the GC does not try to follow them
(they dont need to be followed). When we need to use them, convert them back to atomics, and use them!
This way, we save 8-bytes of memory not storing a useless part of the struct for every object.
Also, we wouldnt need to use ugly-`union`s or `std::variant`. This solution _may_ work, but
I am not completely sure, so I have not implemented it, citing time constraints.

### Back to General Run-Time Stuff ###

the Run-Time implements all of the prims that have survived all the way to the bottom level
(some are compiled away into other operations). Primitve documentation can be found at `primitives.md`.
It is a lot, sooo, enjoy!
