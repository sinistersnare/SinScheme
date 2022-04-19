# Compiler Layout #

## TODO: layout is different with CPS!! ##

The SinScheme compiler goes through many phases of transformations,
which are detailed here.

There is a provided `compiler.rkt`
that runs all of the following phases in order.
Taking in SinScheme code, and returning LLVM IR.

1. Top-level Transformations (`top-level.rkt`) (compiles quasiquotes, pattern matching, `begin`)
2. Desugaring (`desugar.rkt`) (no extraneous forms like `and`, `letrec`, `raise`)
3. Alphatization (`cps.rkt`) (no more variable name shadowing)
4. ANF-Conversion (`cps.rkt`) (enforces an evaluation order explicitly)
4. CPS Conversion (`cps.rkt`) (no more returning, only tail calls.)
5. Closure Conversion (`cps-closure-convert.rkt`) (emits a C-like/Scheme-like language for ez compilation)
6. LLVM IR Emission (`closure-convert.rkt`) (LLVM will handle compilation to machine code)

I would love to detail all of these,
maybe when I am not studying for all the
classes that I am studying for :)
