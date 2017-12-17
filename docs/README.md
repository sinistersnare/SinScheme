# Sin Scheme Documentation #

Here is the documentation in this directory:

## language.md ##

This is a quick tour of the language, including a bit on the syntax, the runtime, and some history
(but its more like current events).

## primitives.md ##

This is a list of all primitives the language supports without needing to import another library.
And because its impossible to import other libraries, you should be happy you have them!

The good thing about the list is the comprehensive examples, unlike the racket documentation(!),
but it lacks a formal syntax definition (it doesnt say `+ : Int ... -> Int`).
But hopefully the example code is enough

## runtime-error.md ##

This lists some runtime-errors that can be avoided by using this language,
as opposed to some other languages created in the compilers class I wrote this in.

These features include divison-by-0 handling, unhandled exception handling,
and vector-out-of-bounds checking. Hopefully 2 more, but we will see.
