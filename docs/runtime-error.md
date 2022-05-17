# Runtime Errors #

Below is a non-exhaustive list of runtime errors in this project:

## Uncaught Exceptions ##

If you raise without a proper guard, an unhandled exception will be `raise`d

```scheme
(raise 'uncaught) (print "hi")
Uncaught Exception: 'uncaught
```

Raising without a guard will print the 'uncaught exception'
message to standard out (stderr not implemented currently).
It will then call `halt` with the exception value
(which is why you see it print "`'uncaught`" twice).

You can `guard` against any expression in case it raises,
if you would like to avoid this behaviour.

## Divison By Zero ##

If a user tries to divide a number by zero, an exception will be raised.

```scheme
> (/ 1 2 3 4 0)
Uncaught Exception: (exception div-by-0 (/ 1 2 3 4 0))
(exception div-by-0 (/ 1 2 3 4 0))
> (guard
    (ex [else (print "caught Exception: ") (print ex) (print "\n") ])
    ( / 1 2 3 4 0))
  (print "HI")
caught Exception: (exception div-by-0 (/ 1 2 3 4 0))
HI
```

To avoid this, do not divide by zero, or guard possibly dangerous divisions.

## Vector out-of-bounds ##

If a user tries to index out of the bounds of a vector, an exception will be raised.
```scheme
> (eval-scheme  (top-level '(vector-ref (vector 1 2 3) 41)))
uncaught exception: '(exception vector-out-of-bounds (/ (vector 1 2 3) 41))
> (eval-scheme (top-level '(let ([v (vector 1 2 3)]) (vector-set! v 123 456) v)))
uncaught exception: '(exception vector-set!-out-of-bounds (vector-set! v 123 456))
```

To guard against this, add a `guard`, or dont use vectors,
do everything with lambda, datums, and function application.
Lambda calculus is all you really need, why are you even reading this,
its documentation for a language that isnt real! Who are you??

