# Runtime Errors #

Below is a non-exhaustive list of runtime errors in this project:

## Uncaught Exceptions ##

If you raise without a proper guard, an unhandled exception will be thrown

```scheme
(raise 'uncaught) (print "hi")
Uncaught Exception: 'uncaught
'uncaught
```

## Divison By Zero ##

If a user tries to divide a number by zero, a fatal runtime error will occur

```scheme
> (/ 1 2 3 4 0)
Uncaught Exception: '(exception . (div-by-0 . ((/ . (1 . (2 . (3 . (4 . (0 . ())))))) . ())))
'(exception . (div-by-0 . ((/ . (1 . (2 . (3 . (4 . (0 . ())))))) . ())))
> (guard
    (ex [else (print "got Exception: ") (print ex) (print "\n") ])
    ( / 1 2 3 4 0))
  (print "HI")
got Exception: '(exception . (div-by-0 . ((/ . (1 . (2 . (3 . (4 . (0 . ())))))) . ())))
HI
```

To avoid this, do not divide by zero, or guard possibly dangerous divisions.
