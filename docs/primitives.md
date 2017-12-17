# Primitives #

Here is a list of supported primitives by Sin Scheme.

To test them, you can run the following command-line:

`racket sinscm.rkt -e "TEST-CODE-HERE" -o res.x -t exe -j scm && ./res.x`

where TEST-CODE-HERE is something like `(= 1 1)`,
or any of the other examples shown here.




## equality ##

### = ###

`=` Tests equality strictly on numbers:

```scheme
(= 1 1) ; ==> #t
(= 1 2) ; ==> #f
(= '(1 2 3) '(1 2 3)) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__61 a
```

Currently not the prettiest error message, and it cant be guarded against at run-time, but, hey, dont do it!


### not ###

Negates a given value. The only 'falsy' value is #f, so giving #f will return #t,
Anything else will return #f.

```scheme
(not #f) ; ==> #t
(not #t) ; ==> #f
(not 'notabool) ; ==> #f
```

### eq? eqv? equal? ###

These functions are general equality testing functions.

```scheme
(eq? 1 1) ; ==> #t
(eq? 1 2) ; ==> #f
(eq? '(1 2 3) '(1 2 3)) ; ==> #t
(eq? (lambda (x) x) (lambda (x) x)) ; ==> #f
```

The functions currently all have the same logic, but they can compare objects of type:

integer, bools, void, null, str, symbol, vector, and cons.

Trying to compare functions will always result in #f

### > ###

A greater than function

```scheme
(> 1 2) ; ==> #f
(> 2 1) ; ==> #t
(2 . > . 1) ; ==> #t
(> '(1 2 3) '(0 0 0)) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__60_61 a
```

Only integers can be compared here, or an unguardable error will be thrown at you.

### >= ###

A greater than OR equal to function

```scheme
(>= 1 2) ; ==> #f
(>= 2 2) ; ==> #t
(>= 2 1) ; ==> #t
(2 . >= . 1) ; ==> #t
(>= '(1 2 3) '(0 0 0)) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__60 a
```

Only integers can be compared here, or an unguardable error will be thrown at you.


### < ###

A less than function

```scheme
(< 1 2) ; ==> #t
(< 2 1) ; ==> #f
(1 . < . 2) ; ==> #t
(> '(1 2 3) '(0 0 0)) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__60 a
```

Only integers can be compared here, or an unguardable error will be thrown at you.


### <= ###

A less than OR equal to function

```scheme
(<= 1 2) ; ==> #t
(<= 2 2) ; ==> #t
(<= 2 1) ; ==> #f
(1 . <= . 2) ; ==> #t
(<= '(1 2 3) '(0 0 0)) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__60_61 a
```

Only integers can be compared here, or an unguardable error will be thrown at you.


## MATH ##

### + ###

An addition operation, that takes any amount of integer arguments.

```scheme
(+ 1 2) ; ==> 3
(+) ; ==> 0
(+ 1) ; ==> 1
(+ 1 2 '(3 4 5)) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__43 b
```

Only integers can be added,
trying to add non-integers will result in an unguardable error being thrown at you.

### - ###

A subtraction operation that takes at least 1 integer argument.

```scheme
(- 1) ; ==> -1
(- 1 2) ; ==> -1
(- 1 2 0) ; ==> -1
(- 1 2 3 4 5) ; ==> -13
(- 5 4 3 2 1) ; ==> -5
(-) ; Will fail to compile.
(- #t '(1 2 3)) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__45 a
```

Only integers can be subtracted.
Trying to subtract non-integers will result in an unguardable error being thrown at your face.

### * ###

```scheme
(*) ; ==> 1
(* 12) ; ==> 12
(* 2 3) ; ==> 6
(* 2 #t) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__42 b
```

### / ###


```scheme
(/) ; ==> Uncaught Exception: (div-takes-at-least-1-arg)
(guard (x [else 'got!]) (/)) ; ==> 'got!
(/ 1) ; ==> 1
(/ 5) ; ==> 0 (note that floats are not implemented this is 1 / 5, so 0).
(/ 6 2) ; ==> 3
(/ 6 5) ; ==> 1
(/ 2 #t) ; ==> Fatal library run-time error: unwrap_int takes an Int object! in fn prim__47 b
```


## Other Prediates ##

### void? ###

Tests whether an argument is of type Void.

```scheme
(define x (display ""))
(void? x) ; ==> #t
```

### promise? ###

Tests whether an argument is a promise object.
TOTEST
```scheme
(define p (delay 5))
(promise? p) ; ==> #t
(promise 123) ; ==> #f
```

### procedure? ###

Tests whether an argument is a procedure, and can be called.
TOTEST
```scheme
(define (x) (+ x 1))
(procedure? x) ; ==> #t
(procedure? (lambda (x) x)) ; ==> #t
```

### integer? ###

Tests whether an argument is an integer, and can be treated as a number for math and other operations.

```scheme
(integer? 5) ; ==> #t
(integer? 'nope-avi) ; ==> #f
```


### number? ###

Because integers are the only numeric types implemented, number? is the same as integer?

Please see that documentation.


## Lists ##

LISP stands for LISt-Processing, so here we are I guess:

### cons ###

Takes two arguments, and creates a pair out of them.
If the second argument is a list, it will add the first argument to the start of the second argument.
If the second argument is not a list, then it will be treated as a regular 'pair' cons object.

```scheme
(cons 1 2) ; ==> '(1 . 2)
(cons (cons 1 2) 3) ; ==> '((1 . 2) . 3)
(cons 1 (cons 2 (cons 3 '())) ; ==> '(1 2 3)
(cons 1 2 3) ; ==> will not compile.
```

Note the great formatting, as opposed to the way that the print functionality ***used*** to print.

### cons? ###

Tests whether a given argument is of type Cons. Returns a Bool.

```scheme
(cons? '(1 2 3)) ; ==> #t
(cons? '()) ; ==> #f
(cons? 1) ; ==> #f
```

### list? ###

Tests if a variable is a null-terminated cons list.

```scheme
(list? '(1 2 3)) ; ==> #t
(list? '()) ; ==> #t
(list? (cons 1 2)) ; ==> #f
(list 1) ; ==> #f
```

### null? ###

Tests whether the given argument is of type Null. Returns a Bool

```scheme
(null? (cons 1 2)) ; ==> #f
(null? '()) ; ==> #t
(null? 5) ; ==> #f
(null? (cdr '(5))) ; ==> #t
(null?) ; will not compile.
```

Takes any type, and only returns #t when given a null value ('()).

### car ###

No, will not return a vehicle, but it will give you the first element of a given Cons.

```scheme
(car '(1 2 3)) ; ==> 1
(car (cons 1 2)) ; ==> 1
(car (cons 1 2 3)) ; ==> 1
(car (cons (cons 1 2) (cons 3 4))) ; ==> '(1 . 2)
(car 3) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_car
```

### cdr ###

Companion to `car`, that returns the second element of a Cons.
Remember that a Cons only has 2 parts, and the second part 'can' be a list, or anything else.

So if the second element of a cons is a list, it will return the list, not the first element of the list.

```scheme
(cdr '(1 2 3)) ; ==> '(2 3)
(cdr (cons 1 2)) ; ==> 2
(cdr (cons 1 (cons 2 3))) ; ==> '(2 . 3)
(cdr (cdr (cons 1 (cons 2 3)))) ; ==> 3
(cdr 4) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_cdr
```



### list ###

Takes a variable amount of arguments, and returns a null-terminated
Cons object that is a linked list of the arguments.

```scheme
(list 1 2 3) ; ==> '(1 2 3)
(list) ; ==> '()
(list (list 1 2) (list 3 4)) ; ==> '((1 2) (3 4))
```

Good luck getting this to fail.

### first ###

Takes an object that satisfies `list?` with at least 1 value, returning the first value.

```scheme
(first '(1 2 3)) ; ==> 1
(first '(1 2)) ; ==> 1
(first (cons 1 2 3)) ; ==> 1
(first (cons (cons 1 2) (cons 3 4))) ; ==> '(1 . 2)
(first 3) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_car
```

### second ###

Takes an object that satisfies `list?` with at least 2 values, returning the second value.

```scheme
(second '(1 2 3)) ; ==> 2
(second (cons  1 (cons 2 '()))) ; ==> 2
(second (cons 1 2)) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_car
```

### third ###

Takes an object that satisfies `list?` with at least 3 values, returning the third value.

```scheme
(third '(1 2 3)) ; ==> 3
(third '(1 2 3 4 5)) ; ==> 3
(third (cons 1 (cons 2 3))) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_car
```


### fourth ###

Takes an object that satisfies `list?` with at least 4 values, returning the fourth value.

```scheme
(fourth '(1 2 3 4)) ; ==> 4
(fourth '(1 2 3 4 5)) ; ==> 4
(fourth (cons 1 (cons 2 (cons 3 4)))) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_car
```

### fifth ###

Takes an object that satisfies `list?` with at least 5 values, returning the fifth value.


```scheme
(fifth '(1 2 3 4 5)) ; ==> 5
(fifth '(1 2 3 4 5)) ; ==> 5
(fifth (cons 1 (cons 2 (cons 3 (cons 4 5))))) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_car
```

### length ###

Returns the length of a Cons object.

```scheme
(length '(1 2 3 4 5)) ; ==> 5
(length '()) ; ==> 0
(length 4) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_cdr
```

### list-tail ###

unimplmemented, just here becuase i want to implement it.

### drop ###

Drops the first `n` elements from a list.

```scheme
(drop '(1 2 3 4 5) 2) ; ==> '(3 4 5)
(drop '(1 2 3 4 5) 12) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_cdr
```

### take ###

unimplmemented, just here becuase i want to implement it.

### member ###

unimplmemented, just here becuase i want to implement it.

### memv ###

memv searches through a list for an element. If it finds that element it will return a new list,
with that value as the car, and the rest as the cdr.

If the value is not found, then #f is returned.

```scheme
(memv 2 '(1 2 3 4)) ; ==> '(2 3 4)
(memv 123 '(1 2 3 4)) ; ==> #f
(memv 2 '(4 3 2 1)) ; ==> '(2 1)
```

### map ###

Map takes a function, and a list.

The function takes a single argument, and returns anything, and the list can be of anything.

Each element of the list is processed through the function,
and a new list is returned with each element changed via the function.

The order in which the elements are fed into the function is unspecified!

```scheme
(map (lambda (x) x) '(1 2 3)) ; ==> '(1 2 3)
(map (lambda (x) (+ x 1)) '(1 2 3)) ; ==> '(2 3 4)
> (map (lambda (x) (print "\n")) '(1 2 3)) ; if you are gonna run this on the CLI escape the quotes.



'(#<void> #<void> #<void>)
```

### append ###

Takes 2 lists, and appends them. The first must be null-terminated (satisfied `list?`),
but the second does not have that restriction.

```scheme
(append (list 1 2) (list 3 4)) ; ==> '(1 2 3 4)
(append (list 1 2) (cons 3 4)) ; ==> '(1 2 3 . 4)
(append (cons 1 2) (cons 3 4)) ; ==> Fatal library run-time error: unwrap_cons takes a Cons object! in fn prim_car
```

### foldl ###

Like `map`, foldl takes a procedure, but it takes other arguments, known as an accumulator, and the list.

The procedure function takes two arguments, the current element being processed, and the current accumulator.
The function will return the newest accumulator, to be used in the next function call.

The accumulator argument is the **initial** accumulator, provided to the leftmost list element.

The list is applied to the function left-to-right argument wise:

(examples from racket documentation!)

```scheme
(foldl cons '() '(1 2 3 4)) ; ==> '(4 3 2 1)'
(foldl + 0 '(1 2 3 4)) ; ==> 10
```

### foldr ###

Similar to foldl, but the list is applied right-to-left.

```scheme
(foldr cons '() '(1 2 3 4)) ; ==> '(1 2 3 4)
(foldr (lambda (v l) (cons (+ 1 v) l)) '() '(1 2 3 4)) ; ==> '(2 3 4 5)
```

## Vectors ##

### vector? ###

Tests whether the given argument is of type Vector.

```scheme
(vector? #f) ; ==> #f
(vector? (vector 1 2 3)) ; ==> #t
```

### vector ###

Takes any amount of arguments and returns a vector with those arguments in that order

```scheme
(vector) ; ==> #()
(vector 1 2 3) ; ==> #(1 2 3)
(vector (cons 1 2) 3 4) ; ==> #((1 . 2) 3 4)
```

### make-vector ###

Takes 2 arguments, an integer, and anything, and returns a vector with the number of slots,
each filled with that anything.

```scheme
(make-vector 2 'hi) ; ==> #(hi hi)
(make-vector 0 'hi) ; ==> #()
```

### vector-ref ###

Takes 2 arguments, a vector, and a position (integer), and will get the positions value in the vector.

If you try to reference a spot out-of-bounds, a ***guardable*** error will be raised(!!!).

```scheme
(vector-ref (vector 5 4 3 2 1) 2) ; ==> 3
(vector-ref (vector 1) 1) ; ==> Uncaught Exception: '(exception vector-out-of-bounds (vector-ref (vector 1 2) 2))
(guard (x [x 'got!]) (vector-ref (vector 1) 1)) ; ==> 'got!
```

### vector-set! ###

Takes 3 arguments, a vector, a position (integer), and a value,
and will set the value to the vectors object at the position

This changes the vector in place, it is ***not*** a functional update.

If you try to reference a spot out-of-bounds, a ***guardable*** error will be raised(!!!).

```scheme
(define v (vector 1 2 3))
(vector-set! v 2 4) ; ==> #<void>
v ; ==> #(1 2 4)
(vector-set! v 123 4) ; ==> Uncaught Exception: '(exception vector-set!-out-of-bounds (vector-set! v 123 4))
(guard (x [x 'got!]) (vector-set! v 123 4)) ; ==> 'got!
```

### vector-length ###

Returns the length of a vector.

```scheme
(vector-length (vector)) ; ==> 0
(vector-length (vector 1 2 3)) ; ==> 3
(vector-length 'not-vector) ; ==> Fatal library run-time error: unwrap_vector takes a Vector object! in fn prim_vector_45length
```
## Sets ##

Unimplemented...

### set ###

### set->list ###

### list->set ###

### set-add ###

### set-union ###

### set-count ###

### set-first ###

### set-rest ###

### set-remove ###

## Hashes ##

Unimplemented...

### hash ###

### hash-ref ###

### hash-set ###

### hash-count ###

### hash-keys ###

### hash-has-key? ###

### hash? ###


### error ###

unimplmemented, just here becuase i want to implement it.

### void ###

Returns a value of type Void.

```scheme
(display (void)) ; ==> NOTHING PRINTED
(display (cons (void) '())) '(#<void>)
```

### print ###

prints a given value to standard out.

Returns a Void typed, value.

```scheme
(print 'hi) ; ==> #<void>
'hi<no-newline>
```

### display ###

unimplmemented, just here becuase i want to implement it.

### write ###

unimplmemented, just here becuase i want to implement it.

### exit ###

unimplmemented, just here becuase i want to implement it.

### halt ###

Ends the program, and prints the given value.

```scheme
(halt 'n) ; ==> 'n
(+ 1 (halt 'n)) ==> 'n
```
