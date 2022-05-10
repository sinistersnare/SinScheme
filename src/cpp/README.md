# SinScheme C++ Runtime #

Hi, this is where runtime stuff exists.
Probably should put more docs here...



## Calling Convention ##

With the migration to Segmented Stacks, we cant utilize LLVM functions,
everything is defined inside of 1 giant LLVM functions, and we operate on a
runtime-created stack (or rather, stacks, more on that elsewhere).

To support segmented stacks, we adopted a calling convention based off of
Hieb et al. in the paper
"Representing Control in the Presence of First-Class Continuations".
This section covers how the calling convention works.

### CALLEE ###

The callee needs to setup the call-frame at the moment of its call, and needs to place the return value in the correct place.

Immediately after jumping to the function:
1. Place locals starting at `&@fpr[3]` (0,1,2 are placed by the caller)
2. Nothing else, do function stuff

Immediately before returning:
1. Place the return value in the `@retr` register.

### CALLER ###

The caller has work to do immediately before a call and immediately after a return.

Immediately before a call:
1. `@fpr += N` (this N is the caller's call-frame size, not the callee's)
2. `@fpr[0] = RA`, `RA` is the return address.
3. `@fpr[1] = N`
3. `@fpr[2] = Arg0`
4. `@fpr[3] = Arg1`

Immediately after the return point is jumped to:
-- Fix this to get N from @fpr[1] --
1. `@fpr -= N` (Again, this N is the callers call-frame size)
2. Place the value of the `@retr` correctly in the call-frame.
3. Nothing else, continue processing.


# TODO #

Should redo these docs!
Tail calls are required, they dont alter @fpr.
And walk through the whole steps, not just what is set...
Something like, 1) increment @fpr, 2) place things, 3) indirectbr.
