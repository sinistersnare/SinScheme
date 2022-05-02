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

The callee only needs to do things at the moment of its call. It needs to place its local variables, and the size of the call-frame. To be clear.

Immediately after jumping to the function:
1. Place locals starting at `&@fpr[3]` (0,1,2 are placed by the caller)
3. Nothing else, do function stuff

Again, there is nothing to do when returning.

### CALLER ###

The caller has work to do immediately before a call and immediately after a return.

Immediately before a call:
1. `@fpr += N` (this N is the caller function's size, not the callee's)
2. `@fpr[0] = RA`, `RA` is the return address.
3. `@fpr[1] = Arg0`
4. `@fpr[2] = Arg1`

Immediately after the return point is jumped to:
1. `@fpr -= @fpr[-1]`
2. Nothing else, continue processing.
