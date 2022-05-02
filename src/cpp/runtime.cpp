
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// THIS IS THE NEW RUNTIME LIBRARY
// FOR SEGMENTED STACKS IN SINSCHEME
//
// FUCK YAH!

// TODO: import most of the shit from the old header.cpp
// TODO: make a runtime.h header.

#define STACK_SIZE 4096 // new stack record size

typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;

typedef enum { Void=0, Null, Bool
             , Closure, Cons, Int
             , Str, Sym, Vector
             , Hash, Set, Other} SinType;

typedef struct SinObj {
    u64* valueptr;
    SinType type;
} SinObj;

typedef struct SinFrame {
    SinObj** stack;
    struct SinFrame* next;
    size_t size;
    void* ret_addr;
} SinFrame;

SinFrame* make_frame(SinFrame* cur_stack_record, s64* cur_fp) {
    SinFrame* sf = (SinFrame*) malloc(sizeof(SinFrame));
    SinObj** new_stack = (SinObj**) calloc(STACK_SIZE, sizeof(SinObj*));
    sf->stack = new_stack;
    sf->size = STACK_SIZE / 8;
    sf->next = cur_stack_record;
    sf->ret_addr = cur_fp;
    return sf;
}

int main() {
    // SinFrame* f = make_frame(NULL, NULL);
    s64* stack = (s64*) calloc(STACK_SIZE, sizeof(s64));
    s64* mid = &stack[10];
    s64* before = &mid[-3];
    return 2;
}

