
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

extern "C" {

typedef char s8;
typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;

typedef enum { Void=0, Null, Bool
             , Closure, Cons, Int
             , Str, Sym, Vector
             , Hash, Set, Other
             , Continuation} SinType;

typedef struct SinObj {
    s8* valueptr;
    SinType type;
} SinObj;

typedef struct SinRecord {
    SinObj** stack;
    struct SinRecord* next;
    s64 size;
    /// Where we go to in the stack
    /// when reinstantiating this record.
    s64 return_fp_offset;
    /// Where we go to in the code-stream
    void* return_address;
} SinRecord;

SinObj* alloc(const u64 amt) {
    // return reinterpret_cast<SinObj*>(GC_MALLOC(sizeof(SinObj) * amt));
    return NULL; // TODO:!
}

SinObj* unwrap_clo(SinObj* clo_obj, const char* fn) {
    ASSERT_TYPE(*clo_obj, Closure, "unwrap_clo takes a Closure object! Got %d in fn %s",
                clo_obj->type, fn);
    return reinterpret_cast<SinObj*>(clo_obj->valueptr);
}


// TODO: rename this to make_record.
SinRecord* make_frame(SinRecord* cur_stack_record) {
    SinRecord* sf = (SinRecord*) malloc(sizeof(SinRecord));
    SinObj** new_stack = (SinObj**) calloc(STACK_SIZE, sizeof(SinObj*));
    sf->stack = new_stack;
    sf->size = STACK_SIZE / sizeof(SinObj*);
    sf->next = cur_stack_record;
    return sf;
}

/**
 * old_stack_record is the 'old' record
 * new_segment_base is the base pointe of the new segment
 * cutoff_slots, #slots (from old base) in the old part of the record to keep
 *               (the new size of the old record)
 * ret_fp_slots is the index of the fp in the old record when reinstantiating.
 * ret_addr is the location in the code stream to return to when reinstantiating.
 */
SinRecord* split_record(SinRecord* old_stack_record, SinObj** new_segment_base,
                        s64 cutoff_slots, s64 ret_fp_slots, void* ret_addr) {
    SinRecord* new_record = (SinRecord*) malloc(sizeof(SinRecord));
    new_record->stack = new_segment_base;
    new_record->size = old_stack_record->size - cutoff_slots;
    new_record->next = old_stack_record;
    old_stack_record->size = cutoff_slots;
    old_stack_record->return_fp_offset = ret_fp_slots;
    old_stack_record->return_address = ret_addr;
    return new_record;
}


SinObj* make_continuation_closure(SinRecord* cont, void* cont_func_loc) {
    SinObj* clo = alloc(2);
    SinObj lam_part, env_part;

    lam_part.type = Other;
    lam_part.valueptr = cont_func_loc;

    env_part.type = Continuation;
    env_part.valueptr = reinterpret_cast<void*>(cont);

    clo_obj[0] = lam_part;
    clo_obj[1] = env_part;

    SinObj* ret = alloc(1);
    ret->type = Closure;
    ret->valueptr = reinterpret_cast<void*>(clo_obj);
    return ret;
}

/// Returns a void pointer describing the env part of the closure
/// This cant be more specific, as when the closure is actually
/// a continuation, it is not a vector.
SinObj* closure_get_env_part(SinObj* clo) {
    SinOBj* clo_obj = unwrap_clo(clo, "closure_get_env_part");
    return clo_obj[1].valueptr;
}

void* closure_get_fn_part(SinObj* clo) {
    SinObj* clo_obj = unwrap_clo(clo, "closure_get_fn_part");
    return reinterpret_cast<void*>(clo_obj[0].valueptr);
}

void* handle_underflow(SinRecord** srr, SinObj*** fpr) {
    SinRecord* cur = *srr;
    SinRecord* next = cur->next;
    cur->next = next->next;
    s64 n = cur->size;
    s64 m = next->size;
    s64 o = next->return_fp_offset;
    void* ret_addr = next->return_address;
    if (m >= n) {
        // TODO: garbage collection (:
        free(cur->stack);
        cur->stack = (SinObj**) calloc(STACK_SIZE, sizeof(SinObj*));
    }
    memcpy(cur->stack, next->stack, m * 8);
    *fpr = &cur->stack[o];
    return ret_addr;
}

??? handle_overflow(SinRecord** srr, SinOBj*** fpr) {
    ///
}

int main() {
    SinRecord* f = make_frame(NULL);
    // s64* stack = (s64*) calloc(STACK_SIZE, sizeof(s64));
    // s64* mid = &stack[10];
    // s64* before = &mid[-3];
    SinObj** stack = f->stack;
    SinRecord* next = f->next;
    printf("%p\n", stack);
    // s64 fp_loc = f->fp_loc;
    return 0;
}

}
