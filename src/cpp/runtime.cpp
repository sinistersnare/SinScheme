
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
SinRecord* make_frame(SinRecord* cur_stack_record, SinObj*** spr) {
    SinRecord* sf = (SinRecord*) malloc(sizeof(SinRecord));
    SinObj** new_stack = (SinObj**) calloc(STACK_SIZE, sizeof(SinObj*));
    *spr = new_stack + STACK_SIZE - 100; // TODO: is this corrrect (:
    sf->stack = new_stack;
    sf->size = STACK_SIZE / sizeof(SinObj*);
    sf->next = cur_stack_record;
    return sf;
}

/**
 * old_stack_record is the 'old' record
 * new_segment_base is the base pointer of the new segment
 * cutoff_slots, #slots (from old base) in the old part of the record
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

void* handle_underflow(SinRecord** srr, SinObj*** fpr, SinObj*** spr) {
    SinRecord* cur = *srr;
    SinRecord* next = cur->next;
    cur->next = next->next;
    s64 n = cur->size;
    s64 m = next->size;
    s64 o = next->return_fp_offset;
    void* ret_addr = next->return_address;
    if (m >= n) {
        // cur->stack may not be the root of the allocation. So we can't free it.
        // TODO: garbage collection (:
        cur->stack = (SinObj**) calloc(STACK_SIZE, sizeof(SinObj*));
        *spr = new_stack + STACK_SIZE - 100; // TODO: is this corrrect (:
    }
    memcpy(cur->stack, next->stack, m * 8);
    *fpr = &cur->stack[o];
    return ret_addr;
}

// returns true if we are safe, false if we will overflow.
bool check_for_overflow(SinRecord** srr, SinObj*** fpr, SinObj*** spr, s64 caller_slots) {
    return reinterpret_cast<s64>(*fpr + caller_slots) < reinterpret_cast<s64>(*spr);
}

bool callcc_at_base(SinRecord** srr, SinObj*** fpr) {
    return (*srr)->stack == *fpr;
}

void handle_overflow(SinRecord** srr, SinObj*** fpr, SinObj*** spr,
                     void* underflow_loc, s64 num_slots_in_overflower) {
    s64 amt_to_descend;
    s64 amt_to_copy = num_slots_in_overflower;
    bool copy_entire = false;
    SinObj** descent = *fpr;
    for (int i=0; i < 4; i++) {
        amt_to_descend = reinterpret_cast<s64>(descent[1]);
        if (amt_to_descend == -1) {
            copy_entire = true;
            break;
        }
        amt_to_copy += amt_to_descend;
        num_frames_to_copy++;
        descent -= amt_to_descend;
    }

    if (copy_entire) {
        // here, it seems the stack that overflowed is super small, so lets
        // just completely copy it into a bigger one.
        // So full is going to be dropped.
        SinRecord* full = *srr;
        // go past the full stack
        SinRecord* empty_record = make_frame(full->next, spr);
        assert (descent == full->stack);
        memcpy(empty_record->stack, descent, amt_to_copy * 8);
        *srr = empty_record;
        *fpr = empty_record->stack + amt_to_copy - num_slots_in_overflower;
    } else {
        SinRecord* full = *srr;
        SinRecord* empty_record = make_frame(full, spr);
        full->return_address = reinterpret_cast<void*>(descent[0]);
        full->size = (reinterpret_cast<s64>(descent) -
                      reinterpret_cast<s64>(full->stack)) / 8
        full->return_fp_offset = full->size - amt_to_descend;
        memcpy(empty_record->stack, descent, amt_to_copy * 8);
        empty_record->stack[0] = reinterpret_cast<SinObj*>(underflow_loc);
        empty_record->stack[1] = reinterpret_cast<SinObj*>(-1);
        *srr = empty_record;
        *fpr = empty_record->stack + amt_to_copy - num_slots_in_overflower;
    }
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
