
#include "runtime.h"
#include "hash.h"
// TODO: maybe prims can be header-only like hash? Then no circularity maybe?

#include <stdio.h>
#include <string.h>
#include <assert.h>

// THIS IS THE NEW RUNTIME LIBRARY
// FOR SEGMENTED STACKS IN SINSCHEME
//
// FUCK YAH!

extern "C" {

// header.h doesnt include hash.h so i have prototypes here.
SinObj* map_to_sin(Map* m);
Map* unwrap_hash(SinObj* hash_obj, const char* fn);

void debug_output_registers(SinRecord** srr, SinObj*** fpr, SinObj*** spr, SinObj** retr) {
    // call like:
    //   call void @debug_output_registers(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr, %struct.SinObj** @retr)
    printf("Register Values:\n");
    printf("srr:`%p`,fpr:`%p`,spr:`%p`,retr:`%p`\n", *srr, *fpr, *spr, *retr);
}

/// Alloc an amount of SinObjs.
SinObj* alloc(const u64 amt) {
    // TODO: replace `malloc`/`calloc` with this... what to do about SinRecord though?
    // return reinterpret_cast<SinObj*>(GC_MALLOC(sizeof(SinObj) * amt));
    // return NULL; // TODO:!
    return reinterpret_cast<SinObj*>(malloc(sizeof(SinObj) * amt));
}

SinObj* alloc_atomic(const u64 amt) {
    // TODO: will this matter in the new GC?
    return alloc(amt);
}

/**
  * Gets the car and cdr of the given lst (the first argument)
  * And puts them into the 2nd and 3rd arguments.
  */
void _get_both(SinObj* lst, SinObj* car, SinObj* cdr) {
    SinObj* cons_obj = unwrap_cons(lst, "_get_both");
    // this is safe because the lifetimes are the same
    // the lifetime of lst >= the lifetimes of car and cdr.
    *car = cons_obj[0];
    *cdr = cons_obj[1];
}

// TODO: actual global objects for constant values like #t, #f or '() ?

SinObj* const_init_int(s64 i) {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = reinterpret_cast<u64*>(i);
    ret->type = Int;
    return ret;
}

SinObj* const_init_void() {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = nullptr;
    ret->type = Void;
    return ret;
}

SinObj* const_init_null() {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = nullptr;
    ret->type = Null;
    return ret;
}

SinObj* const_init_true() {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = reinterpret_cast<void*>(true);
    ret->type = Bool;
    return ret;
}

SinObj* const_init_false() {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = reinterpret_cast<void*>(false);
    ret->type = Bool;
    return ret;
}

SinObj* const_init_string(char* s) {
    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<void*>(s);
    ret->type = Str;
    return ret;
}

SinObj* const_init_symbol(char* s) {
    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<void*>(s);
    ret->type = Sym;
    return ret;
}


SinRecord* make_record(SinRecord* cur_stack_record, SinObj*** spr) {
    SinRecord* sf = reinterpret_cast<SinRecord*>(malloc(sizeof(SinRecord)));
    SinObj** new_stack = reinterpret_cast<SinObj**>(calloc(STACK_SIZE, sizeof(SinObj*)));
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
    SinRecord* new_record = reinterpret_cast<SinRecord*>(malloc(sizeof(SinRecord)));
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

    clo[0] = lam_part;
    clo[1] = env_part;

    SinObj* ret = alloc(1);
    ret->type = Closure;
    ret->valueptr = reinterpret_cast<void*>(clo);
    return ret;
}

/// Closure Memory Layout
/// It is 2 SinObj laid out besides eachother.
/// The first SinObj (index 0) is an Other type and it has a ptrvalue of the function pointer
/// The second SinObj (index 1) is the vector that holds the environment (free variables).
SinObj* closure_alloc(const s64 amt_freevars, void* fptr) {
    SinObj* clo_obj = alloc(2);
    SinObj lam_part, env_part;

    // use Other type for the constituents of the Closure
    // as they are internal to the Closure object itself.
    lam_part.type = Other;
    lam_part.valueptr = reinterpret_cast<void*>(fptr);
    env_part.type = Other;
    env_part.valueptr = reinterpret_cast<void*>(prim_make_45vector(const_init_int(amt_freevars),
                                                                   const_init_int(0)));
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
void* closure_get_env_part(SinObj* clo) {
    SinObj* clo_obj = unwrap_clo(clo, "closure_get_env_part");
    return clo_obj[1].valueptr;
}

void* closure_get_fn_part(SinObj* clo) {
    SinObj* clo_obj = unwrap_clo(clo, "closure_get_fn_part");
    return clo_obj[0].valueptr;
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
        cur->stack = reinterpret_cast<SinObj**>(calloc(STACK_SIZE, sizeof(SinObj*)));
        *spr = cur->stack + STACK_SIZE - 100; // TODO: is this corrrect (:
    }
    memcpy(cur->stack, next->stack, static_cast<size_t>(m) * 8);
    *fpr = &cur->stack[o];
    return ret_addr;
}

// returns true if we are safe, false if we will overflow.
// TODO: srr parameter not needed.
bool check_for_overflow(SinObj*** fpr, SinObj*** spr, s64 caller_slots) {
    return reinterpret_cast<s64>(*fpr + caller_slots) < reinterpret_cast<s64>(*spr);
}

bool callcc_at_base(SinRecord** srr, SinObj*** fpr) {
    return (*srr)->stack == *fpr;
}

void handle_overflow(SinRecord** srr, SinObj*** fpr, SinObj*** spr,
                     void* underflow_loc, s64 num_slots_in_overflower) {
    s64 amt_to_descend = 0;
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
        descent -= amt_to_descend;
    }

    if (copy_entire) {
        // here, it seems the stack that overflowed is super small, so lets
        // just completely copy it into a bigger one.
        // So full is going to be dropped.
        SinRecord* full = *srr;
        // go past the full stack
        SinRecord* empty_record = make_record(full->next, spr);
        assert(descent == full->stack);
        memcpy(empty_record->stack, descent, static_cast<size_t>(amt_to_copy) * 8);
        *srr = empty_record;
        *fpr = empty_record->stack + amt_to_copy - num_slots_in_overflower;
    } else {
        SinRecord* full = *srr;
        SinRecord* empty_record = make_record(full, spr);
        full->return_address = reinterpret_cast<void*>(descent[0]);
        full->size = (reinterpret_cast<s64>(descent) -
                      reinterpret_cast<s64>(full->stack)) / 8;
        full->return_fp_offset = full->size - amt_to_descend;
        memcpy(empty_record->stack, descent, static_cast<size_t>(amt_to_copy) * 8);
        empty_record->stack[0] = reinterpret_cast<SinObj*>(underflow_loc);
        empty_record->stack[1] = reinterpret_cast<SinObj*>(-1);
        *srr = empty_record;
        *fpr = empty_record->stack + amt_to_copy - num_slots_in_overflower;
    }
}

/// This is called when something like `(k x)` happens
/// Where `k` is a continuation object.
void* handle_continuation_function(SinRecord** srr, SinObj*** fpr,
                                   SinObj*** spr, SinObj** retr) {
    SinObj** cur_fp = *fpr;
    // TODO: magic numbers, maybe make these arguments?
    SinObj* clo = cur_fp[2];
    SinObj* arglist = cur_fp[3];
    // continuations take exactly 1 arg.
    SinObj* arg = prim_car(arglist);

    // Get the continuation parts
    SinRecord* cont_record = reinterpret_cast<SinRecord*>(closure_get_env_part(clo));
    SinRecord* cur_record = *srr;

    if (cont_record->size >= cur_record->size) {
        // TODO: cant deallocate cur_record cause it may not be the root.
        //       SO MAKE A GC!
        // overflow will happen, need to reallocate.
        SinRecord* new_record = make_record(cont_record->next, spr);

        cur_record = new_record;
        *srr = new_record;
    }

    memcpy(cur_record->stack, cont_record->stack, static_cast<size_t>(cont_record->size) * 8);

    *retr = arg;
    *fpr = &cur_record->stack[cont_record->return_fp_offset];

    return cont_record->return_address;
}

void start_runtime(SinRecord** srr, SinObj*** fpr, SinObj*** spr) {
    // This will eventually also start the GC! Assuming I make a GC :)
    printf("Starting program!\n");
    *srr = make_record(nullptr, spr);
    *fpr = (*srr)->stack;
}

// Prims


/// Takes a Cons and returns its car
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)
SinObj* prim_car(SinObj* cons_obj) { // car
    SinObj* cons = unwrap_cons(cons_obj, "car");
    return &cons[0];
}

/// Returns a SinObj of type Int
SinObj* prim__43(SinObj* a, SinObj* b) { // +
    s64 a_val = unwrap_int(a, "+ a");
    s64 b_val = unwrap_int(b, "+ b");
    return const_init_int(a_val + b_val);
}

// Takes a Cons object
// (called cur becuase we loop thru it and use the same binding)
SinObj* applyprim__43(SinObj* cur) { // apply +
    SinType typ = cur->type;
    if (typ != Cons && typ != Null) {
        fatal_errf("Expected Cons in apply +, but got %s", get_type_name(typ));
    }

    s64 final = 0;

    while (cur->type == Cons) {
        SinObj car, cdr;
        _get_both(cur, &car, &cdr);
        final += unwrap_int(&car, "apply + final");
        cur = &cdr;
    }
    return const_init_int(final);
}

GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)
SinObj* prim_make_45vector(SinObj* length_obj, SinObj* fill) { // make-vector

    u64 len = static_cast<u64>(unwrap_int(length_obj, "make-vector"));
     // i is amount of elements, + 1 is 1 object for size.
    SinObj* vec = alloc(1 + len);

    vec[0].type = Int;
    vec[0].valueptr = reinterpret_cast<u64*>(len);
    for (u64 i = 1; i <= len; i++) {
        // copy the fill var
        SinObj* cur = alloc(1);
        cur->type = fill->type;
        cur->valueptr = fill->valueptr;

        vec[i].type = Other;
        vec[i].valueptr = reinterpret_cast<u64*>(cur);
    }

    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(vec);
    ret->type = Vector;
    return ret;
}


SinObj* closure_env_get(SinObj* clo, s64 pos) {
    // unwrap handles asserting
    SinObj* clo_obj = unwrap_clo(clo, "closure_env_get");
    SinObj* vec = reinterpret_cast<SinObj*>(clo_obj[1].valueptr);
    return prim_vector_45ref(vec, const_init_int(static_cast<s64>(pos)));
}

Map* unwrap_hash(SinObj* hash_obj, const char* fn) {
    ASSERT_TYPE(*hash_obj, Hash, "unwrap_hash takes a Hash object! Got %d in fn %s",
                hash_obj->type, fn);
    return reinterpret_cast<Map*>(hash_obj->valueptr);
}

SinObj* unwrap_cons(SinObj* cons_obj, const char* fn) {
    ASSERT_TYPE(*cons_obj, Cons, "unwrap_cons takes a Cons object! Got %d in fn %s",
                cons_obj->type, fn);
    return reinterpret_cast<SinObj*>(cons_obj->valueptr);
}

SinObj* unwrap_vector(SinObj* vec_obj, const char* fn) {
    ASSERT_TYPE(*vec_obj, Vector, "unwrap_vector takes a Vector object! Got %d in fn %s",
                vec_obj->type, fn);
    return reinterpret_cast<SinObj*>(vec_obj->valueptr);
}

SinObj* unwrap_clo(SinObj* clo_obj, const char* fn) {
    ASSERT_TYPE(*clo_obj, Closure, "unwrap_clo takes a Closure object! Got %d in fn %s",
                clo_obj->type, fn);
    return reinterpret_cast<SinObj*>(clo_obj->valueptr);
}

s64 unwrap_int(SinObj* int_obj, const char* fn) {
    ASSERT_TYPE(*int_obj, Int, "unwrap_int takes an Int object! Got %d in fn %s",
                int_obj->type, fn);
    return reinterpret_cast<s64>(int_obj->valueptr);
}

u64 unwrap_bool(SinObj* bool_obj, const char* fn) {
    ASSERT_TYPE(*bool_obj, Bool, "unwrap_bool takes a Bool object! Got %d in fn %s",
                bool_obj->type, fn);
    return reinterpret_cast<u64>(bool_obj->valueptr);
}

char* unwrap_str(SinObj* str_obj, const char* fn) {
    ASSERT_TYPE(*str_obj, Str, "unwrap_str takes a Str object! Got %d in fn %s",
                str_obj->type, fn);
    return reinterpret_cast<char*>(str_obj->valueptr);
}

char* unwrap_sym(SinObj* sym_obj, const char* fn) {
    ASSERT_TYPE(*sym_obj, Sym, "unwrap_sym takes a Sym object! Got %d in fn %s",
                sym_obj->type, fn);
    return reinterpret_cast<char*>(sym_obj->valueptr);
}

// TODO: Have this returning a sizeof(bool) int, and use that instead of wasting so much space
u64 is_truthy_value(SinObj* obj) {
    return (obj->type == Bool && (unwrap_bool(obj, "is_truthy_value") == false))
            ? false : true;
}

////// Printing

GEN_EXPECT1ARGLIST(applyprim_display, prim_display)
SinObj* prim_display(SinObj* obj) {
    // forward to `print`
    // theres probably different semantics between
    // print and display... but.... nahhh
    return prim_print(obj);
}

GEN_EXPECT1ARGLIST(applyprim_print, prim_print)
SinObj* prim_print(SinObj* obj) {
    SinType typ = obj->type;

    switch (typ) {
    case Null:
        printf("'()");
        break;
    case Sym:
        printf("'%s", unwrap_sym(obj, "prim_print Sym case."));
        break;
    case Void:
        // print nothing on void.
        break;
    case Cons:
        printf("'(");
        print_cons(obj);
        printf(")");
        break;
    case Bool:
    case Continuation:
    case Closure:
    case Int:
    case Str:
    case Vector:
    case Hash:
    case Set:
    case Other:
        prim_print_aux(obj);
        break;
    }
    return const_init_void();
}

GEN_EXPECT1ARGLIST(applyprim_println, prim_println)
SinObj* prim_println(SinObj* obj) {
    prim_print(obj);
    printf("\n");
    return const_init_void();
}

SinObj* prim_print_aux(SinObj* obj) {
    SinType typ = obj->type;

    switch (typ) {
    case Void:
        printf("#<void>");
        break;
    case Null:
        printf("()");
        break;
    case Bool: {
        u64 bv = unwrap_bool(obj, "prim_print_aux bool case");
        if (bv == 0) {
            printf("#f");
        } else if (bv == 1) {
            printf("#t");
        } else {
            printf("Unknown Boolean value: %lu", bv);
        }
    }
        break;
    case Continuation:
        printf("#<continuation>");
        break;
    case Closure:
        printf("#<procedure>");
        break;
    case Cons:
        printf("(");
        print_cons(obj);
        printf(")");
        break;
    case Int:
        printf("%ld", unwrap_int(obj, "prim_print_aux Int case."));
        break;
    case Str:
        printf("%s", unwrap_str(obj, "prim_print_aux Str case."));
        break;
    case Sym:
        printf("%s", unwrap_sym(obj, "prim_print_aux Sym case."));
        break;
    case Vector:
        print_vector(obj);
        break;
    case Hash:
        printf("#hash(");
        print_hash(obj);
        printf(")");
        break;
    case Set:
        printf("Sets not currently supported!");
        break;
    case Other:
        printf("(print v); unrecognized value %lu", reinterpret_cast<u64>(obj->valueptr));
        break;
    }
    return const_init_void();
}

SinObj* print_cons(SinObj* obj) {
    SinObj* cons = unwrap_cons(obj, "print_cons");
    SinObj* car = &cons[0];
    SinObj* cdr = &cons[1];

    prim_print_aux(car);

    switch (cdr->type) {
    case Null:
        break;
    case Void:
        printf("#<void>");
        break;
    case Cons:
        printf(" ");
        print_cons(cdr);
        break;
    case Continuation:
    case Sym:
    case Bool:
    case Closure:
    case Int:
    case Str:
    case Vector:
    case Hash:
    case Set:
    case Other:
        printf(" . ");
        prim_print_aux(cdr);
        break;
    }
    return const_init_void();
}

SinObj* print_vector(SinObj* obj) {
    SinObj* vector = unwrap_vector(obj, "print_vector");
    u64 len = _get_vector_length(obj);
    printf("#("); // looks like a sad face :P
    for (u64 i = 1; i <= len; i++) {
        if (i != 1) {
            printf(" ");
        }
        SinObj* inner = reinterpret_cast<SinObj*>(vector[i].valueptr);
        prim_print_aux(inner);
    }
    printf(")");

    return const_init_void();
}

SinObj* print_hash(SinObj* obj) {
    Map* map = unwrap_hash(obj, "print_hash");

    while (map != nullptr) {
        printf("(");
        prim_print_aux(map->key);
        printf(" . ");
        prim_print_aux(map->value);
        printf(")");

        if (map->next != nullptr) {
            printf(" ");
        }
        map = map->next;
    }

    return const_init_void();
}

const char* get_type_name(SinType type) {
    switch (type) {
    case Void:
        return "Void";
    case Null:
        return "Null";
    case Bool:
        return "Bool";
    case Continuation:
        return "Continuation";
    case Closure:
        return "Closure";
    case Cons:
        return "Cons";
    case Int:
        return "Int";
    case Str:
        return "String";
    case Sym:
        return "Symbol";
    case Vector:
        return "Vector";
    case Hash:
        return "Hash";
    case Set:
        return "Set";
    case Other:
        return "Other";
    }
}


// Primitives



GEN_EXPECT1ARGLIST(applyprim_halt, prim_halt)
SinObj* prim_halt(SinObj* val) { // halt
    prim_print(val);
    if (val->type != Void) {
        printf("\n");
    }
    exit(0);
}


u64 _get_vector_length(SinObj* obj) {
    SinObj* vec_obj = unwrap_vector(obj, "get_vector_length");
    return reinterpret_cast<u64>(vec_obj[0].valueptr);
}

/// Vector layout is as so
/// vector->type = Vector
/// vector->valueptr = raw_vector
/// raw_vector is a list of SinObj
/// position 0 is Int,len (being the amount of SinObj to the right of this)
/// position i (1-indexed) is Other,SinObj*

SinObj* applyprim_vector(SinObj* curptr) { // apply vector
    SinObj cur = *curptr;
    // TODO: Support for larger vectors?
    SinObj* buf[256] = {nullptr};
    u64 i = 0;

    while (cur.type == Cons && i < 256) {
        SinObj car, cdr;
        _get_both(&cur, &car, &cdr);
        buf[i] = alloc(1);
        buf[i]->type = car.type;
        buf[i++]->valueptr = car.valueptr;
        cur = cdr;
    }
    if (i == 256 && cur.type != Null) {
        fatal_err("Vectors larger than 256 elements are unimplemented. Sorry!");
    }

    // i is amount of elements, add 1 for length at pos 0.
    SinObj* mem = alloc(i+1);

    mem[0].type = Int;
    mem[0].valueptr = reinterpret_cast<u64*>(i);
    for (u64 j = 0; j < i; j++) {
        mem[j+1].type = Other;
        mem[j+1].valueptr = reinterpret_cast<u64*>(buf[j]);
    }

    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(mem);
    ret->type = Vector;
    return ret;
}



GEN_EXPECT1ARGLIST(applyprim_vector_63, prim_vector_63)
SinObj* prim_vector_63(SinObj* maybevec) {
    return (maybevec->type == Vector)
        ? const_init_true()
        : const_init_false();
}


GEN_EXPECT1ARGLIST(applyprim_vector_45length, prim_vector_45length)
SinObj* prim_vector_45length(SinObj* vec) { // vector-length
    SinObj* raw_vector = unwrap_vector(vec, "prim_vector_45length");
    return &raw_vector[0];
}

void bounds_check(SinObj* raw_vector, s64 pos) {
    s64 length = unwrap_int(&raw_vector[0], "bounds check");
    if (pos + 1 > length) {
        fatal_errf("Bounds check fail, wanted pos %ld, only %ld elements", pos, length);
    }
}

GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)
SinObj* prim_vector_45ref(SinObj* vector, SinObj* pos) { // vector-ref

    SinObj* vec_raw = unwrap_vector(vector, "vector-ref");
    s64 index_pos = unwrap_int(pos, "vector-ref index_pos");
    bounds_check(vec_raw, index_pos);

    // + 1 becuase length is at index 0, so need to push everything up 1.
    return reinterpret_cast<SinObj*>(vec_raw[index_pos + 1].valueptr);
}


GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)
SinObj* prim_vector_45set_33(SinObj* vector, SinObj* pos, SinObj* val) { // vector-set!

    SinObj* vec_raw = unwrap_vector(vector, "vector-set!");
    s64 index_pos = unwrap_int(pos, "apply vector-set! index_pos");
    bounds_check(vec_raw, index_pos);

    SinObj* copy = alloc(1);
    copy->type = val->type;
    copy->valueptr = val->valueptr;

    // + 1 becuase length is index 0, so need to push everything up 1.
    vec_raw[index_pos + 1].valueptr = reinterpret_cast<u64*>(copy);
    return const_init_void();
}


/// Returns a SinObj of type Void

GEN_EXPECT0ARGLIST(applyprim_void, prim_void)
SinObj* prim_void() { // void
    return const_init_void();
}

///// eq? eqv? equal?


// it is assumed that if this is called,
// a and b MUST both be Cons objects.
int cons_eq_helper(SinObj* a, SinObj* b) {
    SinObj acar, acdr, bcar, bcdr;

    _get_both(a, &acar, &acdr);
    _get_both(b, &bcar, &bcdr);

    return (eq_helper(&acar, &bcar) == 1) && (eq_helper(&acdr, &bcdr));
}


int vec_eq_helper(SinObj* a, SinObj* b) {
    SinObj* avec = unwrap_vector(a, "vec_eq_helper avec");
    SinObj* bvec = unwrap_vector(b, "vec_eq_helper bvec");
    // choose either vector it doesnt matter which we use for length.
    s64 alen = unwrap_int(&avec[0], "vec_eq_helper alen");
    s64 blen = unwrap_int(&bvec[0], "vec_eq_helper blen");

    if (alen != blen) {
        return false;
    }

    // if the vectors are of different length, it will be caught on first iteration.
    // else, they it will work perfectly.
    for (s64 i=0; i <= alen; i++) {
        if (eq_helper(&avec[i], &bvec[i]) == 0) {
            return 0;
        }
    }
    // if every object in the vector is equal, it must be equal.
    return 1;
}

// Returns 0 for unequal, and 1 for equal.
int eq_helper(SinObj* a, SinObj* b) {
    if (a->type != b->type) {
        return 0;
    }

    switch (a->type) {
    case Void:
        return 1;
    case Null:
        return 1;
    case Bool:
        if (unwrap_bool(a, "prim_eq_63 Bool a") == unwrap_bool(b, "prim_eq_63 Bool b")) {
            return 1;
        } else {
            return 0;
        }
    // TODO: should we at least do ptr equality for these 2?
    case Continuation:
        return 0;
    case Closure:
        return 0;
    case Cons:
        return cons_eq_helper(a, b);
    case Int:
        if (unwrap_int(a, "prim_eq_63 Int a") == unwrap_int(b, "prim_eq_63 Int b")) {
            return 1;
        } else {
            return 0;
        }
    case Str:
    {
        char* astr = unwrap_str(a, "prim_eq_63 Str a");
        char* bstr = unwrap_str(b, "prim_eq_63 Str b");
        if (strcmp(astr, bstr) == 0) {
            return 1;
        } else {
            return 0;
        }
    }
    case Sym:
    {
        char* asym = unwrap_sym(a, "prim_eq_63 Sym a");
        char* bsym = unwrap_sym(b, "prim_eq_63 Sym b");
        if (strcmp(asym, bsym) == 0) {
            return 1;
        } else {
            return 0;
        }
    }
    case Vector:
        return vec_eq_helper(a, b);
    case Hash:
        {fatal_err("Hash eq? not supported currently.")}
    case Set:
        {fatal_err("Set eq? not supported currently.")}
    case Other:
        {fatal_err("I do not know what youre trying to eq?!!.")}
    }
}

// Does not conform to Rackets eq? probably. PRs welcome to do that :)))
// But this is a scheme... so i just need to read the standard...
// Takes in 2 SinObj* and tells if they are equal.
/// Returns a SinObj* of type Bool
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)
SinObj* prim_eq_63(SinObj* a, SinObj* b) { // eq?
    return make_predicate(eq_helper(a, b) == 1);
}

/// Returns a SinObj of type Bool
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)
SinObj* prim_eqv_63(SinObj* a, SinObj* b) { // eqv?
    return prim_eq_63(a, b); // TODO
}

/// Returns a SinObj of type Bool
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)
SinObj* prim_equal_63(SinObj* a, SinObj* b) { // equal?
    return prim_eq_63(a, b); // TODO
}

////// Other Predicates

SinObj* make_predicate(bool b) {
    return b ? const_init_true() : const_init_false();
}

// TODO: return #t/#f constants here?

/// Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)
SinObj* prim_number_63(SinObj* n) { // number?
    // TODO: floats not implemented lul.
    return prim_integer_63(n);
}


/// Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)
SinObj* prim_integer_63(SinObj* n) { // integer?
    return make_predicate(n->type == Int);
}

/// Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_boolean_63, prim_boolean_63)
SinObj* prim_boolean_63(SinObj* b) { // boolean?
    return make_predicate(b->type == Bool);
}


/// Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)
SinObj* prim_void_63(SinObj* obj) { // void?
    return make_predicate(obj->type == Void);
}


/// Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)
SinObj* prim_procedure_63(SinObj* obj) { // procedure?
    return make_predicate(obj->type == Closure);
}

// null? cons? cons car cdr

/// Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)
SinObj* prim_null_63(SinObj* obj) { // null?
    return make_predicate(obj->type == Null);
}


/// Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)
SinObj* prim_cons_63(SinObj* obj) { // cons?
    return make_predicate(obj->type == Cons);
}

/// Returns a SinObj of type Cons
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)
SinObj* prim_cons(SinObj* car, SinObj* cdr) { // cons
    SinObj* ptr = alloc(2);
    // unsure of safety of this.
    ptr[0] = *car;
    ptr[1] = *cdr;

    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(ptr);
    ret->type = Cons;
    return ret;
}

/// Takes a Cons and returns its cdr
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)
SinObj* prim_cdr(SinObj* cons_obj) { // cdr
    SinObj* cons = unwrap_cons(cons_obj, "cdr");
    return &cons[1];
}


/// Returns a SinObj of type Int
SinObj* prim__45(SinObj* a, SinObj* b) { // -
    // a - b
    s64 a_val = unwrap_int(a, "- a");
    s64 b_val = unwrap_int(b, "- b");

    return const_init_int(a_val - b_val);
}

SinObj* applyprim__45(SinObj* list) { // apply -
    SinObj* cons_obj = unwrap_cons(list, "apply -");
    SinObj car = cons_obj[0];
    SinObj cdr = cons_obj[1];

    s64 carval = unwrap_int(&car, "apply - carval");

    if (cdr.type == Null) {
        return const_init_int(0 - carval);
    }

    s64 final = carval;
    SinObj cur = cdr;
    while (cur.type != Null) {
        SinObj cur_car, cur_cdr;

        _get_both(&cur, &cur_car, &cur_cdr);
        final -= unwrap_int(&cur_car, "applyprim__45 final");
        cur = cur_cdr;
    }
    return const_init_int(final);
}


/// Returns a SinObj of type Int
SinObj* prim__42(SinObj* a, SinObj* b) { // *
    // a - b
    return const_init_int(unwrap_int(a, "* a") * unwrap_int(b, "* b"));
}

SinObj* applyprim__42(SinObj* list) { // apply *
    if (list->type == Null) {
        return const_init_int(1);
    } else if (list->type == Cons) {
        SinObj* cons_obj = unwrap_cons(list, "apply * cons_obj");
        SinObj car = cons_obj[0];
        SinObj cdr = cons_obj[1];
        s64 carval = unwrap_int(&car, "apply * carval");
        s64 cdrval = unwrap_int(applyprim__42(&cdr), "apply * cdrval");
        return const_init_int(carval * cdrval);
    } else {
        fatal_err("apply * taking a non-list argument!");
    }
}

/// Returns a SinObj of type Int
SinObj* prim__47(SinObj* a, SinObj* b) { // /
    s64 a_val = unwrap_int(a, "/ a");
    s64 b_val = unwrap_int(b, "/ b");
    return const_init_int(a_val / b_val);
}

// SinObj applyprim__47(SinObj list) { // apply /
//     ASSERT_TYPE(list, Cons, "apply / must take a list as argument.");
//     {fatal_err("unimplemented...");}
//     // original header does not implement this...
// }

/// Takes 2 SinObj's of type Int
/// Returns a SinObj of type Bool
GEN_EXPECT2ARGLIST(applyprim__61, prim__61)
SinObj* prim__61(SinObj* a, SinObj* b) { // =
    if (unwrap_int(a, "= a") == unwrap_int(b, "= b")) {
        return const_init_true();
    } else {
        return const_init_false();
    }
}

/// Takes 2 SinObj's of type Int
/// Returns a SinObj of type Bool
SinObj* prim__60(SinObj* a , SinObj* b) { // <
    return make_predicate(unwrap_int(a, "< a") < unwrap_int(b, "< b"));
}

/// Takes 2 SinObj's of type Int
/// Returns a SinObj of type Bool
GEN_EXPECT2ARGLIST(applyprim__60_61, prim__60_61)
SinObj* prim__60_61(SinObj* a, SinObj* b) { // <=
    return make_predicate(unwrap_int(a, "<= a") <= unwrap_int(b, "<= b"));
}

/// Takes a SinObj of type Bool Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)
SinObj* prim_not(SinObj* b) { // not
    return make_predicate(unwrap_bool(b, "not") == false);
}



///// Hash Object


SinObj* map_to_sin(Map* m) {
    SinObj* ret = alloc(1);
    ret->type = Hash;
    ret->valueptr = reinterpret_cast<u64*>(m);
    return ret;
}

GEN_EXPECT2ARGLIST(applyprim_hash_45has_45key_63, prim_hash_45has_45key_63)
SinObj* prim_hash_45has_45key_63(SinObj* hash, SinObj* key) { // hash-has-key?
    Map* map = unwrap_hash(hash, "hash-has-key?");

    return make_predicate(map_has_key(map, key));
}

SinObj* applyprim_hash(SinObj* cur) { // apply hash
    Map* map = nullptr;

    while (cur->type != Null) {
        SinObj *car = alloc(1), *cdr = alloc(1),
                *cadr = alloc(1), *cddr = alloc(1);
        _get_both(cur, car, cdr);

        if (cdr->type != Cons) {
            fatal_err("Key not provided value in (hash)");
        }
        _get_both(cdr, cadr, cddr);

        map = map_insert(map, car, cadr);
        cur = cddr;
    }
    return map_to_sin(map);
}


GEN_EXPECT1ARGLIST(applyprim_hash_45keys, prim_hash_45keys)
SinObj* prim_hash_45keys(SinObj* hash) { // hash-keys
    Map* map = unwrap_hash(hash, "hash-keys");
    return map_keys(map);
}

/// hash-ref called in apply form can have the optional arg
SinObj* applyprim_hash_45ref(SinObj* cur) { // hash-ref
    SinObj* hash = nullptr;
    SinObj* key = nullptr;
    SinObj* default_obj = nullptr;

    if (cur->type == Cons) {
        SinObj hashcar, hashcdr;
        _get_both(cur, &hashcar, &hashcdr);
        hash = &hashcar;
        if (hashcdr.type == Cons) {
            SinObj keycar, keycdr;
            _get_both(&hashcdr, &keycar, &keycdr);
            key = &keycar;
            if (keycdr.type == Cons) {
                SinObj defcar, defcdr;
                _get_both(&keycdr, &defcar, &defcdr);
                default_obj = &defcar;
                if (defcdr.type == Null) {
                    return hash_ref_impl(hash, key, default_obj);
                } else {
                    fatal_err("Too many args given in hash-ref"); // TODO test this?
                }
            } else if (keycdr.type == Null) {
                return hash_ref_impl(hash, key, default_obj);
            }
        }
    }
    fatal_err("Bad Types Somewhere in hash-ref... good luck!")
}


/// hash-ref called in regular untagged form will not have the optional arg
/// as desugar checks if the key exists first.
/// So we should always have the key here.
SinObj* prim_hash_45ref(SinObj* hash, SinObj* key) {
    return hash_ref_impl(hash, key, nullptr);
}

SinObj* hash_ref_impl(SinObj* hash, SinObj* key, SinObj* default_obj) {
    Map* map = unwrap_hash(hash, "hash-ref");

    SinObj* val = map_get(map, key);
    if (val == nullptr) {
        if (default_obj == nullptr) {
            fatal_err("Map did not have requested key");
        }
        return default_obj;
    }
    return val;
}


GEN_EXPECT3ARGLIST(applyprim_hash_45set, prim_hash_45set)
SinObj* prim_hash_45set(SinObj* hash, SinObj* key, SinObj* val) { // hash-set
    Map* map = unwrap_hash(hash, "hash-set");

    Map* new_map = map_insert(map, key, val);

    return map_to_sin(new_map);
}

GEN_EXPECT1ARGLIST(applyprim_hash_63, prim_hash_63)
SinObj* prim_hash_63(SinObj* hash) { // hash?
    return make_predicate(hash->type == Hash);
}

GEN_EXPECT1ARGLIST(applyprim_hash_45count, prim_hash_45count)
SinObj* prim_hash_45count(SinObj* hash) { // hash-count
    Map* map = unwrap_hash(hash, "hash-count");
    u64 count = map_count(map);
    return const_init_int(static_cast<s64>(count));
}

}
