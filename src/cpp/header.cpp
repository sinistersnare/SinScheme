#define GC_DEBUG 1

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreserved-id-macro"
// gc.h has a macro __GC that is 'reserved', but i cant do anything about it.
#include "gc.h"
#pragma clang diagnostic pop

/*#include "hamt/hamt.h"*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "header.h"


extern "C" {

// TODO:
// * vector-* methods have an Int as its first value, but treat it as a u64.
//      Should there be a Len type that is pointer sized?
// * Implement The following:
//            set set->list set->remove set-union set-count set-first
//            hash hash-keys hash-ref hash-set


// called by `i32 @main();`
void start_program() {
    GC_INIT();
}

/// alloc amt SinObj's.
/// if you do alloc(sizeof(X)) then youre doing something wrong!!!
/// Use alloc when you need to allocate a SinObj with a pointer in it
/// This includes Cons, Vector, Hash, Closure, etc.
/// If you are allocating something without a pointer, such as Int,
/// you can still use this, but it is more efficient to use `alloc_atomic`.
SinObj* alloc(const u64 amt) {
    return reinterpret_cast<SinObj*>(GC_MALLOC(sizeof(SinObj) * amt));
}

/// alloc_atomic allocates atomic SinObj's.
/// This includes Int, Bool, and other non-pointer-containing objects.
/// If you need to allocate a pointer that contains an pointer, use `alloc()`
/// If you allocate something with a pointer (like a Vec) using this, you WILL
/// Get unintentionally free'd pointers! Dont do it!
SinObj* alloc_atomic(const u64 amt) {
    return reinterpret_cast<SinObj*>(GC_MALLOC_ATOMIC(sizeof(SinObj) * amt));
}

////////////// Objects

///// Closures

/// Returns a u64, which can be `inttoptr`d/`reinterpret`d
/// into a (void (SinObj*, SinObj*)) function pointer.
u64 closure_get_fn_part(SinObj* clo) {
    SinObj* clo_obj = unwrap_clo(clo, "closure_get_fn_part");
    // safe becuase right after using closure_get_fn_part
    // clo-app converts this a pointer and uses it.
    // so its not a hazard of getting collected or anything.
    return reinterpret_cast<u64>(clo_obj[0].valueptr);
}

// Returns a vector object internally,
// but it should be treated as an opaque object, accessed thru closure_env_* API
SinObj* closure_get_env_part(SinObj* clo) {
    SinObj* clo_obj = unwrap_clo(clo, "closure_get_env_part");
    return &clo_obj[1];
}

/// Closure Memory Layout
/// It is 2 SinObj laid out besides eachother.
// The first SinObj (index 0) is an Other type and it has a ptrvalue of the function pointer
// The second SinObj (index 1) is the vector that holds the environment (free variables).
SinObj* closure_alloc(const u64 amt_freevars, u64 cloval) {
    SinObj* clo_obj = alloc(2);
    SinObj* vec = prim_make_45vector(const_init_int(static_cast<s64>(amt_freevars)), const_init_int(0));
    SinObj clo_part;
    clo_part.type = Other; // TODO, layout of closures?
    // another reinterpret (the first being in proc->llvm) to make sure this doesnt get swept.
    clo_part.valueptr = reinterpret_cast<u64*>(cloval);
    clo_obj[0] = clo_part;
    clo_obj[1] = *vec;

    SinObj* ret = alloc(1);
    ret->type = Closure;
    ret->valueptr = reinterpret_cast<u64*>(clo_obj);
    return ret;
}

void closure_place_freevar(SinObj* clo, SinObj* freevar, u64 positionint) {
    // unwrap handles asserting
    SinObj* clo_obj = unwrap_clo(clo, "closure_place_freevar");

    SinObj* vec = &clo_obj[1];
    // TODO: probably want that Len type, or just make all lengths s64.
    SinObj* pos = const_init_int(static_cast<s64>(positionint));

    prim_vector_45set_33(vec, pos, freevar);
}

SinObj* closure_env_get(SinObj* clo, u64 pos) {
    // unwrap handles asserting
    SinObj* clo_obj = unwrap_clo(clo, "closure_env_get");
    SinObj* vec = &clo_obj[1];
    return prim_vector_45ref(vec, const_init_int(static_cast<s64>(pos)));
}

/*
SinObj* unwrap_hash(SinObj* hash_obj, const char* fn) {
    ASSERT_TYPE(*hash_obj, Hash, "unwrap_hash takes a Hash object! in fn %s", fn);
    return reinterpret_cast<SinObj*>(hash_obj->ptrvalue);
}*/

SinObj* unwrap_cons(SinObj* cons_obj, const char* fn) {
    ASSERT_TYPE(*cons_obj, Cons, "unwrap_cons takes a Cons object! in fn %s", fn);
    return reinterpret_cast<SinObj*>(cons_obj->valueptr);
}

SinObj* unwrap_vector(SinObj* vec_obj, const char* fn) {
    ASSERT_TYPE(*vec_obj, Vector, "unwrap_vector takes a Vector object! in fn %s", fn);
    return reinterpret_cast<SinObj*>(vec_obj->valueptr);
}

SinObj* unwrap_clo(SinObj* clo_obj, const char* fn) {
    ASSERT_TYPE(*clo_obj, Closure, "unwrap_clo takes a Closure object! in fn %s", fn);
    return reinterpret_cast<SinObj*>(clo_obj->valueptr);
}

s64 unwrap_int(SinObj* int_obj, const char* fn) {
    ASSERT_TYPE(*int_obj, Int, "unwrap_int takes an Int object! in fn %s", fn);
    return reinterpret_cast<s64>(int_obj->valueptr);
}

u64 unwrap_bool(SinObj* bool_obj, const char* fn) {
    ASSERT_TYPE(*bool_obj, Bool, "unwrap_bool takes a Bool object! in fn %s", fn);
    return reinterpret_cast<u64>(bool_obj->valueptr);
}

char* unwrap_str(SinObj* str_obj, const char* fn) {
    ASSERT_TYPE(*str_obj, Str, "unwrap_str takes a Str object! in fn %s", fn);
    return reinterpret_cast<char*>(str_obj->valueptr);
}

char* unwrap_sym(SinObj* sym_obj, const char* fn) {
    ASSERT_TYPE(*sym_obj, Sym, "unwrap_sym takes a Sym object! in fn %s", fn);
    return reinterpret_cast<char*>(sym_obj->valueptr);
}

u64 is_truthy_value(SinObj* obj) {
    return (obj->type == Bool && (unwrap_bool(obj, "is_truthy_value") == false))?false:true;
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
    ret->valueptr = NULL;
    ret->type = Void;
    return ret;
}

SinObj* const_init_null() {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = NULL;
    ret->type = Null;
    return ret;
}

SinObj* const_init_true() {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = reinterpret_cast<u64*>(true);
    ret->type = Bool;
    return ret;
}

SinObj* const_init_false() {
    SinObj* ret = alloc_atomic(1);
    ret->valueptr = reinterpret_cast<u64*>(false);
    ret->type = Bool;
    return ret;
}

SinObj* const_init_string(char* s) {
    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(s);
    ret->type = Str;
    return ret;
}

SinObj* const_init_symbol(char* s) {
    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(s);
    ret->type = Sym;
    return ret;
}

////// Printing

GEN_EXPECT1ARGLIST(applyprim_print,prim_print)
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
            printf("Unknown Boolean value: %llu", bv);
        }
    }
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
        printf("%lld", unwrap_int(obj, "prim_print_aux Int case."));
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
        printf("Hashes not currently supported!");
        break;
    case Set:
        printf("Sets not currently supported!");
        break;
    case Other:
        printf("(print v); unrecognized value %llu", reinterpret_cast<u64>(obj->valueptr));
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
    printf("#("); // sad face
    for (u64 i = 1; i <= len; i++) {
        if (i != 1) {
            printf(" ");
        }
        prim_print_aux(&vector[i]);
    }
    printf(")");

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

// TODO: use this more
void _get_both(SinObj* lst, SinObj* car, SinObj* cdr) {
    SinObj* cons_obj = unwrap_cons(lst, "_get_both");
    // this is safe because the lifetimes are the same
    // the lifetime of lst is >= than the lifetimes
    // of car and cdr.
    *car = cons_obj[0];
    *cdr = cons_obj[1];
}

SinObj* applyprim_vector(SinObj* curptr) { // apply vector
    SinObj cur = *curptr;
    // FIXME: arbitrarily large vectors.
    SinObj* buf = new SinObj[256];
    u64 i = 0;

    while (cur.type == Cons && i < 256) {
        SinObj car, cdr;
        _get_both(&cur, &car, &cdr);
        buf[i++] = car;
        cur = cdr;
    }
    if (i == 256 && cur.type != Null) {fatal_err("Vectors larger than 256 elements are unimplemented. Sorry!");}

    // i is amount of elements, + 1 is 1 object for size.
    SinObj* mem = alloc(i+1);

    SinObj size;
    size.valueptr = reinterpret_cast<u64*>(i);
    size.type = Int;
    mem[0] = size;

    for (u64 j = 1; j <= i; j++) {
        mem[j] = buf[j-1];
    }
    delete [] buf;

    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(mem);
    ret->type = Vector;
    return ret;
}

GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)
SinObj* prim_make_45vector(SinObj* length_obj, SinObj* fill) { // make-vector

    u64 len = static_cast<u64>(unwrap_int(length_obj, "make-vector"));
     // i is amount of elements, + 1 is 1 object for size.
    SinObj* vec = alloc(1 + len);

    vec[0] = *length_obj;
    for (u64 i = 1; i <= len; i++) {
        // not sure how safe it is to just put fill here a lot.
        vec[i] = *fill;
    }

    SinObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(vec);
    ret->type = Vector;
    return ret;
}


GEN_EXPECT1ARGLIST(applyprim_vector_45length, prim_vector_45length)
SinObj* prim_vector_45length(SinObj* vec) { // vector-length
    SinObj* vec_obj = unwrap_vector(vec, "prim_vector_45length");
    return &vec_obj[0];
}


GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)
SinObj* prim_vector_45ref(SinObj* vector, SinObj* pos) { // vector-ref

    SinObj* vec = unwrap_vector(vector, "vector-ref vec");
    u64 index_pos = static_cast<u64>(unwrap_int(pos, "vector-ref index_pos"));

    // + 1 becuase length is at index 0, so need to push everything up 1.
    return &vec[index_pos + 1];
}


GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)
SinObj* prim_vector_45set_33(SinObj* vec, SinObj* pos, SinObj* val) { // vector-set!

    SinObj* vector = unwrap_vector(vec, "vector-set!");
    u64 index_pos = static_cast<u64>(unwrap_int(pos, "apply vector-set! index_pos"));

    // + 1 becuase length is index 0, so need to push everything up 1.
    // unsure of safety of lifetimes just to deref val here.
    SinObj* newpos = alloc(1);
    newpos->valueptr = val->valueptr;
    newpos->type = val->type;
    vector[index_pos + 1] = *newpos;
    return const_init_void();
}


/// Returns a SinObj of type Void
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


// it is assumed that if this is called,
// a and b MUST both be Vector objects.
int vec_eq_helper(SinObj* a, SinObj* b) {
    SinObj* avec = unwrap_vector(a, "vec_eq_helper avec");
    SinObj* bvec = unwrap_vector(b, "vec_eq_helper bvec");
    // choose either vector it doesnt matter which we use for length.
    u64 len = static_cast<u64>(unwrap_int(&avec[0], "vec_eq_helper len"));

    // if the vectors are of different length, it will be caught on first iteration.
    // else, they it will work perfectly.
    for (u64 i=0; i <= len; i++) {
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
    return b?const_init_true():const_init_false();
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

/// Takes a Cons and returns its car
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)
SinObj* prim_car(SinObj* cons_obj) { // car
    SinObj* cons = unwrap_cons(cons_obj, "car");
    return &cons[0];
}


/// Takes a Cons and returns its cdr
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)
SinObj* prim_cdr(SinObj* cons_obj) { // cdr
    SinObj* cons = unwrap_cons(cons_obj, "cdr");
    return &cons[1];
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
SinObj* prim__60_61(SinObj* a, SinObj* b) { // <=
    return make_predicate(unwrap_int(a, "<= a") <= unwrap_int(b, "<= b"));
}

/// Takes a SinObj of type Bool Returns a SinObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)
SinObj* prim_not(SinObj* b) { // not
    return make_predicate(unwrap_bool(b, "not") == false);
}



/*
/// Hash Object
// hash hash-keys hash-ref hash-set

// TODO: make SinObj a C++-style class. :(
// Need to implement `operator==` and `u64 hash()`

/// Takes a Cons-list of things to put in the Hash.
Sinobj* applyprim_hash(SinObj* cur) {
    if (cur->type != Cons || cur->type != Null) {
        fatal_errf("applyprim_hash takes a list, but was given a %s", get_type_name(cur->type));
    }
    const hamt<SinObj, SinObj>* h = new ((hamt<SinObj,SinObj>*)GC_MALLOC(sizeof(hamt<SinObj,SinObj>))) hamt<SinObj,SinObj>();

    bool over = false;
    while (!over && cur->type == Cons) {
        SinObj car, cdr, cadr, cddr;

        _get_both(cur, &car, &cdr);
        if (cdr.type == Null) {
            fatal_err("applyprim_hash needs a value for its key!");
        }
        if (cdr.type == Cons) {
            _get_both(&cdr, &cadr, &cddr);
            // car and cadr are key and value, respectively.
            h = h->insert(&car, &cadr);
            cur = &cddr;
        } else {
            over = true;
        }
    }

    SinObj* ret = alloc(1);
    ret->type = Hash;
    ret->value = 0;
    ret->ptrvalue = reinterpret_cast<u64*>(h);
    return ret;
}
*/


} // end extern "C"


