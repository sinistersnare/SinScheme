
//#include "gc.h"
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "header.h"



extern "C" {


// TODO:
// * Does not look like >, >= is implemented?
// * vector-* methods have an Int as its first value, but treat it as a u64.
//      Should there be a Len type that is pointer sized?



SinObj* alloc(const u64 amt) {
    return new SinObj[amt];
    // return (SinObj*) GC_MALLOC(amt);
}

////////////// Objects

/// Returns a u64, which can be reinterpreted
/// into a (void (SinObj*, SinObj*)) function pointer.
u64 closure_get_fn_part(SinObj* clo) {
    SinObj* clo_obj = unwrap_clo(clo);
    return clo_obj[0].value;
}

// Returns a vector object internally,
// but it should be treated as an opaque object, accessed thru
// closure_env_* API
SinObj* closure_get_env_part(SinObj* clo) {
    SinObj* clo_obj = unwrap_clo(clo);
    return &clo_obj[1];
}

SinObj* closure_alloc(const u64 amt_freevars, u64 cloval) {
    SinObj* clo_obj = alloc(2);
    SinObj* vec = prim_make_45vector(const_init_int(amt_freevars), const_init_int(0));
    SinObj clo_part;
    clo_part.type = Other; // TODO, layout of closures?
    clo_part.value = cloval;
    clo_obj[0] = clo_part;
    clo_obj[1] = *vec;

    SinObj* ret = alloc(1);
    ret->type = Closure;
    ret->value = reinterpret_cast<u64>(clo_obj);

    return ret;
}

void closure_place_freevar(SinObj* clo, SinObj* freevar, u64 positionint) {
    // unwrap handles asserting
    SinObj* clo_obj = unwrap_clo(clo);

    SinObj* vec = &clo_obj[1];
    // TODO: probably want that Len type, or just make all lengths s64.
    SinObj* pos = const_init_int((s64) positionint);
    prim_vector_45set_33(vec, pos, freevar);
}

SinObj* closure_env_get(SinObj* clo, u64 pos) {
    // unwrap handles asserting
    SinObj* clo_obj = unwrap_clo(clo);

    SinObj* vec = &clo_obj[1];
    return prim_vector_45ref(vec, const_init_int((s64) pos));
}




SinObj* unwrap_cons(SinObj* cons_obj) {
    ASSERT_TYPE(*cons_obj, Cons, "unwrap_cons takes a Cons object!");
    return reinterpret_cast<SinObj*>(cons_obj->value);
}

SinObj* unwrap_vector(SinObj* vec_obj) {
    ASSERT_TYPE(*vec_obj, Vector, "unwrap_vector takes a Vector object!");
    return reinterpret_cast<SinObj*>(vec_obj->value);
}

SinObj* unwrap_clo(SinObj* clo_obj) {
    ASSERT_TYPE(*clo_obj, Closure, "unwrap_clo takes a Closure object!");
    return reinterpret_cast<SinObj*>(clo_obj->value);
}

s64 unwrap_int(SinObj* int_obj) {
    ASSERT_TYPE(*int_obj, Int, "unwrap_int takes an Int object!");
    return static_cast<s64>(int_obj->value);
}

u64 unwrap_bool(SinObj* bool_obj) {
    ASSERT_TYPE(*bool_obj, Bool, "unwrap_bool takes a Bool object!");
    return bool_obj->value;
}

char* unwrap_str(SinObj* str_obj) {
    ASSERT_TYPE(*str_obj, Str, "unwrap_str takes a Str object!");
    return reinterpret_cast<char*>(str_obj->value);
}

char* unwrap_sym(SinObj* sym_obj) {
    ASSERT_TYPE(*sym_obj, Sym, "unwrap_sym takes a Sym object!");
    return reinterpret_cast<char*>(sym_obj->value);
}

u64 is_truthy_value(SinObj* obj) {
    if (obj->type == Bool && (unwrap_bool(obj) == 0)) {
        return 0;
    }
    return 1;
}

// TODO: global objects for constant values like #t, #f or '() ?

SinObj* const_init_int(s64 i) {
    SinObj* ret = alloc(1);
    ret->value = static_cast<u64>(i);
    ret->type = Int;
    return ret;
}

SinObj* const_init_void() {
    SinObj* ret = alloc(1);
    ret->value = 0;
    ret->type = Void;
    return ret;
}

SinObj* const_init_null() {
    SinObj* ret = alloc(1);
    ret->value = 0;
    ret->type = Null;
    return ret;
}
SinObj* const_init_true() {
    SinObj* ret = alloc(1);
    ret->value = 1;
    ret->type = Bool;
    return ret;
}

SinObj* const_init_false() {
    SinObj* ret = alloc(1);
    ret->value = 0;
    ret->type = Bool;
    return ret;
}

SinObj* const_init_string(const char* s) {
    SinObj* ret = alloc(1);
    ret->value = reinterpret_cast<u64>(s);
    ret->type = Str;
    return ret;
}

SinObj* const_init_symbol(const char* s) {
    SinObj* ret = alloc(1);
    ret->value = reinterpret_cast<u64>(s);
    ret->type = Sym;
    return ret;
}


////// Printing
SinObj* print_cons(SinObj*);
SinObj* print_vector(SinObj*);

SinObj* prim_print_aux(SinObj* obj) {
    SinType typ = obj->type;
    u64 val = obj->value;

    switch (typ) {
    case Void:
        printf("#<void>");
        break;
    case Null:
        printf("()");
        break;
    case Bool:
        if (val == 0) {
            printf("#f");
        } else if (val == 1) {
            printf("#t");
        } else {
            printf("Unknown Boolean value: %llu", val);
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
        printf("%lld", unwrap_int(obj));
        break;
    case Str:
        printf("%s", unwrap_str(obj));
        break;
    case Sym:
        printf("%s", unwrap_sym(obj));
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
        printf("(print v); unrecognized value %llu", val);
        break;
    // default:
    //     printf("(print v); unrecognized value %llu", val);
    //     break;
    }
    return const_init_void();
}

SinObj* prim_print(SinObj* obj) {
    SinType typ = obj->type;

    switch (typ) {
    case Null:
        printf("'()");
        break;
    case Sym:
        printf("'%s", unwrap_sym(obj));
        break;
    default:
        prim_print_aux(obj);
        break;
    }
    return const_init_void();
}

GEN_EXPECT1ARGLIST(applyprim_print,prim_print)


SinObj* print_cons(SinObj* obj) {

    SinObj* cons = unwrap_cons(obj);
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
    default:
        printf(" . ");
        prim_print_aux(cdr);
        break;
    }

    return const_init_void();
}


SinObj* print_vector(SinObj* obj) {
    SinObj* vector = unwrap_vector(obj);
    u64 len = vector[0].value;
    printf("#(");
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
    // default:
    //     return "UNKNOWN??";
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

// TODO: use this more
void _get_both(SinObj lst, SinObj* car, SinObj* cdr) {
    ASSERT_TYPE(lst, Cons, "_get_both only takes a Cons!");
    SinObj* cons_obj = unwrap_cons(&lst);
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
        _get_both(cur, &car, &cdr);
        buf[i++] = car;
        cur = cdr;
    }
    if (i == 256 && cur.type != Null) {fatal_err("Vectors larger than 256 elements are unimplemented. Sorry!");}

    SinObj* mem = alloc(i+1);
    SinObj size;
    size.value = i;
    size.type = Int;
    mem[0] = size;

    for (u64 j = 1; j <= i; j++) {
        mem[j] = buf[j-1];
    }
    delete [] buf;

    SinObj* ret = alloc(1);
    ret->value = reinterpret_cast<u64>(mem);
    ret->type = Vector;
    return ret;
}

// TODO
SinObj* prim_make_45vector(SinObj* length_obj, SinObj* fill) { // make-vector
    ASSERT_TYPE(*length_obj, Int, "First argument to make-vector must be an Int.");

    u64 len = length_obj->value;
    SinObj* vec = alloc(1 + len);

    vec[0] = *length_obj;
    for (u64 i = 1; i <= len; i++) {
        // not sure how safe it is to just put fill here a lot.
        vec[i] = *fill;
    }

    SinObj* ret = alloc(1);
    ret->value = reinterpret_cast<u64>(vec);
    ret->type = Vector;
    return ret;
}

GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)


SinObj* prim_vector_45length(SinObj* vec) { // vector-length
    ASSERT_TYPE(*vec, Vector, "First argument to vector-length must be a Vector.");

    SinObj* vec_obj = unwrap_vector(vec);
    return &vec_obj[0];
}

GEN_EXPECT1ARGLIST(applyprim_vector_45length, prim_vector_45length)

SinObj* prim_vector_45ref(SinObj* vector, SinObj* pos) { // vector-ref
    ASSERT_TYPE(*pos, Int, "Second argument to vector-ref must be an Int.");

    SinObj* vec = unwrap_vector(vector);

    // just do ->value becuase we want u64 and not s64.
    u64 index_pos = pos->value;

    // TODO: at runtime, wrap vector-ref calls with this.
    // (vec[0] is length)
    if (index_pos > vector[0].value) {fatal_err("Vector indexing out of bounds: TODO: at top-level");}

    // + 1 becuase length is at index 0, so need to push everything up 1.
    return &vec[index_pos + 1];
}

GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)


SinObj* prim_vector_45set_33(SinObj* vec, SinObj* pos, SinObj* val) { // vector-set!
    ASSERT_TYPE(*pos, Int, "Second argument to vector-set! must be an Int.");

    SinObj* vector = unwrap_vector(vec);
    // just do ->value becuase we want u64 and not s64.
    u64 index_pos = pos->value;

    // TODO: at runtime, wrap vector-ref calls with this.
    // (vec[0] is length)
    if (index_pos > vector[0].value) {fatal_err("Vector indexing out of bounds: TODO: at top-level");}

    // + 1 becuase length is index 0, so need to push everything up 1.
    vector[index_pos + 1] = *val; // unsure of safety of lifetimes just to deref val here.
    return const_init_void();
}

GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)

/// Returns a SinObj of type Void
SinObj* prim_void() { // void
    return const_init_void();
}


///// eq? eqv? equal?

/// Returns a SinObj of type Bool
// CURRENTLY ONLY TAKES INTS, TODO
SinObj* prim_eq_63(SinObj* a, SinObj* b) { // eq?
    if (b->type != a->type) return const_init_false();

    // TODO: is this enough?
    // probably need special checks? Or for other types.
    SinObj* ret = alloc(1);
    ret->value = unwrap_int(a) == unwrap_int(b);
    ret->type = Bool;
    return ret;
}

GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)

/// Returns a SinObj of type Bool
SinObj* prim_eqv_63(SinObj* a, SinObj* b) { // eqv?
    return prim_eq_63(a, b); // TODO
}

GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)

/// Returns a SinObj of type Bool
SinObj* prim_equal_63(SinObj* a, SinObj* b) { // equal?
    return prim_eq_63(a, b); // TODO
}

GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)

////// Other Predicates


// TODO: return #t/#f constants here?

/// Returns a SinObj of type Bool
SinObj* prim_number_63(SinObj* n) { // number?
    // TODO: floats not implemented lul.
    SinObj* ret = alloc(1);
    ret->value = n->type == Int;
    ret->type = Bool;
    return ret;
}

GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)

/// Returns a SinObj of type Bool
SinObj* prim_integer_63(SinObj* n) { // integer?
    SinObj* ret = alloc(1);
    ret->value = n->type == Int;
    ret->type = Bool;
    return ret;
}

GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)

/// Returns a SinObj of type Bool
SinObj* prim_void_63(SinObj* obj) { // void?
    SinObj* ret = alloc(1);
    ret->value = obj->type == Void;
    ret->type = Bool;
    return ret;
}

GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)

/// Returns a SinObj of type Bool
SinObj* prim_procedure_63(SinObj* obj) { // procedure?
    SinObj* ret = alloc(1);
    ret->value = obj->type == Closure;
    ret->type = Bool;
    return ret;
}

GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)

// null? cons? cons car cdr

/// Returns a SinObj of type Bool
SinObj* prim_null_63(SinObj* obj) { // null?
    SinObj* ret = alloc(1);
    ret->value = obj->type == Null;
    ret->type = Bool;
    return ret;
}

GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)

/// Returns a SinObj of type Bool
SinObj* prim_cons_63(SinObj* obj) { // cons?
    SinObj* ret = alloc(1);
    ret->value = obj->type == Cons;
    ret->type = Bool;
    return ret;
}

GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)


/// Returns a SinObj of type Cons
SinObj* prim_cons(SinObj* car, SinObj* cdr) { // cons
    SinObj* ptr = alloc(2 * sizeof(SinObj));
    // unsure of safety of this.
    ptr[0] = *car;
    ptr[1] = *cdr;
    SinObj* ret = alloc(1);
    ret->value = reinterpret_cast<u64>(ptr);
    // ret.value = (u64) ptr;
    ret->type = Cons;
    return ret;
}

// GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)

/// Takes a Cons and returns its car
SinObj* prim_car(SinObj* cons_obj) { // car
    SinObj* cons = unwrap_cons(cons_obj);

    return &cons[0];
}

GEN_EXPECT1ARGLIST(applyprim_car, prim_car)

/// Takes a Cons and returns its cdr
SinObj* prim_cdr(SinObj* cons_obj) { // cdr
    SinObj* cons = unwrap_cons(cons_obj);

    return &cons[1];
}

GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)

/// Returns a SinObj of type Int
SinObj* prim__43(SinObj* a, SinObj* b) { // +
    s64 a_val = unwrap_int(a);
    s64 b_val = unwrap_int(b);

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
        _get_both(*cur, &car, &cdr);
        ASSERT_TYPE(car, Int, "+ takes only Int arguments.");
        final += unwrap_int(&car);
        cur = &cdr;
    }
    return const_init_int(final);
}

/// Returns a SinObj of type Int
SinObj* prim__45(SinObj* a, SinObj* b) { // -
    // a - b
    s64 a_val = unwrap_int(a);
    s64 b_val = unwrap_int(b);

    return const_init_int(a_val - b_val);
}

SinObj* applyprim__45(SinObj* list) { // apply -
    SinObj* cons_obj = unwrap_cons(list);
    SinObj car = cons_obj[0];
    SinObj cdr = cons_obj[1];

    ASSERT_TYPE(car, Int, "First argument to - must be an Int");
    s64 carval = unwrap_int(&car);

    if (cdr.type == Null) {
        return const_init_int(0 - carval);
    }

    s64 final = carval;
    SinObj cur = cdr;
    while (cur.type != Null) {
        SinObj cur_car, cur_cdr;

        _get_both(cur, &cur_car, &cur_cdr);
        ASSERT_TYPE(cur_car, Int, "apply - only takes Int arguments.");
        final -= unwrap_int(&cur_car);
        cur = cur_cdr;
    }
    return const_init_int(final);
}


/// Returns a SinObj of type Int
SinObj* prim__42(SinObj* a, SinObj* b) { // *
    // a - b
    return const_init_int(unwrap_int(a) * unwrap_int(b));
}

SinObj* applyprim__42(SinObj* list) { // apply *
    if (list->type == Null) {
        return const_init_int(1);
    } else if (list->type == Cons) {
        SinObj* cons_obj = unwrap_cons(list);
        SinObj car = cons_obj[0];
        SinObj cdr = cons_obj[1];
        ASSERT_TYPE(car, Int, "* Only takes Integer arguments.");
        s64 carval = unwrap_int(&car);
        s64 cdrval = unwrap_int(applyprim__42(&cdr));
        return const_init_int(carval * cdrval);
    } else {
        fatal_err("applyprim__42 taking a non-list argument!");
    }
}

/// Returns a SinObj of type Int
SinObj* prim__47(SinObj* a, SinObj* b) { // /
    s64 a_val = unwrap_int(a);
    s64 b_val = unwrap_int(b);
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
    if (unwrap_int(a) == unwrap_int(b)) {
        return const_init_true();
    } else {
        return const_init_false();
    }
}

/// Takes 2 SinObj's of type Int
/// Returns a SinObj of type Bool
SinObj* prim__60(SinObj* a , SinObj* b) { // <
    if (unwrap_int(a) < unwrap_int(b)) {
        return const_init_true();
    } else {
        return const_init_false();
    }
}

/// Takes 2 SinObj's of type Int
/// Returns a SinObj of type Bool
SinObj* prim__60_61(SinObj* a, SinObj* b) { // <=
    if (unwrap_int(a) <= unwrap_int(b)) {
        return const_init_true();
    } else {
        return const_init_false();
    }
}

/// Takes a SinObj of type Bool Returns a SinObj of type Bool
SinObj* prim_not(SinObj* b) { // not
    if (b->value == 0) {
        return const_init_true();
    } else {
        return const_init_false();
    }
}

GEN_EXPECT1ARGLIST(applyprim_not, prim_not)












}
