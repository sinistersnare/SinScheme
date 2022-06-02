#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

extern "C" {

#define ASSERT_TYPE(obj,expected,msg,...) if (((obj).type) != (expected)) {fatal_errf(msg, __VA_ARGS__);}

#define fatal_err(format) { \
        printf("Fatal library run-time error: "); \
        printf(format); \
        printf("\n"); \
        exit(1); \
    }

#define fatal_errf(format, ...) { \
        printf("Fatal library run-time error: "); \
        printf(format, __VA_ARGS__); \
        printf("\n"); \
        exit(1); \
    }


#define GEN_EXPECT0ARGLIST(newname, fn_name) \
    SinObj* newname(SinObj* lst) { \
        if (lst->type != Null) {fatal_errf("Expected an empty list but got something else for function '%s'", #newname);} \
        return fn_name(); \
    }

#define GEN_EXPECT1ARGLIST(newname, fn_name) \
    SinObj* newname(SinObj* lst) { \
        if (lst->type != Cons) {fatal_errf("Expected cons but got something else for function '%s'", #newname);} \
        SinObj* cons_obj = unwrap_cons(lst, #newname); \
        SinObj car = cons_obj[0]; \
        SinObj cdr = cons_obj[1]; \
        if (cdr.type != Null) {fatal_errf("function '%s' only takes 1 argument.", #newname );} \
        return fn_name(&car); \
    }

#define GEN_EXPECT2ARGLIST(newname, fn_name) \
    SinObj* newname(SinObj* lst) { \
        if (lst->type != Cons) {fatal_errf("Expected cons but got something else for function '%s'", #newname);} \
        SinObj* cons_obj = unwrap_cons(lst, #newname); \
        SinObj car = cons_obj[0]; \
        SinObj cdr = cons_obj[1]; \
        if (cdr.type != Cons) {fatal_errf("Function '%s' expected 2 arguments but got 1.", #newname );} \
        SinObj* cdr_obj = unwrap_cons(&cdr, #newname); \
        SinObj cadr = cdr_obj[0]; \
        SinObj cddr = cdr_obj[1]; \
        if (cddr.type != Null) {fatal_errf("Function '%s' only takes 2 arguments.", #newname );} \
        return fn_name(&car, &cadr); \
    }

#define GEN_EXPECT3ARGLIST(newname, fn_name) \
    SinObj* newname(SinObj* lst) { \
        if (lst->type != Cons) {fatal_errf("Expected cons but got something else for function '%s'", #newname);} \
        SinObj* cons_obj = unwrap_cons(lst, #newname); \
        SinObj car = cons_obj[0]; \
        SinObj cdr = cons_obj[1]; \
        if (cdr.type != Cons) {fatal_errf("Function '%s' expected 3 arguments but got 1.", #newname );} \
        SinObj* cdr_obj = unwrap_cons(&cdr, #newname); \
        SinObj cadr = cdr_obj[0]; \
        SinObj cddr = cdr_obj[1]; \
        if (cddr.type != Cons) {fatal_errf("Function '%s' expected 3 arguments but got 2.", #newname );} \
        SinObj* cddr_object = unwrap_cons(&cddr, #newname); \
        SinObj caddr = cddr_object[0]; \
        SinObj cdddr = cddr_object[1]; \
        if (cdddr.type != Null) {fatal_errf("Function '%s' only takes 3 arguments.", #newname );} \
        return fn_name(&car, &cadr, &caddr); \
    }


#define STACK_SIZE 4096 // new stack record size


typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;

typedef void (*SinFunc)();

typedef enum { Void=0, Null, Bool
             , Closure, Cons, Int
             , Str, Sym, Vector
             , Hash, Set, Other
             , Continuation} SinType;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpadded"
typedef struct SinObj {
    void* valueptr;
    SinType type;
} SinObj;
#pragma clang diagnostic pop

typedef struct SinRecord {
    SinObj** stack;
    struct SinRecord* next;
    s64 size;
    /// Where we go to in the stack
    /// when reinstantiating this record.
    s64 return_fp_offset;
    /// Where we go to in the code-stream
    SinFunc return_address;
} SinRecord;

SinObj* alloc(const u64 amt);
SinObj* alloc_atomic(const u64 amt);

const char* get_type_name(SinType);
SinObj* const_init_int(s64);
SinObj* const_init_void();
SinObj* const_init_null();
SinObj* const_init_true();
SinObj* const_init_false();
SinObj* const_init_string(char*);
SinObj* const_init_symbol(char*);

SinObj* unwrap_cons(SinObj*, const char*);
SinObj* unwrap_vector(SinObj*, const char*);
SinObj* unwrap_clo(SinObj*, const char*);
s64 unwrap_int(SinObj*, const char*);
char* unwrap_str(SinObj*, const char*);
char* unwrap_sym(SinObj*, const char*);
u64 unwrap_bool(SinObj*, const char*);


void _get_both(SinObj*, SinObj*, SinObj*);

// utility
void debug_output_registers(SinRecord** srr, SinObj*** fpr, SinObj*** spr, SinObj** retr);
int eq_helper(SinObj*, SinObj*);
int cons_eq_helper(SinObj* a, SinObj* b);
int vec_eq_helper(SinObj* a, SinObj* b);
void bounds_check(SinObj*, s64);
u64 _get_vector_length(SinObj*);
SinObj* hash_ref_impl(SinObj*, SinObj*, SinObj*);
const char* get_type_name(SinType);
void _get_both(SinObj*, SinObj*, SinObj*);
bool is_truthy_value(SinObj*);
SinObj* prim_print_aux(SinObj*);
SinObj* make_predicate(bool b);

SinRecord* make_record(SinRecord* cur_stack_record, SinObj*** spr);
SinRecord* split_record(SinRecord* old_stack_record, SinObj** new_segment_base,
                        s64 cutoff_slots, s64 ret_fp_slots, SinFunc ret_addr);
SinObj* make_continuation_closure(SinRecord* cont, SinFunc cont_func_loc);
SinObj* closure_alloc(const s64 amt_freevars, SinFunc fptr);

void closure_place_freevar(SinObj* clo, SinObj* freevar, s64 pos);
void* closure_get_env_part(SinObj* clo);
SinFunc closure_get_fn_part(SinObj* clo);
SinObj* closure_env_get(SinObj* clo, s64 pos);
SinFunc handle_continuation_function(SinRecord** srr, SinObj*** fpr,
                                     SinObj*** spr, SinObj** retr);

bool check_for_overflow(SinObj*** fpr, SinObj*** spr, s64 caller_slots);
SinFunc handle_underflow(SinRecord** srr, SinObj*** fpr, SinObj*** spr);
void handle_overflow(SinRecord** srr, SinObj*** fpr, SinObj*** spr,
                     SinFunc underflow_loc, s64 num_slots_in_overflower);
bool callcc_at_base(SinRecord** srr, SinObj*** fpr);


void start_runtime(SinRecord** srr, SinObj*** fpr, SinObj*** spr);

// Prims //

// primitives in no particular order...
SinObj* prim_car(SinObj* cons_obj);
SinObj* applyprim_car(SinObj* cur);
SinObj* prim__43(SinObj* a, SinObj* b);
SinObj* applyprim__43(SinObj* cur);
SinObj* prim_make_45vector(SinObj*, SinObj*);
SinObj* applyprim_make_45vector(SinObj*);
SinObj* print_hash(SinObj*);
SinObj* print_cons(SinObj*);
SinObj* print_vector(SinObj*);
SinObj* prim_display(SinObj*);
SinObj* applyprim_display(SinObj*);
SinObj* prim_print(SinObj*);
SinObj* applyprim_print(SinObj*);
SinObj* prim_println(SinObj*);
SinObj* applyprim_println(SinObj*);
SinObj* prim_equal_63(SinObj*,SinObj*);
SinObj* applyprim_equal_63(SinObj*);
SinObj* prim_number_63(SinObj*);
SinObj* applyprim_number_63(SinObj*);
SinObj* prim_integer_63(SinObj*);
SinObj* applyprim_integer_63(SinObj*);
SinObj* prim_boolean_63(SinObj*);
SinObj* applyprim_boolean_63(SinObj*);
SinObj* prim_cdr(SinObj*);
SinObj* applyprim_cdr(SinObj*);
SinObj* prim__42(SinObj*, SinObj*);
SinObj* applyprim__42(SinObj*);
SinObj* prim__45(SinObj*, SinObj*);
SinObj* applyprim__45(SinObj*);
SinObj* prim_cdr(SinObj*);
SinObj* applyprim_cdr(SinObj*);
SinObj* prim_cons_63(SinObj*);
SinObj* applyprim_cons_63(SinObj*);
SinObj* prim_cons(SinObj*, SinObj*);
SinObj* applyprim_cons(SinObj*);
SinObj* prim_procedure_63(SinObj*);
SinObj* applyprim_procedure_63(SinObj*);
SinObj* prim_null_63(SinObj*);
SinObj* applyprim_null_63(SinObj*);
SinObj* prim_eqv_63(SinObj*, SinObj*);
SinObj* applyprim_eqv_63(SinObj*);
SinObj* prim_void_63(SinObj*);
SinObj* applyprim_void_63(SinObj*);
SinObj* prim_eq_63(SinObj*, SinObj*);
SinObj* applyprim_eq_63(SinObj*);
SinObj* prim_void();
SinObj* applyprim_void(SinObj*);
SinObj* prim_halt(SinObj*);
SinObj* applyprim_halt(SinObj*);
SinObj* prim_vector_63(SinObj*);
SinObj* applyprim_vector_63(SinObj*);
SinObj* prim_vector_45length(SinObj*);
SinObj* applyprim_vector_45length(SinObj*);
SinObj* prim_vector_45set_33(SinObj*, SinObj*, SinObj*);
SinObj* applyprim_vector_45set_33(SinObj*);
SinObj* prim_vector_45ref(SinObj*, SinObj*);
SinObj* applyprim_vector_45ref(SinObj*);
SinObj* applyprim_vector(SinObj*);
SinObj* prim__45(SinObj*, SinObj*);
SinObj* applyprim__45(SinObj*);
SinObj* applyprim__42(SinObj*);
SinObj* prim__47(SinObj*, SinObj*);
SinObj* applyprim__61(SinObj*);
SinObj* prim__61(SinObj*, SinObj*);
SinObj* prim__60(SinObj* , SinObj*);
SinObj* applyprim__60_61(SinObj*);
SinObj* prim__60_61(SinObj*, SinObj*);
SinObj* prim_not(SinObj*);
SinObj* applyprim_not(SinObj*);

SinObj* applyprim_hash(SinObj*);
SinObj* prim_hash_45has_45key_63(SinObj*, SinObj*);
SinObj* applyprim_hash_45has_45key_63(SinObj*);
SinObj* prim_hash_45keys(SinObj*);
SinObj* applyprim_hash_45keys(SinObj*);
SinObj* prim_hash_45ref(SinObj*, SinObj*);
SinObj* applyprim_hash_45ref(SinObj*);
SinObj* prim_hash_45set(SinObj*, SinObj*, SinObj*);
SinObj* applyprim_hash_45set(SinObj*);
SinObj* prim_hash_63(SinObj*);
SinObj* applyprim_hash_63(SinObj*);
SinObj* prim_hash_45count(SinObj*);
SinObj* applyprim_hash_45count(SinObj*);

}
