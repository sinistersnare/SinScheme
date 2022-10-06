; ModuleID = './src/cpp/runtime.cpp'
source_filename = "./src/cpp/runtime.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Map = type { %struct.Map*, %struct.SinObj*, %struct.SinObj* }
%struct.SinObj = type { i8*, i32 }
%struct.SinRecord = type { %struct.SinObj**, %struct.SinRecord*, i64, i64, void ()* }

@.str = private unnamed_addr constant [19 x i8] c"Debug number: %ld\0A\00", align 1
@.str.1 = private unnamed_addr constant [18 x i8] c"Register Values:\0A\00", align 1
@.str.2 = private unnamed_addr constant [38 x i8] c"srr:`%p`,fpr:`%p`,spr:`%p`,retr:`%p`\0A\00", align 1
@.str.3 = private unnamed_addr constant [16 x i8] c"Current @retr: \00", align 1
@.str.4 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.5 = private unnamed_addr constant [10 x i8] c"_get_both\00", align 1
@.str.6 = private unnamed_addr constant [22 x i8] c"closure_place_freevar\00", align 1
@.str.7 = private unnamed_addr constant [21 x i8] c"closure_get_env_part\00", align 1
@.str.8 = private unnamed_addr constant [20 x i8] c"closure_get_fn_part\00", align 1
@.str.9 = private unnamed_addr constant [23 x i8] c"descent == full->stack\00", align 1
@.str.10 = private unnamed_addr constant [22 x i8] c"./src/cpp/runtime.cpp\00", align 1
@__PRETTY_FUNCTION__.handle_overflow = private unnamed_addr constant [73 x i8] c"void handle_overflow(SinRecord **, SinObj ***, SinObj ***, SinFunc, s64)\00", align 1
@.str.11 = private unnamed_addr constant [31 x i8] c"Fatal library run-time error: \00", align 1
@.str.12 = private unnamed_addr constant [55 x i8] c"Expected cons but got something else for function '%s'\00", align 1
@.str.13 = private unnamed_addr constant [14 x i8] c"applyprim_car\00", align 1
@.str.14 = private unnamed_addr constant [37 x i8] c"function '%s' only takes 1 argument.\00", align 1
@.str.15 = private unnamed_addr constant [4 x i8] c"car\00", align 1
@.str.16 = private unnamed_addr constant [4 x i8] c"+ a\00", align 1
@.str.17 = private unnamed_addr constant [4 x i8] c"+ b\00", align 1
@.str.18 = private unnamed_addr constant [37 x i8] c"Expected Cons in apply +, but got %s\00", align 1
@.str.19 = private unnamed_addr constant [14 x i8] c"apply + final\00", align 1
@.str.20 = private unnamed_addr constant [24 x i8] c"applyprim_make_45vector\00", align 1
@.str.21 = private unnamed_addr constant [46 x i8] c"Function '%s' expected 2 arguments but got 1.\00", align 1
@.str.22 = private unnamed_addr constant [38 x i8] c"Function '%s' only takes 2 arguments.\00", align 1
@.str.23 = private unnamed_addr constant [12 x i8] c"make-vector\00", align 1
@.str.24 = private unnamed_addr constant [16 x i8] c"closure_env_get\00", align 1
@.str.25 = private unnamed_addr constant [49 x i8] c"unwrap_hash takes a Hash object! Got %d in fn %s\00", align 1
@.str.26 = private unnamed_addr constant [49 x i8] c"unwrap_cons takes a Cons object! Got %d in fn %s\00", align 1
@.str.27 = private unnamed_addr constant [53 x i8] c"unwrap_vector takes a Vector object! Got %d in fn %s\00", align 1
@.str.28 = private unnamed_addr constant [51 x i8] c"unwrap_clo takes a Closure object! Got %d in fn %s\00", align 1
@.str.29 = private unnamed_addr constant [48 x i8] c"unwrap_int takes an Int object! Got %d in fn %s\00", align 1
@.str.30 = private unnamed_addr constant [49 x i8] c"unwrap_bool takes a Bool object! Got %d in fn %s\00", align 1
@.str.31 = private unnamed_addr constant [47 x i8] c"unwrap_str takes a Str object! Got %d in fn %s\00", align 1
@.str.32 = private unnamed_addr constant [47 x i8] c"unwrap_sym takes a Sym object! Got %d in fn %s\00", align 1
@.str.33 = private unnamed_addr constant [16 x i8] c"is_truthy_value\00", align 1
@.str.34 = private unnamed_addr constant [18 x i8] c"applyprim_display\00", align 1
@.str.35 = private unnamed_addr constant [16 x i8] c"applyprim_print\00", align 1
@.str.36 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.37 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.38 = private unnamed_addr constant [21 x i8] c"prim_print Sym case.\00", align 1
@.str.39 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.40 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.41 = private unnamed_addr constant [18 x i8] c"applyprim_println\00", align 1
@.str.42 = private unnamed_addr constant [8 x i8] c"#<void>\00", align 1
@.str.43 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.44 = private unnamed_addr constant [25 x i8] c"prim_print_aux bool case\00", align 1
@.str.45 = private unnamed_addr constant [3 x i8] c"#f\00", align 1
@.str.46 = private unnamed_addr constant [3 x i8] c"#t\00", align 1
@.str.47 = private unnamed_addr constant [27 x i8] c"Unknown Boolean value: %lu\00", align 1
@.str.48 = private unnamed_addr constant [16 x i8] c"#<continuation>\00", align 1
@.str.49 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.50 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.51 = private unnamed_addr constant [4 x i8] c"%ld\00", align 1
@.str.52 = private unnamed_addr constant [25 x i8] c"prim_print_aux Int case.\00", align 1
@.str.53 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.54 = private unnamed_addr constant [25 x i8] c"prim_print_aux Str case.\00", align 1
@.str.55 = private unnamed_addr constant [25 x i8] c"prim_print_aux Sym case.\00", align 1
@.str.56 = private unnamed_addr constant [7 x i8] c"#hash(\00", align 1
@.str.57 = private unnamed_addr constant [30 x i8] c"Sets not currently supported!\00", align 1
@.str.58 = private unnamed_addr constant [34 x i8] c"(print v); unrecognized value %lu\00", align 1
@.str.59 = private unnamed_addr constant [11 x i8] c"print_cons\00", align 1
@.str.60 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str.61 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.62 = private unnamed_addr constant [13 x i8] c"print_vector\00", align 1
@.str.63 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.64 = private unnamed_addr constant [11 x i8] c"print_hash\00", align 1
@.str.65 = private unnamed_addr constant [5 x i8] c"Void\00", align 1
@.str.66 = private unnamed_addr constant [5 x i8] c"Null\00", align 1
@.str.67 = private unnamed_addr constant [5 x i8] c"Bool\00", align 1
@.str.68 = private unnamed_addr constant [13 x i8] c"Continuation\00", align 1
@.str.69 = private unnamed_addr constant [8 x i8] c"Closure\00", align 1
@.str.70 = private unnamed_addr constant [5 x i8] c"Cons\00", align 1
@.str.71 = private unnamed_addr constant [4 x i8] c"Int\00", align 1
@.str.72 = private unnamed_addr constant [7 x i8] c"String\00", align 1
@.str.73 = private unnamed_addr constant [7 x i8] c"Symbol\00", align 1
@.str.74 = private unnamed_addr constant [7 x i8] c"Vector\00", align 1
@.str.75 = private unnamed_addr constant [5 x i8] c"Hash\00", align 1
@.str.76 = private unnamed_addr constant [4 x i8] c"Set\00", align 1
@.str.77 = private unnamed_addr constant [6 x i8] c"Other\00", align 1
@.str.78 = private unnamed_addr constant [15 x i8] c"applyprim_halt\00", align 1
@.str.79 = private unnamed_addr constant [18 x i8] c"get_vector_length\00", align 1
@.str.80 = private unnamed_addr constant [59 x i8] c"Vectors larger than 256 elements are unimplemented. Sorry!\00", align 1
@.str.81 = private unnamed_addr constant [20 x i8] c"applyprim_vector_63\00", align 1
@.str.82 = private unnamed_addr constant [26 x i8] c"applyprim_vector_45length\00", align 1
@.str.83 = private unnamed_addr constant [21 x i8] c"prim_vector_45length\00", align 1
@.str.84 = private unnamed_addr constant [13 x i8] c"bounds check\00", align 1
@.str.85 = private unnamed_addr constant [53 x i8] c"Bounds check fail, wanted pos %ld, only %ld elements\00", align 1
@.str.86 = private unnamed_addr constant [23 x i8] c"applyprim_vector_45ref\00", align 1
@.str.87 = private unnamed_addr constant [11 x i8] c"vector-ref\00", align 1
@.str.88 = private unnamed_addr constant [21 x i8] c"vector-ref index_pos\00", align 1
@.str.89 = private unnamed_addr constant [26 x i8] c"applyprim_vector_45set_33\00", align 1
@.str.90 = private unnamed_addr constant [46 x i8] c"Function '%s' expected 3 arguments but got 1.\00", align 1
@.str.91 = private unnamed_addr constant [46 x i8] c"Function '%s' expected 3 arguments but got 2.\00", align 1
@.str.92 = private unnamed_addr constant [38 x i8] c"Function '%s' only takes 3 arguments.\00", align 1
@.str.93 = private unnamed_addr constant [12 x i8] c"vector-set!\00", align 1
@.str.94 = private unnamed_addr constant [28 x i8] c"apply vector-set! index_pos\00", align 1
@.str.95 = private unnamed_addr constant [64 x i8] c"Expected an empty list but got something else for function '%s'\00", align 1
@.str.96 = private unnamed_addr constant [15 x i8] c"applyprim_void\00", align 1
@.str.97 = private unnamed_addr constant [19 x i8] c"vec_eq_helper avec\00", align 1
@.str.98 = private unnamed_addr constant [19 x i8] c"vec_eq_helper bvec\00", align 1
@.str.99 = private unnamed_addr constant [19 x i8] c"vec_eq_helper alen\00", align 1
@.str.100 = private unnamed_addr constant [19 x i8] c"vec_eq_helper blen\00", align 1
@.str.101 = private unnamed_addr constant [18 x i8] c"prim_eq_63 Bool a\00", align 1
@.str.102 = private unnamed_addr constant [18 x i8] c"prim_eq_63 Bool b\00", align 1
@.str.103 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Int a\00", align 1
@.str.104 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Int b\00", align 1
@.str.105 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Str a\00", align 1
@.str.106 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Str b\00", align 1
@.str.107 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Sym a\00", align 1
@.str.108 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Sym b\00", align 1
@.str.109 = private unnamed_addr constant [34 x i8] c"Hash eq? not supported currently.\00", align 1
@.str.110 = private unnamed_addr constant [33 x i8] c"Set eq? not supported currently.\00", align 1
@.str.111 = private unnamed_addr constant [42 x i8] c"I do not know what youre trying to eq?!!.\00", align 1
@.str.112 = private unnamed_addr constant [16 x i8] c"applyprim_eq_63\00", align 1
@.str.113 = private unnamed_addr constant [17 x i8] c"applyprim_eqv_63\00", align 1
@.str.114 = private unnamed_addr constant [19 x i8] c"applyprim_equal_63\00", align 1
@.str.115 = private unnamed_addr constant [20 x i8] c"applyprim_number_63\00", align 1
@.str.116 = private unnamed_addr constant [21 x i8] c"applyprim_integer_63\00", align 1
@.str.117 = private unnamed_addr constant [21 x i8] c"applyprim_boolean_63\00", align 1
@.str.118 = private unnamed_addr constant [18 x i8] c"applyprim_void_63\00", align 1
@.str.119 = private unnamed_addr constant [23 x i8] c"applyprim_procedure_63\00", align 1
@.str.120 = private unnamed_addr constant [18 x i8] c"applyprim_null_63\00", align 1
@.str.121 = private unnamed_addr constant [18 x i8] c"applyprim_cons_63\00", align 1
@.str.122 = private unnamed_addr constant [15 x i8] c"applyprim_cons\00", align 1
@.str.123 = private unnamed_addr constant [14 x i8] c"applyprim_cdr\00", align 1
@.str.124 = private unnamed_addr constant [4 x i8] c"cdr\00", align 1
@.str.125 = private unnamed_addr constant [4 x i8] c"- a\00", align 1
@.str.126 = private unnamed_addr constant [4 x i8] c"- b\00", align 1
@.str.127 = private unnamed_addr constant [8 x i8] c"apply -\00", align 1
@.str.128 = private unnamed_addr constant [15 x i8] c"apply - carval\00", align 1
@.str.129 = private unnamed_addr constant [20 x i8] c"applyprim__45 final\00", align 1
@.str.130 = private unnamed_addr constant [4 x i8] c"* a\00", align 1
@.str.131 = private unnamed_addr constant [4 x i8] c"* b\00", align 1
@.str.132 = private unnamed_addr constant [17 x i8] c"apply * cons_obj\00", align 1
@.str.133 = private unnamed_addr constant [15 x i8] c"apply * carval\00", align 1
@.str.134 = private unnamed_addr constant [15 x i8] c"apply * cdrval\00", align 1
@.str.135 = private unnamed_addr constant [36 x i8] c"apply * taking a non-list argument!\00", align 1
@.str.136 = private unnamed_addr constant [4 x i8] c"/ a\00", align 1
@.str.137 = private unnamed_addr constant [4 x i8] c"/ b\00", align 1
@.str.138 = private unnamed_addr constant [14 x i8] c"applyprim__61\00", align 1
@.str.139 = private unnamed_addr constant [4 x i8] c"= a\00", align 1
@.str.140 = private unnamed_addr constant [4 x i8] c"= b\00", align 1
@.str.141 = private unnamed_addr constant [4 x i8] c"< a\00", align 1
@.str.142 = private unnamed_addr constant [4 x i8] c"< b\00", align 1
@.str.143 = private unnamed_addr constant [17 x i8] c"applyprim__60_61\00", align 1
@.str.144 = private unnamed_addr constant [5 x i8] c"<= a\00", align 1
@.str.145 = private unnamed_addr constant [5 x i8] c"<= b\00", align 1
@.str.146 = private unnamed_addr constant [14 x i8] c"applyprim_not\00", align 1
@.str.147 = private unnamed_addr constant [4 x i8] c"not\00", align 1
@.str.148 = private unnamed_addr constant [30 x i8] c"applyprim_hash_45has_45key_63\00", align 1
@.str.149 = private unnamed_addr constant [14 x i8] c"hash-has-key?\00", align 1
@.str.150 = private unnamed_addr constant [33 x i8] c"Key not provided value in (hash)\00", align 1
@.str.151 = private unnamed_addr constant [22 x i8] c"applyprim_hash_45keys\00", align 1
@.str.152 = private unnamed_addr constant [10 x i8] c"hash-keys\00", align 1
@.str.153 = private unnamed_addr constant [32 x i8] c"Too many args given in hash-ref\00", align 1
@.str.154 = private unnamed_addr constant [46 x i8] c"Bad Types Somewhere in hash-ref... good luck!\00", align 1
@.str.155 = private unnamed_addr constant [9 x i8] c"hash-ref\00", align 1
@.str.156 = private unnamed_addr constant [31 x i8] c"Map did not have requested key\00", align 1
@.str.157 = private unnamed_addr constant [21 x i8] c"applyprim_hash_45set\00", align 1
@.str.158 = private unnamed_addr constant [9 x i8] c"hash-set\00", align 1
@.str.159 = private unnamed_addr constant [18 x i8] c"applyprim_hash_63\00", align 1
@.str.160 = private unnamed_addr constant [23 x i8] c"applyprim_hash_45count\00", align 1
@.str.161 = private unnamed_addr constant [11 x i8] c"hash-count\00", align 1

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.Map* @insert_update(%struct.Map* noundef %0, %struct.SinObj* noundef %1, %struct.SinObj* noundef %2) #0 {
  %4 = alloca %struct.Map*, align 8
  %5 = alloca %struct.Map*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj*, align 8
  %8 = alloca %struct.Map*, align 8
  %9 = alloca %struct.Map*, align 8
  store %struct.Map* %0, %struct.Map** %5, align 8
  store %struct.SinObj* %1, %struct.SinObj** %6, align 8
  store %struct.SinObj* %2, %struct.SinObj** %7, align 8
  %10 = load %struct.Map*, %struct.Map** %5, align 8
  %11 = icmp eq %struct.Map* %10, null
  br i1 %11, label %12, label %24

12:                                               ; preds = %3
  %13 = call noalias i8* @malloc(i64 noundef 24) #9
  %14 = bitcast i8* %13 to %struct.Map*
  store %struct.Map* %14, %struct.Map** %8, align 8
  %15 = load %struct.Map*, %struct.Map** %8, align 8
  %16 = getelementptr inbounds %struct.Map, %struct.Map* %15, i32 0, i32 0
  store %struct.Map* null, %struct.Map** %16, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %18 = load %struct.Map*, %struct.Map** %8, align 8
  %19 = getelementptr inbounds %struct.Map, %struct.Map* %18, i32 0, i32 1
  store %struct.SinObj* %17, %struct.SinObj** %19, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %21 = load %struct.Map*, %struct.Map** %8, align 8
  %22 = getelementptr inbounds %struct.Map, %struct.Map* %21, i32 0, i32 2
  store %struct.SinObj* %20, %struct.SinObj** %22, align 8
  %23 = load %struct.Map*, %struct.Map** %8, align 8
  store %struct.Map* %23, %struct.Map** %4, align 8
  br label %67

24:                                               ; preds = %3
  %25 = call noalias i8* @malloc(i64 noundef 24) #9
  %26 = bitcast i8* %25 to %struct.Map*
  store %struct.Map* %26, %struct.Map** %9, align 8
  %27 = load %struct.Map*, %struct.Map** %5, align 8
  %28 = getelementptr inbounds %struct.Map, %struct.Map* %27, i32 0, i32 1
  %29 = load %struct.SinObj*, %struct.SinObj** %28, align 8
  %30 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %31 = call i32 @eq_helper(%struct.SinObj* noundef %29, %struct.SinObj* noundef %30)
  %32 = icmp eq i32 %31, 1
  br i1 %32, label %33, label %46

33:                                               ; preds = %24
  %34 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %35 = load %struct.Map*, %struct.Map** %9, align 8
  %36 = getelementptr inbounds %struct.Map, %struct.Map* %35, i32 0, i32 1
  store %struct.SinObj* %34, %struct.SinObj** %36, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %38 = load %struct.Map*, %struct.Map** %9, align 8
  %39 = getelementptr inbounds %struct.Map, %struct.Map* %38, i32 0, i32 2
  store %struct.SinObj* %37, %struct.SinObj** %39, align 8
  %40 = load %struct.Map*, %struct.Map** %5, align 8
  %41 = getelementptr inbounds %struct.Map, %struct.Map* %40, i32 0, i32 0
  %42 = load %struct.Map*, %struct.Map** %41, align 8
  %43 = call %struct.Map* @insert_copy(%struct.Map* noundef %42)
  %44 = load %struct.Map*, %struct.Map** %9, align 8
  %45 = getelementptr inbounds %struct.Map, %struct.Map* %44, i32 0, i32 0
  store %struct.Map* %43, %struct.Map** %45, align 8
  br label %65

46:                                               ; preds = %24
  %47 = load %struct.Map*, %struct.Map** %5, align 8
  %48 = getelementptr inbounds %struct.Map, %struct.Map* %47, i32 0, i32 1
  %49 = load %struct.SinObj*, %struct.SinObj** %48, align 8
  %50 = load %struct.Map*, %struct.Map** %9, align 8
  %51 = getelementptr inbounds %struct.Map, %struct.Map* %50, i32 0, i32 1
  store %struct.SinObj* %49, %struct.SinObj** %51, align 8
  %52 = load %struct.Map*, %struct.Map** %5, align 8
  %53 = getelementptr inbounds %struct.Map, %struct.Map* %52, i32 0, i32 2
  %54 = load %struct.SinObj*, %struct.SinObj** %53, align 8
  %55 = load %struct.Map*, %struct.Map** %9, align 8
  %56 = getelementptr inbounds %struct.Map, %struct.Map* %55, i32 0, i32 2
  store %struct.SinObj* %54, %struct.SinObj** %56, align 8
  %57 = load %struct.Map*, %struct.Map** %5, align 8
  %58 = getelementptr inbounds %struct.Map, %struct.Map* %57, i32 0, i32 0
  %59 = load %struct.Map*, %struct.Map** %58, align 8
  %60 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %61 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %62 = call %struct.Map* @insert_update(%struct.Map* noundef %59, %struct.SinObj* noundef %60, %struct.SinObj* noundef %61)
  %63 = load %struct.Map*, %struct.Map** %9, align 8
  %64 = getelementptr inbounds %struct.Map, %struct.Map* %63, i32 0, i32 0
  store %struct.Map* %62, %struct.Map** %64, align 8
  br label %65

65:                                               ; preds = %46, %33
  %66 = load %struct.Map*, %struct.Map** %9, align 8
  store %struct.Map* %66, %struct.Map** %4, align 8
  br label %67

67:                                               ; preds = %65, %12
  %68 = load %struct.Map*, %struct.Map** %4, align 8
  ret %struct.Map* %68
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64 noundef) #1

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i32 @eq_helper(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca i8*, align 8
  %7 = alloca i8*, align 8
  %8 = alloca i8*, align 8
  %9 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %11 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i32 0, i32 1
  %12 = load i32, i32* %11, align 8
  %13 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %14 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %13, i32 0, i32 1
  %15 = load i32, i32* %14, align 8
  %16 = icmp ne i32 %12, %15
  br i1 %16, label %17, label %18

17:                                               ; preds = %2
  store i32 0, i32* %3, align 4
  br label %85

18:                                               ; preds = %2
  %19 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %20 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %19, i32 0, i32 1
  %21 = load i32, i32* %20, align 8
  switch i32 %21, label %84 [
    i32 0, label %22
    i32 1, label %23
    i32 2, label %24
    i32 12, label %32
    i32 3, label %33
    i32 4, label %34
    i32 5, label %38
    i32 6, label %46
    i32 7, label %57
    i32 8, label %68
    i32 9, label %72
    i32 10, label %76
    i32 11, label %80
  ]

22:                                               ; preds = %18
  store i32 1, i32* %3, align 4
  br label %85

23:                                               ; preds = %18
  store i32 1, i32* %3, align 4
  br label %85

24:                                               ; preds = %18
  %25 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %26 = call i64 @unwrap_bool(%struct.SinObj* noundef %25, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.101, i64 0, i64 0))
  %27 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %28 = call i64 @unwrap_bool(%struct.SinObj* noundef %27, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.102, i64 0, i64 0))
  %29 = icmp eq i64 %26, %28
  br i1 %29, label %30, label %31

30:                                               ; preds = %24
  store i32 1, i32* %3, align 4
  br label %85

31:                                               ; preds = %24
  store i32 0, i32* %3, align 4
  br label %85

32:                                               ; preds = %18
  store i32 0, i32* %3, align 4
  br label %85

33:                                               ; preds = %18
  store i32 0, i32* %3, align 4
  br label %85

34:                                               ; preds = %18
  %35 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %36 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %37 = call i32 @cons_eq_helper(%struct.SinObj* noundef %35, %struct.SinObj* noundef %36)
  store i32 %37, i32* %3, align 4
  br label %85

38:                                               ; preds = %18
  %39 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %40 = call i64 @unwrap_int(%struct.SinObj* noundef %39, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.103, i64 0, i64 0))
  %41 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %42 = call i64 @unwrap_int(%struct.SinObj* noundef %41, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.104, i64 0, i64 0))
  %43 = icmp eq i64 %40, %42
  br i1 %43, label %44, label %45

44:                                               ; preds = %38
  store i32 1, i32* %3, align 4
  br label %85

45:                                               ; preds = %38
  store i32 0, i32* %3, align 4
  br label %85

46:                                               ; preds = %18
  %47 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %48 = call i8* @unwrap_str(%struct.SinObj* noundef %47, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.105, i64 0, i64 0))
  store i8* %48, i8** %6, align 8
  %49 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %50 = call i8* @unwrap_str(%struct.SinObj* noundef %49, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.106, i64 0, i64 0))
  store i8* %50, i8** %7, align 8
  %51 = load i8*, i8** %6, align 8
  %52 = load i8*, i8** %7, align 8
  %53 = call i32 @strcmp(i8* noundef %51, i8* noundef %52) #10
  %54 = icmp eq i32 %53, 0
  br i1 %54, label %55, label %56

55:                                               ; preds = %46
  store i32 1, i32* %3, align 4
  br label %85

56:                                               ; preds = %46
  store i32 0, i32* %3, align 4
  br label %85

57:                                               ; preds = %18
  %58 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %59 = call i8* @unwrap_sym(%struct.SinObj* noundef %58, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.107, i64 0, i64 0))
  store i8* %59, i8** %8, align 8
  %60 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %61 = call i8* @unwrap_sym(%struct.SinObj* noundef %60, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.108, i64 0, i64 0))
  store i8* %61, i8** %9, align 8
  %62 = load i8*, i8** %8, align 8
  %63 = load i8*, i8** %9, align 8
  %64 = call i32 @strcmp(i8* noundef %62, i8* noundef %63) #10
  %65 = icmp eq i32 %64, 0
  br i1 %65, label %66, label %67

66:                                               ; preds = %57
  store i32 1, i32* %3, align 4
  br label %85

67:                                               ; preds = %57
  store i32 0, i32* %3, align 4
  br label %85

68:                                               ; preds = %18
  %69 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %70 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %71 = call i32 @vec_eq_helper(%struct.SinObj* noundef %69, %struct.SinObj* noundef %70)
  store i32 %71, i32* %3, align 4
  br label %85

72:                                               ; preds = %18
  %73 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %74 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([34 x i8], [34 x i8]* @.str.109, i64 0, i64 0))
  %75 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

76:                                               ; preds = %18
  %77 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %78 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([33 x i8], [33 x i8]* @.str.110, i64 0, i64 0))
  %79 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

80:                                               ; preds = %18
  %81 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %82 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([42 x i8], [42 x i8]* @.str.111, i64 0, i64 0))
  %83 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

84:                                               ; preds = %18
  call void @llvm.trap()
  unreachable

85:                                               ; preds = %68, %67, %66, %56, %55, %45, %44, %34, %33, %32, %31, %30, %23, %22, %17
  %86 = load i32, i32* %3, align 4
  ret i32 %86
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.Map* @insert_copy(%struct.Map* noundef %0) #0 {
  %2 = alloca %struct.Map*, align 8
  %3 = alloca %struct.Map*, align 8
  %4 = alloca %struct.Map*, align 8
  store %struct.Map* %0, %struct.Map** %3, align 8
  %5 = load %struct.Map*, %struct.Map** %3, align 8
  %6 = icmp eq %struct.Map* %5, null
  br i1 %6, label %7, label %8

7:                                                ; preds = %1
  store %struct.Map* null, %struct.Map** %2, align 8
  br label %28

8:                                                ; preds = %1
  %9 = call noalias i8* @malloc(i64 noundef 24) #9
  %10 = bitcast i8* %9 to %struct.Map*
  store %struct.Map* %10, %struct.Map** %4, align 8
  %11 = load %struct.Map*, %struct.Map** %3, align 8
  %12 = getelementptr inbounds %struct.Map, %struct.Map* %11, i32 0, i32 1
  %13 = load %struct.SinObj*, %struct.SinObj** %12, align 8
  %14 = load %struct.Map*, %struct.Map** %4, align 8
  %15 = getelementptr inbounds %struct.Map, %struct.Map* %14, i32 0, i32 1
  store %struct.SinObj* %13, %struct.SinObj** %15, align 8
  %16 = load %struct.Map*, %struct.Map** %3, align 8
  %17 = getelementptr inbounds %struct.Map, %struct.Map* %16, i32 0, i32 2
  %18 = load %struct.SinObj*, %struct.SinObj** %17, align 8
  %19 = load %struct.Map*, %struct.Map** %4, align 8
  %20 = getelementptr inbounds %struct.Map, %struct.Map* %19, i32 0, i32 2
  store %struct.SinObj* %18, %struct.SinObj** %20, align 8
  %21 = load %struct.Map*, %struct.Map** %3, align 8
  %22 = getelementptr inbounds %struct.Map, %struct.Map* %21, i32 0, i32 0
  %23 = load %struct.Map*, %struct.Map** %22, align 8
  %24 = call %struct.Map* @insert_copy(%struct.Map* noundef %23)
  %25 = load %struct.Map*, %struct.Map** %4, align 8
  %26 = getelementptr inbounds %struct.Map, %struct.Map* %25, i32 0, i32 0
  store %struct.Map* %24, %struct.Map** %26, align 8
  %27 = load %struct.Map*, %struct.Map** %4, align 8
  store %struct.Map* %27, %struct.Map** %2, align 8
  br label %28

28:                                               ; preds = %8, %7
  %29 = load %struct.Map*, %struct.Map** %2, align 8
  ret %struct.Map* %29
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.Map* @map_insert(%struct.Map* noundef %0, %struct.SinObj* noundef %1, %struct.SinObj* noundef %2) #0 {
  %4 = alloca %struct.Map*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  store %struct.Map* %0, %struct.Map** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  store %struct.SinObj* %2, %struct.SinObj** %6, align 8
  %7 = load %struct.Map*, %struct.Map** %4, align 8
  %8 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %10 = call %struct.Map* @insert_update(%struct.Map* noundef %7, %struct.SinObj* noundef %8, %struct.SinObj* noundef %9)
  ret %struct.Map* %10
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @map_keys(%struct.Map* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.Map*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  store %struct.Map* %0, %struct.Map** %3, align 8
  %7 = load %struct.Map*, %struct.Map** %3, align 8
  %8 = icmp eq %struct.Map* %7, null
  br i1 %8, label %9, label %11

9:                                                ; preds = %1
  %10 = call %struct.SinObj* @const_init_null()
  store %struct.SinObj* %10, %struct.SinObj** %2, align 8
  br label %23

11:                                               ; preds = %1
  %12 = load %struct.Map*, %struct.Map** %3, align 8
  %13 = getelementptr inbounds %struct.Map, %struct.Map* %12, i32 0, i32 1
  %14 = load %struct.SinObj*, %struct.SinObj** %13, align 8
  store %struct.SinObj* %14, %struct.SinObj** %4, align 8
  %15 = load %struct.Map*, %struct.Map** %3, align 8
  %16 = getelementptr inbounds %struct.Map, %struct.Map* %15, i32 0, i32 0
  %17 = load %struct.Map*, %struct.Map** %16, align 8
  %18 = call %struct.SinObj* @map_keys(%struct.Map* noundef %17)
  store %struct.SinObj* %18, %struct.SinObj** %5, align 8
  %19 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %21 = call %struct.SinObj* @prim_cons(%struct.SinObj* noundef %19, %struct.SinObj* noundef %20)
  store %struct.SinObj* %21, %struct.SinObj** %6, align 8
  %22 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  store %struct.SinObj* %22, %struct.SinObj** %2, align 8
  br label %23

23:                                               ; preds = %11, %9
  %24 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  ret %struct.SinObj* %24
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @const_init_null() #2 {
  %1 = alloca %struct.SinObj*, align 8
  %2 = call %struct.SinObj* @alloc_atomic(i64 noundef 1)
  store %struct.SinObj* %2, %struct.SinObj** %1, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 0
  store i8* null, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  store i32 1, i32* %6, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_cons(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #2 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %7 = call %struct.SinObj* @alloc(i64 noundef 2)
  store %struct.SinObj* %7, %struct.SinObj** %5, align 8
  %8 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i64 0
  %11 = bitcast %struct.SinObj* %10 to i8*
  %12 = bitcast %struct.SinObj* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %11, i8* align 8 %12, i64 16, i1 false)
  %13 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %14 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %15 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %14, i64 1
  %16 = bitcast %struct.SinObj* %15 to i8*
  %17 = bitcast %struct.SinObj* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %16, i8* align 8 %17, i64 16, i1 false)
  %18 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %18, %struct.SinObj** %6, align 8
  %19 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %20 = bitcast %struct.SinObj* %19 to i64*
  %21 = bitcast i64* %20 to i8*
  %22 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %23 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %22, i32 0, i32 0
  store i8* %21, i8** %23, align 8
  %24 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i32 0, i32 1
  store i32 4, i32* %25, align 8
  %26 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  ret %struct.SinObj* %26
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @map_get(%struct.Map* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.Map*, align 8
  %5 = alloca %struct.SinObj*, align 8
  store %struct.Map* %0, %struct.Map** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  br label %6

6:                                                ; preds = %20, %2
  %7 = load %struct.Map*, %struct.Map** %4, align 8
  %8 = icmp ne %struct.Map* %7, null
  br i1 %8, label %9, label %24

9:                                                ; preds = %6
  %10 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %11 = load %struct.Map*, %struct.Map** %4, align 8
  %12 = getelementptr inbounds %struct.Map, %struct.Map* %11, i32 0, i32 1
  %13 = load %struct.SinObj*, %struct.SinObj** %12, align 8
  %14 = call i32 @eq_helper(%struct.SinObj* noundef %10, %struct.SinObj* noundef %13)
  %15 = icmp eq i32 %14, 1
  br i1 %15, label %16, label %20

16:                                               ; preds = %9
  %17 = load %struct.Map*, %struct.Map** %4, align 8
  %18 = getelementptr inbounds %struct.Map, %struct.Map* %17, i32 0, i32 2
  %19 = load %struct.SinObj*, %struct.SinObj** %18, align 8
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  br label %25

20:                                               ; preds = %9
  %21 = load %struct.Map*, %struct.Map** %4, align 8
  %22 = getelementptr inbounds %struct.Map, %struct.Map* %21, i32 0, i32 0
  %23 = load %struct.Map*, %struct.Map** %22, align 8
  store %struct.Map* %23, %struct.Map** %4, align 8
  br label %6, !llvm.loop !6

24:                                               ; preds = %6
  store %struct.SinObj* null, %struct.SinObj** %3, align 8
  br label %25

25:                                               ; preds = %24, %16
  %26 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  ret %struct.SinObj* %26
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local zeroext i1 @map_has_key(%struct.Map* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca i1, align 1
  %4 = alloca %struct.Map*, align 8
  %5 = alloca %struct.SinObj*, align 8
  store %struct.Map* %0, %struct.Map** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  br label %6

6:                                                ; preds = %17, %2
  %7 = load %struct.Map*, %struct.Map** %4, align 8
  %8 = icmp ne %struct.Map* %7, null
  br i1 %8, label %9, label %21

9:                                                ; preds = %6
  %10 = load %struct.Map*, %struct.Map** %4, align 8
  %11 = getelementptr inbounds %struct.Map, %struct.Map* %10, i32 0, i32 1
  %12 = load %struct.SinObj*, %struct.SinObj** %11, align 8
  %13 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %14 = call i32 @eq_helper(%struct.SinObj* noundef %12, %struct.SinObj* noundef %13)
  %15 = icmp eq i32 %14, 1
  br i1 %15, label %16, label %17

16:                                               ; preds = %9
  store i1 true, i1* %3, align 1
  br label %22

17:                                               ; preds = %9
  %18 = load %struct.Map*, %struct.Map** %4, align 8
  %19 = getelementptr inbounds %struct.Map, %struct.Map* %18, i32 0, i32 0
  %20 = load %struct.Map*, %struct.Map** %19, align 8
  store %struct.Map* %20, %struct.Map** %4, align 8
  br label %6, !llvm.loop !8

21:                                               ; preds = %6
  store i1 false, i1* %3, align 1
  br label %22

22:                                               ; preds = %21, %16
  %23 = load i1, i1* %3, align 1
  ret i1 %23
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local i64 @map_count(%struct.Map* noundef %0) #2 {
  %2 = alloca %struct.Map*, align 8
  %3 = alloca i64, align 8
  store %struct.Map* %0, %struct.Map** %2, align 8
  store i64 0, i64* %3, align 8
  br label %4

4:                                                ; preds = %7, %1
  %5 = load %struct.Map*, %struct.Map** %2, align 8
  %6 = icmp ne %struct.Map* %5, null
  br i1 %6, label %7, label %13

7:                                                ; preds = %4
  %8 = load i64, i64* %3, align 8
  %9 = add i64 %8, 1
  store i64 %9, i64* %3, align 8
  %10 = load %struct.Map*, %struct.Map** %2, align 8
  %11 = getelementptr inbounds %struct.Map, %struct.Map* %10, i32 0, i32 0
  %12 = load %struct.Map*, %struct.Map** %11, align 8
  store %struct.Map* %12, %struct.Map** %2, align 8
  br label %4, !llvm.loop !9

13:                                               ; preds = %4
  %14 = load i64, i64* %3, align 8
  ret i64 %14
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void @debug_output_registers(%struct.SinRecord** noundef %0, %struct.SinObj*** noundef %1, %struct.SinObj*** noundef %2, %struct.SinObj** noundef %3, i64 noundef %4) #0 {
  %6 = alloca %struct.SinRecord**, align 8
  %7 = alloca %struct.SinObj***, align 8
  %8 = alloca %struct.SinObj***, align 8
  %9 = alloca %struct.SinObj**, align 8
  %10 = alloca i64, align 8
  store %struct.SinRecord** %0, %struct.SinRecord*** %6, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %7, align 8
  store %struct.SinObj*** %2, %struct.SinObj**** %8, align 8
  store %struct.SinObj** %3, %struct.SinObj*** %9, align 8
  store i64 %4, i64* %10, align 8
  %11 = load i64, i64* %10, align 8
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str, i64 0, i64 0), i64 noundef %11)
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.1, i64 0, i64 0))
  %14 = load %struct.SinRecord**, %struct.SinRecord*** %6, align 8
  %15 = load %struct.SinRecord*, %struct.SinRecord** %14, align 8
  %16 = bitcast %struct.SinRecord* %15 to i8*
  %17 = load %struct.SinObj***, %struct.SinObj**** %7, align 8
  %18 = load %struct.SinObj**, %struct.SinObj*** %17, align 8
  %19 = bitcast %struct.SinObj** %18 to i8*
  %20 = load %struct.SinObj***, %struct.SinObj**** %8, align 8
  %21 = load %struct.SinObj**, %struct.SinObj*** %20, align 8
  %22 = bitcast %struct.SinObj** %21 to i8*
  %23 = load %struct.SinObj**, %struct.SinObj*** %9, align 8
  %24 = load %struct.SinObj*, %struct.SinObj** %23, align 8
  %25 = bitcast %struct.SinObj* %24 to i8*
  %26 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.2, i64 0, i64 0), i8* noundef %16, i8* noundef %19, i8* noundef %22, i8* noundef %25)
  %27 = load %struct.SinObj**, %struct.SinObj*** %9, align 8
  %28 = load %struct.SinObj*, %struct.SinObj** %27, align 8
  %29 = icmp ne %struct.SinObj* %28, null
  br i1 %29, label %30, label %36

30:                                               ; preds = %5
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.3, i64 0, i64 0))
  %32 = load %struct.SinObj**, %struct.SinObj*** %9, align 8
  %33 = load %struct.SinObj*, %struct.SinObj** %32, align 8
  %34 = call %struct.SinObj* @prim_display(%struct.SinObj* noundef %33)
  %35 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  br label %36

36:                                               ; preds = %30, %5
  ret void
}

declare i32 @printf(i8* noundef, ...) #3

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_display(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = call %struct.SinObj* @prim_print(%struct.SinObj* noundef %3)
  ret %struct.SinObj* %4
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @alloc(i64 noundef %0) #2 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = mul i64 16, %3
  %5 = call noalias i8* @malloc(i64 noundef %4) #9
  %6 = bitcast i8* %5 to %struct.SinObj*
  ret %struct.SinObj* %6
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @alloc_atomic(i64 noundef %0) #2 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call %struct.SinObj* @alloc(i64 noundef %3)
  ret %struct.SinObj* %4
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void @_get_both(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1, %struct.SinObj* noundef %2) #0 {
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  store %struct.SinObj* %2, %struct.SinObj** %6, align 8
  %8 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %9 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %8, i8* noundef getelementptr inbounds ([10 x i8], [10 x i8]* @.str.5, i64 0, i64 0))
  store %struct.SinObj* %9, %struct.SinObj** %7, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %11 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i64 0
  %12 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %13 = bitcast %struct.SinObj* %12 to i8*
  %14 = bitcast %struct.SinObj* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %13, i8* align 8 %14, i64 16, i1 false)
  %15 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %16 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %15, i64 1
  %17 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %18 = bitcast %struct.SinObj* %17 to i8*
  %19 = bitcast %struct.SinObj* %16 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %18, i8* align 8 %19, i64 16, i1 false)
  ret void
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 4
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([49 x i8], [49 x i8]* @.str.26, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = bitcast i8* %20 to %struct.SinObj*
  ret %struct.SinObj* %21
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #4

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @const_init_int(i64 noundef %0) #2 {
  %2 = alloca i64, align 8
  %3 = alloca %struct.SinObj*, align 8
  store i64 %0, i64* %2, align 8
  %4 = call %struct.SinObj* @alloc_atomic(i64 noundef 1)
  store %struct.SinObj* %4, %struct.SinObj** %3, align 8
  %5 = load i64, i64* %2, align 8
  %6 = inttoptr i64 %5 to i64*
  %7 = bitcast i64* %6 to i8*
  %8 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %9 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 0
  store i8* %7, i8** %9, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %11 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i32 0, i32 1
  store i32 5, i32* %11, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  ret %struct.SinObj* %12
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @const_init_void() #2 {
  %1 = alloca %struct.SinObj*, align 8
  %2 = call %struct.SinObj* @alloc_atomic(i64 noundef 1)
  store %struct.SinObj* %2, %struct.SinObj** %1, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 0
  store i8* null, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  store i32 0, i32* %6, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @const_init_true() #2 {
  %1 = alloca %struct.SinObj*, align 8
  %2 = call %struct.SinObj* @alloc_atomic(i64 noundef 1)
  store %struct.SinObj* %2, %struct.SinObj** %1, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 0
  store i8* inttoptr (i64 1 to i8*), i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  store i32 2, i32* %6, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @const_init_false() #2 {
  %1 = alloca %struct.SinObj*, align 8
  %2 = call %struct.SinObj* @alloc_atomic(i64 noundef 1)
  store %struct.SinObj* %2, %struct.SinObj** %1, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 0
  store i8* null, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  store i32 2, i32* %6, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %1, align 8
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @const_init_string(i8* noundef %0) #2 {
  %2 = alloca i8*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store i8* %0, i8** %2, align 8
  %4 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %4, %struct.SinObj** %3, align 8
  %5 = load i8*, i8** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 0
  store i8* %5, i8** %7, align 8
  %8 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %9 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  store i32 6, i32* %9, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  ret %struct.SinObj* %10
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @const_init_symbol(i8* noundef %0) #2 {
  %2 = alloca i8*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store i8* %0, i8** %2, align 8
  %4 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %4, %struct.SinObj** %3, align 8
  %5 = load i8*, i8** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 0
  store i8* %5, i8** %7, align 8
  %8 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %9 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  store i32 7, i32* %9, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  ret %struct.SinObj* %10
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinRecord* @make_record(%struct.SinRecord* noundef %0, %struct.SinObj*** noundef %1) #2 {
  %3 = alloca %struct.SinRecord*, align 8
  %4 = alloca %struct.SinObj***, align 8
  %5 = alloca %struct.SinRecord*, align 8
  %6 = alloca %struct.SinObj**, align 8
  store %struct.SinRecord* %0, %struct.SinRecord** %3, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %4, align 8
  %7 = call noalias i8* @malloc(i64 noundef 40) #9
  %8 = bitcast i8* %7 to %struct.SinRecord*
  store %struct.SinRecord* %8, %struct.SinRecord** %5, align 8
  %9 = call noalias i8* @calloc(i64 noundef 4096, i64 noundef 8) #9
  %10 = bitcast i8* %9 to %struct.SinObj**
  store %struct.SinObj** %10, %struct.SinObj*** %6, align 8
  %11 = load %struct.SinObj**, %struct.SinObj*** %6, align 8
  %12 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %11, i64 4096
  %13 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %12, i64 -100
  %14 = load %struct.SinObj***, %struct.SinObj**** %4, align 8
  store %struct.SinObj** %13, %struct.SinObj*** %14, align 8
  %15 = load %struct.SinObj**, %struct.SinObj*** %6, align 8
  %16 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  %17 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %16, i32 0, i32 0
  store %struct.SinObj** %15, %struct.SinObj*** %17, align 8
  %18 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  %19 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %18, i32 0, i32 2
  store i64 512, i64* %19, align 8
  %20 = load %struct.SinRecord*, %struct.SinRecord** %3, align 8
  %21 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  %22 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %21, i32 0, i32 1
  store %struct.SinRecord* %20, %struct.SinRecord** %22, align 8
  %23 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  ret %struct.SinRecord* %23
}

; Function Attrs: nounwind
declare noalias i8* @calloc(i64 noundef, i64 noundef) #1

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinRecord* @split_record(%struct.SinRecord* noundef %0, %struct.SinObj** noundef %1, i64 noundef %2, i64 noundef %3, void ()* noundef %4) #2 {
  %6 = alloca %struct.SinRecord*, align 8
  %7 = alloca %struct.SinObj**, align 8
  %8 = alloca i64, align 8
  %9 = alloca i64, align 8
  %10 = alloca void ()*, align 8
  %11 = alloca %struct.SinRecord*, align 8
  store %struct.SinRecord* %0, %struct.SinRecord** %6, align 8
  store %struct.SinObj** %1, %struct.SinObj*** %7, align 8
  store i64 %2, i64* %8, align 8
  store i64 %3, i64* %9, align 8
  store void ()* %4, void ()** %10, align 8
  %12 = call noalias i8* @malloc(i64 noundef 40) #9
  %13 = bitcast i8* %12 to %struct.SinRecord*
  store %struct.SinRecord* %13, %struct.SinRecord** %11, align 8
  %14 = load %struct.SinObj**, %struct.SinObj*** %7, align 8
  %15 = load %struct.SinRecord*, %struct.SinRecord** %11, align 8
  %16 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %15, i32 0, i32 0
  store %struct.SinObj** %14, %struct.SinObj*** %16, align 8
  %17 = load %struct.SinRecord*, %struct.SinRecord** %6, align 8
  %18 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %17, i32 0, i32 2
  %19 = load i64, i64* %18, align 8
  %20 = load i64, i64* %8, align 8
  %21 = sub nsw i64 %19, %20
  %22 = load %struct.SinRecord*, %struct.SinRecord** %11, align 8
  %23 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %22, i32 0, i32 2
  store i64 %21, i64* %23, align 8
  %24 = load %struct.SinRecord*, %struct.SinRecord** %6, align 8
  %25 = load %struct.SinRecord*, %struct.SinRecord** %11, align 8
  %26 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %25, i32 0, i32 1
  store %struct.SinRecord* %24, %struct.SinRecord** %26, align 8
  %27 = load i64, i64* %8, align 8
  %28 = load %struct.SinRecord*, %struct.SinRecord** %6, align 8
  %29 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %28, i32 0, i32 2
  store i64 %27, i64* %29, align 8
  %30 = load i64, i64* %9, align 8
  %31 = load %struct.SinRecord*, %struct.SinRecord** %6, align 8
  %32 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %31, i32 0, i32 3
  store i64 %30, i64* %32, align 8
  %33 = load void ()*, void ()** %10, align 8
  %34 = load %struct.SinRecord*, %struct.SinRecord** %6, align 8
  %35 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %34, i32 0, i32 4
  store void ()* %33, void ()** %35, align 8
  %36 = load %struct.SinRecord*, %struct.SinRecord** %11, align 8
  ret %struct.SinRecord* %36
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @make_continuation_closure(%struct.SinRecord* noundef %0, void ()* noundef %1) #2 {
  %3 = alloca %struct.SinRecord*, align 8
  %4 = alloca void ()*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj*, align 8
  store %struct.SinRecord* %0, %struct.SinRecord** %3, align 8
  store void ()* %1, void ()** %4, align 8
  %9 = call %struct.SinObj* @alloc(i64 noundef 2)
  store %struct.SinObj* %9, %struct.SinObj** %5, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  store i32 11, i32* %10, align 8
  %11 = load void ()*, void ()** %4, align 8
  %12 = bitcast void ()* %11 to i8*
  %13 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 0
  store i8* %12, i8** %13, align 8
  %14 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 1
  store i32 12, i32* %14, align 8
  %15 = load %struct.SinRecord*, %struct.SinRecord** %3, align 8
  %16 = bitcast %struct.SinRecord* %15 to i8*
  %17 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 0
  store i8* %16, i8** %17, align 8
  %18 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i64 0
  %20 = bitcast %struct.SinObj* %19 to i8*
  %21 = bitcast %struct.SinObj* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %20, i8* align 8 %21, i64 16, i1 false)
  %22 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %23 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %22, i64 1
  %24 = bitcast %struct.SinObj* %23 to i8*
  %25 = bitcast %struct.SinObj* %7 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %24, i8* align 8 %25, i64 16, i1 false)
  %26 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %26, %struct.SinObj** %8, align 8
  %27 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %27, i32 0, i32 1
  store i32 3, i32* %28, align 8
  %29 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %30 = bitcast %struct.SinObj* %29 to i8*
  %31 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %32 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %31, i32 0, i32 0
  store i8* %30, i8** %32, align 8
  %33 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @closure_alloc(i64 noundef %0, void ()* noundef %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca void ()*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj*, align 8
  store i64 %0, i64* %3, align 8
  store void ()* %1, void ()** %4, align 8
  %9 = call %struct.SinObj* @alloc(i64 noundef 2)
  store %struct.SinObj* %9, %struct.SinObj** %5, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  store i32 11, i32* %10, align 8
  %11 = load void ()*, void ()** %4, align 8
  %12 = bitcast void ()* %11 to i8*
  %13 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 0
  store i8* %12, i8** %13, align 8
  %14 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 1
  store i32 11, i32* %14, align 8
  %15 = load i64, i64* %3, align 8
  %16 = call %struct.SinObj* @const_init_int(i64 noundef %15)
  %17 = call %struct.SinObj* @const_init_int(i64 noundef 0)
  %18 = call %struct.SinObj* @prim_make_45vector(%struct.SinObj* noundef %16, %struct.SinObj* noundef %17)
  %19 = bitcast %struct.SinObj* %18 to i8*
  %20 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 0
  store i8* %19, i8** %20, align 8
  %21 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 0
  %23 = bitcast %struct.SinObj* %22 to i8*
  %24 = bitcast %struct.SinObj* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %26 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %25, i64 1
  %27 = bitcast %struct.SinObj* %26 to i8*
  %28 = bitcast %struct.SinObj* %7 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %27, i8* align 8 %28, i64 16, i1 false)
  %29 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %29, %struct.SinObj** %8, align 8
  %30 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %31 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %30, i32 0, i32 1
  store i32 3, i32* %31, align 8
  %32 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %33 = bitcast %struct.SinObj* %32 to i8*
  %34 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %35 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %34, i32 0, i32 0
  store i8* %33, i8** %35, align 8
  %36 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  ret %struct.SinObj* %36
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_make_45vector(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %struct.SinObj*, align 8
  %9 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %11 = call i64 @unwrap_int(%struct.SinObj* noundef %10, i8* noundef getelementptr inbounds ([12 x i8], [12 x i8]* @.str.23, i64 0, i64 0))
  store i64 %11, i64* %5, align 8
  %12 = load i64, i64* %5, align 8
  %13 = add i64 1, %12
  %14 = call %struct.SinObj* @alloc(i64 noundef %13)
  store %struct.SinObj* %14, %struct.SinObj** %6, align 8
  %15 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %16 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %15, i64 0
  %17 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %16, i32 0, i32 1
  store i32 5, i32* %17, align 8
  %18 = load i64, i64* %5, align 8
  %19 = inttoptr i64 %18 to i64*
  %20 = bitcast i64* %19 to i8*
  %21 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 0
  %23 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %22, i32 0, i32 0
  store i8* %20, i8** %23, align 8
  store i64 1, i64* %7, align 8
  br label %24

24:                                               ; preds = %51, %2
  %25 = load i64, i64* %7, align 8
  %26 = load i64, i64* %5, align 8
  %27 = icmp ule i64 %25, %26
  br i1 %27, label %28, label %54

28:                                               ; preds = %24
  %29 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %29, %struct.SinObj** %8, align 8
  %30 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %31 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %30, i32 0, i32 1
  %32 = load i32, i32* %31, align 8
  %33 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %34 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %33, i32 0, i32 1
  store i32 %32, i32* %34, align 8
  %35 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %36 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %35, i32 0, i32 0
  %37 = load i8*, i8** %36, align 8
  %38 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %39 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %38, i32 0, i32 0
  store i8* %37, i8** %39, align 8
  %40 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %41 = load i64, i64* %7, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %40, i64 %41
  %43 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %42, i32 0, i32 1
  store i32 11, i32* %43, align 8
  %44 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %45 = bitcast %struct.SinObj* %44 to i64*
  %46 = bitcast i64* %45 to i8*
  %47 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %48 = load i64, i64* %7, align 8
  %49 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %47, i64 %48
  %50 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %49, i32 0, i32 0
  store i8* %46, i8** %50, align 8
  br label %51

51:                                               ; preds = %28
  %52 = load i64, i64* %7, align 8
  %53 = add i64 %52, 1
  store i64 %53, i64* %7, align 8
  br label %24, !llvm.loop !10

54:                                               ; preds = %24
  %55 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %55, %struct.SinObj** %9, align 8
  %56 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %57 = bitcast %struct.SinObj* %56 to i64*
  %58 = bitcast i64* %57 to i8*
  %59 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %60 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %59, i32 0, i32 0
  store i8* %58, i8** %60, align 8
  %61 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %62 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %61, i32 0, i32 1
  store i32 8, i32* %62, align 8
  %63 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  ret %struct.SinObj* %63
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void @closure_place_freevar(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1, i64 noundef %2) #0 {
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %struct.SinObj*, align 8
  %8 = alloca %struct.SinObj*, align 8
  %9 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  store i64 %2, i64* %6, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %11 = call %struct.SinObj* @unwrap_clo(%struct.SinObj* noundef %10, i8* noundef getelementptr inbounds ([22 x i8], [22 x i8]* @.str.6, i64 0, i64 0))
  store %struct.SinObj* %11, %struct.SinObj** %7, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %13 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %12, i64 1
  %14 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %13, i32 0, i32 0
  %15 = load i8*, i8** %14, align 8
  %16 = bitcast i8* %15 to %struct.SinObj*
  store %struct.SinObj* %16, %struct.SinObj** %8, align 8
  %17 = load i64, i64* %6, align 8
  %18 = call %struct.SinObj* @const_init_int(i64 noundef %17)
  store %struct.SinObj* %18, %struct.SinObj** %9, align 8
  %19 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %21 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %22 = call %struct.SinObj* @prim_vector_45set_33(%struct.SinObj* noundef %19, %struct.SinObj* noundef %20, %struct.SinObj* noundef %21)
  ret void
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @unwrap_clo(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 3
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([51 x i8], [51 x i8]* @.str.28, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = bitcast i8* %20 to %struct.SinObj*
  ret %struct.SinObj* %21
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_vector_45set_33(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1, %struct.SinObj* noundef %2) #0 {
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  store %struct.SinObj* %2, %struct.SinObj** %6, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %11 = call %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %10, i8* noundef getelementptr inbounds ([12 x i8], [12 x i8]* @.str.93, i64 0, i64 0))
  store %struct.SinObj* %11, %struct.SinObj** %7, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %13 = call i64 @unwrap_int(%struct.SinObj* noundef %12, i8* noundef getelementptr inbounds ([28 x i8], [28 x i8]* @.str.94, i64 0, i64 0))
  store i64 %13, i64* %8, align 8
  %14 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %15 = load i64, i64* %8, align 8
  call void @bounds_check(%struct.SinObj* noundef %14, i64 noundef %15)
  %16 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %16, %struct.SinObj** %9, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i32 0, i32 1
  %19 = load i32, i32* %18, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i32 0, i32 1
  store i32 %19, i32* %21, align 8
  %22 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %23 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %22, i32 0, i32 0
  %24 = load i8*, i8** %23, align 8
  %25 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %26 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %25, i32 0, i32 0
  store i8* %24, i8** %26, align 8
  %27 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %28 = bitcast %struct.SinObj* %27 to i64*
  %29 = bitcast i64* %28 to i8*
  %30 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %31 = load i64, i64* %8, align 8
  %32 = add nsw i64 %31, 1
  %33 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %30, i64 %32
  %34 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %33, i32 0, i32 0
  store i8* %29, i8** %34, align 8
  %35 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %35
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i8* @closure_get_env_part(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.SinObj* @unwrap_clo(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.7, i64 0, i64 0))
  store %struct.SinObj* %5, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i64 1
  %8 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 0
  %9 = load i8*, i8** %8, align 8
  ret i8* %9
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void ()* @closure_get_fn_part(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.SinObj* @unwrap_clo(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.8, i64 0, i64 0))
  store %struct.SinObj* %5, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i64 0
  %8 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 0
  %9 = load i8*, i8** %8, align 8
  %10 = bitcast i8* %9 to void ()*
  ret void ()* %10
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local zeroext i1 @check_for_overflow(%struct.SinObj*** noundef %0, %struct.SinObj*** noundef %1, i64 noundef %2) #2 {
  %4 = alloca %struct.SinObj***, align 8
  %5 = alloca %struct.SinObj***, align 8
  %6 = alloca i64, align 8
  store %struct.SinObj*** %0, %struct.SinObj**** %4, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %5, align 8
  store i64 %2, i64* %6, align 8
  %7 = load %struct.SinObj***, %struct.SinObj**** %4, align 8
  %8 = load %struct.SinObj**, %struct.SinObj*** %7, align 8
  %9 = load i64, i64* %6, align 8
  %10 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %8, i64 %9
  %11 = ptrtoint %struct.SinObj** %10 to i64
  %12 = load %struct.SinObj***, %struct.SinObj**** %5, align 8
  %13 = load %struct.SinObj**, %struct.SinObj*** %12, align 8
  %14 = ptrtoint %struct.SinObj** %13 to i64
  %15 = icmp slt i64 %11, %14
  ret i1 %15
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local zeroext i1 @callcc_at_base(%struct.SinRecord** noundef %0, %struct.SinObj*** noundef %1) #2 {
  %3 = alloca %struct.SinRecord**, align 8
  %4 = alloca %struct.SinObj***, align 8
  store %struct.SinRecord** %0, %struct.SinRecord*** %3, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %4, align 8
  %5 = load %struct.SinRecord**, %struct.SinRecord*** %3, align 8
  %6 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  %7 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %6, i32 0, i32 0
  %8 = load %struct.SinObj**, %struct.SinObj*** %7, align 8
  %9 = load %struct.SinObj***, %struct.SinObj**** %4, align 8
  %10 = load %struct.SinObj**, %struct.SinObj*** %9, align 8
  %11 = icmp eq %struct.SinObj** %8, %10
  ret i1 %11
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local void @handle_overflow(%struct.SinRecord** noundef %0, %struct.SinObj*** noundef %1, %struct.SinObj*** noundef %2, void ()* noundef %3, i64 noundef %4) #2 {
  %6 = alloca %struct.SinRecord**, align 8
  %7 = alloca %struct.SinObj***, align 8
  %8 = alloca %struct.SinObj***, align 8
  %9 = alloca void ()*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i8, align 1
  %14 = alloca %struct.SinObj**, align 8
  %15 = alloca i32, align 4
  %16 = alloca %struct.SinRecord*, align 8
  %17 = alloca %struct.SinRecord*, align 8
  %18 = alloca %struct.SinRecord*, align 8
  %19 = alloca %struct.SinRecord*, align 8
  store %struct.SinRecord** %0, %struct.SinRecord*** %6, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %7, align 8
  store %struct.SinObj*** %2, %struct.SinObj**** %8, align 8
  store void ()* %3, void ()** %9, align 8
  store i64 %4, i64* %10, align 8
  store i64 0, i64* %11, align 8
  %20 = load i64, i64* %10, align 8
  store i64 %20, i64* %12, align 8
  store i8 0, i8* %13, align 1
  %21 = load %struct.SinObj***, %struct.SinObj**** %7, align 8
  %22 = load %struct.SinObj**, %struct.SinObj*** %21, align 8
  store %struct.SinObj** %22, %struct.SinObj*** %14, align 8
  store i32 0, i32* %15, align 4
  br label %23

23:                                               ; preds = %42, %5
  %24 = load i32, i32* %15, align 4
  %25 = icmp slt i32 %24, 4
  br i1 %25, label %26, label %45

26:                                               ; preds = %23
  %27 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %28 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %27, i64 1
  %29 = load %struct.SinObj*, %struct.SinObj** %28, align 8
  %30 = ptrtoint %struct.SinObj* %29 to i64
  store i64 %30, i64* %11, align 8
  %31 = load i64, i64* %11, align 8
  %32 = icmp eq i64 %31, -1
  br i1 %32, label %33, label %34

33:                                               ; preds = %26
  store i8 1, i8* %13, align 1
  br label %45

34:                                               ; preds = %26
  %35 = load i64, i64* %11, align 8
  %36 = load i64, i64* %12, align 8
  %37 = add nsw i64 %36, %35
  store i64 %37, i64* %12, align 8
  %38 = load i64, i64* %11, align 8
  %39 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %40 = sub i64 0, %38
  %41 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %39, i64 %40
  store %struct.SinObj** %41, %struct.SinObj*** %14, align 8
  br label %42

42:                                               ; preds = %34
  %43 = load i32, i32* %15, align 4
  %44 = add nsw i32 %43, 1
  store i32 %44, i32* %15, align 4
  br label %23, !llvm.loop !11

45:                                               ; preds = %33, %23
  %46 = load i8, i8* %13, align 1
  %47 = trunc i8 %46 to i1
  br i1 %47, label %48, label %84

48:                                               ; preds = %45
  %49 = load %struct.SinRecord**, %struct.SinRecord*** %6, align 8
  %50 = load %struct.SinRecord*, %struct.SinRecord** %49, align 8
  store %struct.SinRecord* %50, %struct.SinRecord** %16, align 8
  %51 = load %struct.SinRecord*, %struct.SinRecord** %16, align 8
  %52 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %51, i32 0, i32 1
  %53 = load %struct.SinRecord*, %struct.SinRecord** %52, align 8
  %54 = load %struct.SinObj***, %struct.SinObj**** %8, align 8
  %55 = call %struct.SinRecord* @make_record(%struct.SinRecord* noundef %53, %struct.SinObj*** noundef %54)
  store %struct.SinRecord* %55, %struct.SinRecord** %17, align 8
  %56 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %57 = load %struct.SinRecord*, %struct.SinRecord** %16, align 8
  %58 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %57, i32 0, i32 0
  %59 = load %struct.SinObj**, %struct.SinObj*** %58, align 8
  %60 = icmp eq %struct.SinObj** %56, %59
  br i1 %60, label %61, label %62

61:                                               ; preds = %48
  br label %64

62:                                               ; preds = %48
  call void @__assert_fail(i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.9, i64 0, i64 0), i8* noundef getelementptr inbounds ([22 x i8], [22 x i8]* @.str.10, i64 0, i64 0), i32 noundef 249, i8* noundef getelementptr inbounds ([73 x i8], [73 x i8]* @__PRETTY_FUNCTION__.handle_overflow, i64 0, i64 0)) #11
  unreachable

63:                                               ; No predecessors!
  br label %64

64:                                               ; preds = %63, %61
  %65 = load %struct.SinRecord*, %struct.SinRecord** %17, align 8
  %66 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %65, i32 0, i32 0
  %67 = load %struct.SinObj**, %struct.SinObj*** %66, align 8
  %68 = bitcast %struct.SinObj** %67 to i8*
  %69 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %70 = bitcast %struct.SinObj** %69 to i8*
  %71 = load i64, i64* %12, align 8
  %72 = mul i64 %71, 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %68, i8* align 8 %70, i64 %72, i1 false)
  %73 = load %struct.SinRecord*, %struct.SinRecord** %17, align 8
  %74 = load %struct.SinRecord**, %struct.SinRecord*** %6, align 8
  store %struct.SinRecord* %73, %struct.SinRecord** %74, align 8
  %75 = load %struct.SinRecord*, %struct.SinRecord** %17, align 8
  %76 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %75, i32 0, i32 0
  %77 = load %struct.SinObj**, %struct.SinObj*** %76, align 8
  %78 = load i64, i64* %12, align 8
  %79 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %77, i64 %78
  %80 = load i64, i64* %10, align 8
  %81 = sub i64 0, %80
  %82 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %79, i64 %81
  %83 = load %struct.SinObj***, %struct.SinObj**** %7, align 8
  store %struct.SinObj** %82, %struct.SinObj*** %83, align 8
  br label %141

84:                                               ; preds = %45
  %85 = load %struct.SinRecord**, %struct.SinRecord*** %6, align 8
  %86 = load %struct.SinRecord*, %struct.SinRecord** %85, align 8
  store %struct.SinRecord* %86, %struct.SinRecord** %18, align 8
  %87 = load %struct.SinRecord*, %struct.SinRecord** %18, align 8
  %88 = load %struct.SinObj***, %struct.SinObj**** %8, align 8
  %89 = call %struct.SinRecord* @make_record(%struct.SinRecord* noundef %87, %struct.SinObj*** noundef %88)
  store %struct.SinRecord* %89, %struct.SinRecord** %19, align 8
  %90 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %91 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %90, i64 0
  %92 = load %struct.SinObj*, %struct.SinObj** %91, align 8
  %93 = bitcast %struct.SinObj* %92 to void ()*
  %94 = load %struct.SinRecord*, %struct.SinRecord** %18, align 8
  %95 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %94, i32 0, i32 4
  store void ()* %93, void ()** %95, align 8
  %96 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %97 = ptrtoint %struct.SinObj** %96 to i64
  %98 = load %struct.SinRecord*, %struct.SinRecord** %18, align 8
  %99 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %98, i32 0, i32 0
  %100 = load %struct.SinObj**, %struct.SinObj*** %99, align 8
  %101 = ptrtoint %struct.SinObj** %100 to i64
  %102 = sub nsw i64 %97, %101
  %103 = sdiv i64 %102, 8
  %104 = load %struct.SinRecord*, %struct.SinRecord** %18, align 8
  %105 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %104, i32 0, i32 2
  store i64 %103, i64* %105, align 8
  %106 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %107 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %106, i64 1
  %108 = load %struct.SinObj*, %struct.SinObj** %107, align 8
  %109 = ptrtoint %struct.SinObj* %108 to i64
  %110 = load %struct.SinRecord*, %struct.SinRecord** %18, align 8
  %111 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %110, i32 0, i32 3
  store i64 %109, i64* %111, align 8
  %112 = load %struct.SinRecord*, %struct.SinRecord** %19, align 8
  %113 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %112, i32 0, i32 0
  %114 = load %struct.SinObj**, %struct.SinObj*** %113, align 8
  %115 = bitcast %struct.SinObj** %114 to i8*
  %116 = load %struct.SinObj**, %struct.SinObj*** %14, align 8
  %117 = bitcast %struct.SinObj** %116 to i8*
  %118 = load i64, i64* %12, align 8
  %119 = mul i64 %118, 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %115, i8* align 8 %117, i64 %119, i1 false)
  %120 = load void ()*, void ()** %9, align 8
  %121 = bitcast void ()* %120 to %struct.SinObj*
  %122 = load %struct.SinRecord*, %struct.SinRecord** %19, align 8
  %123 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %122, i32 0, i32 0
  %124 = load %struct.SinObj**, %struct.SinObj*** %123, align 8
  %125 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %124, i64 0
  store %struct.SinObj* %121, %struct.SinObj** %125, align 8
  %126 = load %struct.SinRecord*, %struct.SinRecord** %19, align 8
  %127 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %126, i32 0, i32 0
  %128 = load %struct.SinObj**, %struct.SinObj*** %127, align 8
  %129 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %128, i64 1
  store %struct.SinObj* inttoptr (i64 -1 to %struct.SinObj*), %struct.SinObj** %129, align 8
  %130 = load %struct.SinRecord*, %struct.SinRecord** %19, align 8
  %131 = load %struct.SinRecord**, %struct.SinRecord*** %6, align 8
  store %struct.SinRecord* %130, %struct.SinRecord** %131, align 8
  %132 = load %struct.SinRecord*, %struct.SinRecord** %19, align 8
  %133 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %132, i32 0, i32 0
  %134 = load %struct.SinObj**, %struct.SinObj*** %133, align 8
  %135 = load i64, i64* %12, align 8
  %136 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %134, i64 %135
  %137 = load i64, i64* %10, align 8
  %138 = sub i64 0, %137
  %139 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %136, i64 %138
  %140 = load %struct.SinObj***, %struct.SinObj**** %7, align 8
  store %struct.SinObj** %139, %struct.SinObj*** %140, align 8
  br label %141

141:                                              ; preds = %84, %64
  ret void
}

; Function Attrs: noreturn nounwind
declare void @__assert_fail(i8* noundef, i8* noundef, i32 noundef, i8* noundef) #5

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local void ()* @handle_underflow(%struct.SinRecord** noundef %0, %struct.SinObj*** noundef %1, %struct.SinObj*** noundef %2) #2 {
  %4 = alloca %struct.SinRecord**, align 8
  %5 = alloca %struct.SinObj***, align 8
  %6 = alloca %struct.SinObj***, align 8
  %7 = alloca %struct.SinRecord*, align 8
  %8 = alloca %struct.SinRecord*, align 8
  %9 = alloca i64, align 8
  %10 = alloca i64, align 8
  %11 = alloca i64, align 8
  %12 = alloca void ()*, align 8
  store %struct.SinRecord** %0, %struct.SinRecord*** %4, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %5, align 8
  store %struct.SinObj*** %2, %struct.SinObj**** %6, align 8
  %13 = load %struct.SinRecord**, %struct.SinRecord*** %4, align 8
  %14 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  store %struct.SinRecord* %14, %struct.SinRecord** %7, align 8
  %15 = load %struct.SinRecord*, %struct.SinRecord** %7, align 8
  %16 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %15, i32 0, i32 1
  %17 = load %struct.SinRecord*, %struct.SinRecord** %16, align 8
  store %struct.SinRecord* %17, %struct.SinRecord** %8, align 8
  %18 = load %struct.SinRecord*, %struct.SinRecord** %8, align 8
  %19 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %18, i32 0, i32 1
  %20 = load %struct.SinRecord*, %struct.SinRecord** %19, align 8
  %21 = load %struct.SinRecord*, %struct.SinRecord** %7, align 8
  %22 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %21, i32 0, i32 1
  store %struct.SinRecord* %20, %struct.SinRecord** %22, align 8
  %23 = load %struct.SinRecord*, %struct.SinRecord** %7, align 8
  %24 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %23, i32 0, i32 2
  %25 = load i64, i64* %24, align 8
  store i64 %25, i64* %9, align 8
  %26 = load %struct.SinRecord*, %struct.SinRecord** %8, align 8
  %27 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %26, i32 0, i32 2
  %28 = load i64, i64* %27, align 8
  store i64 %28, i64* %10, align 8
  %29 = load %struct.SinRecord*, %struct.SinRecord** %8, align 8
  %30 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %29, i32 0, i32 3
  %31 = load i64, i64* %30, align 8
  store i64 %31, i64* %11, align 8
  %32 = load %struct.SinRecord*, %struct.SinRecord** %8, align 8
  %33 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %32, i32 0, i32 4
  %34 = load void ()*, void ()** %33, align 8
  store void ()* %34, void ()** %12, align 8
  %35 = load i64, i64* %10, align 8
  %36 = load i64, i64* %9, align 8
  %37 = icmp sge i64 %35, %36
  br i1 %37, label %38, label %49

38:                                               ; preds = %3
  %39 = call noalias i8* @calloc(i64 noundef 4096, i64 noundef 8) #9
  %40 = bitcast i8* %39 to %struct.SinObj**
  %41 = load %struct.SinRecord*, %struct.SinRecord** %7, align 8
  %42 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %41, i32 0, i32 0
  store %struct.SinObj** %40, %struct.SinObj*** %42, align 8
  %43 = load %struct.SinRecord*, %struct.SinRecord** %7, align 8
  %44 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %43, i32 0, i32 0
  %45 = load %struct.SinObj**, %struct.SinObj*** %44, align 8
  %46 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %45, i64 4096
  %47 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %46, i64 -100
  %48 = load %struct.SinObj***, %struct.SinObj**** %6, align 8
  store %struct.SinObj** %47, %struct.SinObj*** %48, align 8
  br label %49

49:                                               ; preds = %38, %3
  %50 = load %struct.SinRecord*, %struct.SinRecord** %7, align 8
  %51 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %50, i32 0, i32 0
  %52 = load %struct.SinObj**, %struct.SinObj*** %51, align 8
  %53 = bitcast %struct.SinObj** %52 to i8*
  %54 = load %struct.SinRecord*, %struct.SinRecord** %8, align 8
  %55 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %54, i32 0, i32 0
  %56 = load %struct.SinObj**, %struct.SinObj*** %55, align 8
  %57 = bitcast %struct.SinObj** %56 to i8*
  %58 = load i64, i64* %10, align 8
  %59 = mul i64 %58, 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %53, i8* align 8 %57, i64 %59, i1 false)
  %60 = load %struct.SinRecord*, %struct.SinRecord** %7, align 8
  %61 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %60, i32 0, i32 0
  %62 = load %struct.SinObj**, %struct.SinObj*** %61, align 8
  %63 = load i64, i64* %10, align 8
  %64 = load i64, i64* %11, align 8
  %65 = sub nsw i64 %63, %64
  %66 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %62, i64 %65
  %67 = load %struct.SinObj***, %struct.SinObj**** %5, align 8
  store %struct.SinObj** %66, %struct.SinObj*** %67, align 8
  %68 = load void ()*, void ()** %12, align 8
  ret void ()* %68
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void ()* @handle_continuation_function(%struct.SinRecord** noundef %0, %struct.SinObj*** noundef %1, %struct.SinObj*** noundef %2, %struct.SinObj** noundef %3) #0 {
  %5 = alloca %struct.SinRecord**, align 8
  %6 = alloca %struct.SinObj***, align 8
  %7 = alloca %struct.SinObj***, align 8
  %8 = alloca %struct.SinObj**, align 8
  %9 = alloca %struct.SinObj**, align 8
  %10 = alloca %struct.SinObj*, align 8
  %11 = alloca %struct.SinObj*, align 8
  %12 = alloca %struct.SinObj*, align 8
  %13 = alloca %struct.SinRecord*, align 8
  %14 = alloca %struct.SinRecord*, align 8
  %15 = alloca %struct.SinRecord*, align 8
  store %struct.SinRecord** %0, %struct.SinRecord*** %5, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %6, align 8
  store %struct.SinObj*** %2, %struct.SinObj**** %7, align 8
  store %struct.SinObj** %3, %struct.SinObj*** %8, align 8
  %16 = load %struct.SinObj***, %struct.SinObj**** %6, align 8
  %17 = load %struct.SinObj**, %struct.SinObj*** %16, align 8
  store %struct.SinObj** %17, %struct.SinObj*** %9, align 8
  %18 = load %struct.SinObj**, %struct.SinObj*** %9, align 8
  %19 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %18, i64 3
  %20 = load %struct.SinObj*, %struct.SinObj** %19, align 8
  store %struct.SinObj* %20, %struct.SinObj** %10, align 8
  %21 = load %struct.SinObj*, %struct.SinObj** %10, align 8
  %22 = call %struct.SinObj* @prim_car(%struct.SinObj* noundef %21)
  store %struct.SinObj* %22, %struct.SinObj** %11, align 8
  %23 = load %struct.SinObj*, %struct.SinObj** %11, align 8
  %24 = load %struct.SinObj**, %struct.SinObj*** %8, align 8
  store %struct.SinObj* %23, %struct.SinObj** %24, align 8
  %25 = load %struct.SinObj**, %struct.SinObj*** %9, align 8
  %26 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %25, i64 2
  %27 = load %struct.SinObj*, %struct.SinObj** %26, align 8
  store %struct.SinObj* %27, %struct.SinObj** %12, align 8
  %28 = load %struct.SinObj*, %struct.SinObj** %12, align 8
  %29 = call i8* @closure_get_env_part(%struct.SinObj* noundef %28)
  %30 = bitcast i8* %29 to %struct.SinRecord*
  store %struct.SinRecord* %30, %struct.SinRecord** %13, align 8
  %31 = load %struct.SinRecord**, %struct.SinRecord*** %5, align 8
  %32 = load %struct.SinRecord*, %struct.SinRecord** %31, align 8
  store %struct.SinRecord* %32, %struct.SinRecord** %14, align 8
  %33 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  %34 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %33, i32 0, i32 2
  %35 = load i64, i64* %34, align 8
  %36 = add nsw i64 %35, 3
  %37 = load %struct.SinRecord*, %struct.SinRecord** %14, align 8
  %38 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %37, i32 0, i32 2
  %39 = load i64, i64* %38, align 8
  %40 = icmp sge i64 %36, %39
  br i1 %40, label %41, label %50

41:                                               ; preds = %4
  %42 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  %43 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %42, i32 0, i32 1
  %44 = load %struct.SinRecord*, %struct.SinRecord** %43, align 8
  %45 = load %struct.SinObj***, %struct.SinObj**** %7, align 8
  %46 = call %struct.SinRecord* @make_record(%struct.SinRecord* noundef %44, %struct.SinObj*** noundef %45)
  store %struct.SinRecord* %46, %struct.SinRecord** %15, align 8
  %47 = load %struct.SinRecord*, %struct.SinRecord** %15, align 8
  store %struct.SinRecord* %47, %struct.SinRecord** %14, align 8
  %48 = load %struct.SinRecord*, %struct.SinRecord** %15, align 8
  %49 = load %struct.SinRecord**, %struct.SinRecord*** %5, align 8
  store %struct.SinRecord* %48, %struct.SinRecord** %49, align 8
  br label %50

50:                                               ; preds = %41, %4
  %51 = load %struct.SinRecord*, %struct.SinRecord** %14, align 8
  %52 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %51, i32 0, i32 0
  %53 = load %struct.SinObj**, %struct.SinObj*** %52, align 8
  %54 = bitcast %struct.SinObj** %53 to i8*
  %55 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  %56 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %55, i32 0, i32 0
  %57 = load %struct.SinObj**, %struct.SinObj*** %56, align 8
  %58 = bitcast %struct.SinObj** %57 to i8*
  %59 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  %60 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %59, i32 0, i32 2
  %61 = load i64, i64* %60, align 8
  %62 = mul i64 %61, 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %54, i8* align 8 %58, i64 %62, i1 false)
  %63 = load %struct.SinRecord*, %struct.SinRecord** %14, align 8
  %64 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %63, i32 0, i32 0
  %65 = load %struct.SinObj**, %struct.SinObj*** %64, align 8
  %66 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  %67 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %66, i32 0, i32 2
  %68 = load i64, i64* %67, align 8
  %69 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  %70 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %69, i32 0, i32 3
  %71 = load i64, i64* %70, align 8
  %72 = sub nsw i64 %68, %71
  %73 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %65, i64 %72
  %74 = load %struct.SinObj***, %struct.SinObj**** %6, align 8
  store %struct.SinObj** %73, %struct.SinObj*** %74, align 8
  %75 = load %struct.SinRecord*, %struct.SinRecord** %13, align 8
  %76 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %75, i32 0, i32 4
  %77 = load void ()*, void ()** %76, align 8
  ret void ()* %77
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_car(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.15, i64 0, i64 0))
  store %struct.SinObj* %5, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i64 0
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local void @start_runtime(%struct.SinRecord** noundef %0, %struct.SinObj*** noundef %1, %struct.SinObj*** noundef %2) #2 {
  %4 = alloca %struct.SinRecord**, align 8
  %5 = alloca %struct.SinObj***, align 8
  %6 = alloca %struct.SinObj***, align 8
  store %struct.SinRecord** %0, %struct.SinRecord*** %4, align 8
  store %struct.SinObj*** %1, %struct.SinObj**** %5, align 8
  store %struct.SinObj*** %2, %struct.SinObj**** %6, align 8
  %7 = load %struct.SinObj***, %struct.SinObj**** %6, align 8
  %8 = call %struct.SinRecord* @make_record(%struct.SinRecord* noundef null, %struct.SinObj*** noundef %7)
  %9 = load %struct.SinRecord**, %struct.SinRecord*** %4, align 8
  store %struct.SinRecord* %8, %struct.SinRecord** %9, align 8
  %10 = load %struct.SinRecord**, %struct.SinRecord*** %4, align 8
  %11 = load %struct.SinRecord*, %struct.SinRecord** %10, align 8
  %12 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %11, i32 0, i32 0
  %13 = load %struct.SinObj**, %struct.SinObj*** %12, align 8
  %14 = load %struct.SinObj***, %struct.SinObj**** %5, align 8
  store %struct.SinObj** %13, %struct.SinObj*** %14, align 8
  ret void
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_car(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.13, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.13, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.13, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_car(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: noreturn nounwind
declare void @exit(i32 noundef) #5

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim__43(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %8 = call i64 @unwrap_int(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.16, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.SinObj* noundef %9, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.17, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %5, align 8
  %12 = load i64, i64* %6, align 8
  %13 = add nsw i64 %11, %12
  %14 = call %struct.SinObj* @const_init_int(i64 noundef %13)
  ret %struct.SinObj* %14
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i64 @unwrap_int(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 5
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([48 x i8], [48 x i8]* @.str.29, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = ptrtoint i8* %20 to i64
  ret i64 %21
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim__43(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i64, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %8 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 1
  %9 = load i32, i32* %8, align 8
  store i32 %9, i32* %3, align 4
  %10 = load i32, i32* %3, align 4
  %11 = icmp ne i32 %10, 4
  br i1 %11, label %12, label %21

12:                                               ; preds = %1
  %13 = load i32, i32* %3, align 4
  %14 = icmp ne i32 %13, 1
  br i1 %14, label %15, label %21

15:                                               ; preds = %12
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %17 = load i32, i32* %3, align 4
  %18 = call i8* @get_type_name(i32 noundef %17)
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.18, i64 0, i64 0), i8* noundef %18)
  %20 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

21:                                               ; preds = %12, %1
  store i64 0, i64* %4, align 8
  br label %22

22:                                               ; preds = %27, %21
  %23 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %24 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %23, i32 0, i32 1
  %25 = load i32, i32* %24, align 8
  %26 = icmp eq i32 %25, 4
  br i1 %26, label %27, label %32

27:                                               ; preds = %22
  %28 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  call void @_get_both(%struct.SinObj* noundef %28, %struct.SinObj* noundef %5, %struct.SinObj* noundef %6)
  %29 = call i64 @unwrap_int(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.19, i64 0, i64 0))
  %30 = load i64, i64* %4, align 8
  %31 = add nsw i64 %30, %29
  store i64 %31, i64* %4, align 8
  store %struct.SinObj* %6, %struct.SinObj** %2, align 8
  br label %22, !llvm.loop !12

32:                                               ; preds = %22
  %33 = load i64, i64* %4, align 8
  %34 = call %struct.SinObj* @const_init_int(i64 noundef %33)
  ret %struct.SinObj* %34
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local i8* @get_type_name(i32 noundef %0) #2 {
  %2 = alloca i8*, align 8
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  switch i32 %4, label %18 [
    i32 0, label %5
    i32 1, label %6
    i32 2, label %7
    i32 12, label %8
    i32 3, label %9
    i32 4, label %10
    i32 5, label %11
    i32 6, label %12
    i32 7, label %13
    i32 8, label %14
    i32 9, label %15
    i32 10, label %16
    i32 11, label %17
  ]

5:                                                ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.65, i64 0, i64 0), i8** %2, align 8
  br label %19

6:                                                ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.66, i64 0, i64 0), i8** %2, align 8
  br label %19

7:                                                ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.67, i64 0, i64 0), i8** %2, align 8
  br label %19

8:                                                ; preds = %1
  store i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.68, i64 0, i64 0), i8** %2, align 8
  br label %19

9:                                                ; preds = %1
  store i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.69, i64 0, i64 0), i8** %2, align 8
  br label %19

10:                                               ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.70, i64 0, i64 0), i8** %2, align 8
  br label %19

11:                                               ; preds = %1
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.71, i64 0, i64 0), i8** %2, align 8
  br label %19

12:                                               ; preds = %1
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.72, i64 0, i64 0), i8** %2, align 8
  br label %19

13:                                               ; preds = %1
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.73, i64 0, i64 0), i8** %2, align 8
  br label %19

14:                                               ; preds = %1
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.74, i64 0, i64 0), i8** %2, align 8
  br label %19

15:                                               ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.75, i64 0, i64 0), i8** %2, align 8
  br label %19

16:                                               ; preds = %1
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.76, i64 0, i64 0), i8** %2, align 8
  br label %19

17:                                               ; preds = %1
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.77, i64 0, i64 0), i8** %2, align 8
  br label %19

18:                                               ; preds = %1
  call void @llvm.trap()
  unreachable

19:                                               ; preds = %17, %16, %15, %14, %13, %12, %11, %10, %9, %8, %7, %6, %5
  %20 = load i8*, i8** %2, align 8
  ret i8* %20
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_make_45vector(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([24 x i8], [24 x i8]* @.str.20, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([24 x i8], [24 x i8]* @.str.20, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([24 x i8], [24 x i8]* @.str.20, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([24 x i8], [24 x i8]* @.str.20, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([24 x i8], [24 x i8]* @.str.20, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim_make_45vector(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @closure_env_get(%struct.SinObj* noundef %0, i64 noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i64 %1, i64* %4, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %8 = call %struct.SinObj* @unwrap_clo(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.24, i64 0, i64 0))
  store %struct.SinObj* %8, %struct.SinObj** %5, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i64 1
  %11 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i32 0, i32 0
  %12 = load i8*, i8** %11, align 8
  %13 = bitcast i8* %12 to %struct.SinObj*
  store %struct.SinObj* %13, %struct.SinObj** %6, align 8
  %14 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %15 = load i64, i64* %4, align 8
  %16 = call %struct.SinObj* @const_init_int(i64 noundef %15)
  %17 = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* noundef %14, %struct.SinObj* noundef %16)
  ret %struct.SinObj* %17
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_vector_45ref(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %8 = call %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([11 x i8], [11 x i8]* @.str.87, i64 0, i64 0))
  store %struct.SinObj* %8, %struct.SinObj** %5, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.SinObj* noundef %9, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.88, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %12 = load i64, i64* %6, align 8
  call void @bounds_check(%struct.SinObj* noundef %11, i64 noundef %12)
  %13 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %14 = load i64, i64* %6, align 8
  %15 = add nsw i64 %14, 1
  %16 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %13, i64 %15
  %17 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %16, i32 0, i32 0
  %18 = load i8*, i8** %17, align 8
  %19 = bitcast i8* %18 to %struct.SinObj*
  ret %struct.SinObj* %19
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.Map* @unwrap_hash(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 9
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([49 x i8], [49 x i8]* @.str.25, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = bitcast i8* %20 to %struct.Map*
  ret %struct.Map* %21
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 8
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([53 x i8], [53 x i8]* @.str.27, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = bitcast i8* %20 to %struct.SinObj*
  ret %struct.SinObj* %21
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i64 @unwrap_bool(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 2
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([49 x i8], [49 x i8]* @.str.30, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = ptrtoint i8* %20 to i64
  ret i64 %21
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i8* @unwrap_str(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 6
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([47 x i8], [47 x i8]* @.str.31, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  ret i8* %20
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i8* @unwrap_sym(%struct.SinObj* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 7
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %11 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %12 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([47 x i8], [47 x i8]* @.str.32, i64 0, i64 0), i32 noundef %13, i8* noundef %14)
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  ret i8* %20
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local zeroext i1 @is_truthy_value(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 2
  br i1 %6, label %7, label %11

7:                                                ; preds = %1
  %8 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %9 = call i64 @unwrap_bool(%struct.SinObj* noundef %8, i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.33, i64 0, i64 0))
  %10 = icmp eq i64 %9, 0
  br label %11

11:                                               ; preds = %7, %1
  %12 = phi i1 [ false, %1 ], [ %10, %7 ]
  %13 = zext i1 %12 to i64
  %14 = select i1 %12, i1 false, i1 true
  ret i1 %14
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_display(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.34, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.34, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.34, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_display(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_print(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca i32, align 4
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %4, i32 0, i32 1
  %6 = load i32, i32* %5, align 8
  store i32 %6, i32* %3, align 4
  %7 = load i32, i32* %3, align 4
  switch i32 %7, label %23 [
    i32 1, label %8
    i32 7, label %10
    i32 0, label %14
    i32 4, label %15
    i32 2, label %20
    i32 12, label %20
    i32 3, label %20
    i32 5, label %20
    i32 6, label %20
    i32 8, label %20
    i32 9, label %20
    i32 10, label %20
    i32 11, label %20
  ]

8:                                                ; preds = %1
  %9 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.36, i64 0, i64 0))
  br label %23

10:                                               ; preds = %1
  %11 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %12 = call i8* @unwrap_sym(%struct.SinObj* noundef %11, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.38, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.37, i64 0, i64 0), i8* noundef %12)
  br label %23

14:                                               ; preds = %1
  br label %23

15:                                               ; preds = %1
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.39, i64 0, i64 0))
  %17 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %18 = call %struct.SinObj* @print_cons(%struct.SinObj* noundef %17)
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.40, i64 0, i64 0))
  br label %23

20:                                               ; preds = %1, %1, %1, %1, %1, %1, %1, %1, %1
  %21 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %22 = call %struct.SinObj* @prim_print_aux(%struct.SinObj* noundef %21)
  br label %23

23:                                               ; preds = %1, %20, %15, %14, %10, %8
  %24 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %24
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_print(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.35, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.35, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.35, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_print(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @print_cons(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %6, i8* noundef getelementptr inbounds ([11 x i8], [11 x i8]* @.str.59, i64 0, i64 0))
  store %struct.SinObj* %7, %struct.SinObj** %3, align 8
  %8 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %9 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i64 0
  store %struct.SinObj* %9, %struct.SinObj** %4, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %11 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i64 1
  store %struct.SinObj* %11, %struct.SinObj** %5, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %13 = call %struct.SinObj* @prim_print_aux(%struct.SinObj* noundef %12)
  %14 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %15 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %14, i32 0, i32 1
  %16 = load i32, i32* %15, align 8
  switch i32 %16, label %28 [
    i32 1, label %17
    i32 0, label %18
    i32 4, label %20
    i32 12, label %24
    i32 7, label %24
    i32 2, label %24
    i32 3, label %24
    i32 5, label %24
    i32 6, label %24
    i32 8, label %24
    i32 9, label %24
    i32 10, label %24
    i32 11, label %24
  ]

17:                                               ; preds = %1
  br label %28

18:                                               ; preds = %1
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([8 x i8], [8 x i8]* @.str.42, i64 0, i64 0))
  br label %28

20:                                               ; preds = %1
  %21 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.60, i64 0, i64 0))
  %22 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %23 = call %struct.SinObj* @print_cons(%struct.SinObj* noundef %22)
  br label %28

24:                                               ; preds = %1, %1, %1, %1, %1, %1, %1, %1, %1, %1
  %25 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.61, i64 0, i64 0))
  %26 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %27 = call %struct.SinObj* @prim_print_aux(%struct.SinObj* noundef %26)
  br label %28

28:                                               ; preds = %1, %24, %20, %18, %17
  %29 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %29
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_print_aux(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  store i32 %7, i32* %3, align 4
  %8 = load i32, i32* %3, align 4
  switch i32 %8, label %67 [
    i32 0, label %9
    i32 1, label %11
    i32 2, label %13
    i32 12, label %30
    i32 3, label %32
    i32 4, label %34
    i32 5, label %39
    i32 6, label %43
    i32 7, label %47
    i32 8, label %51
    i32 9, label %54
    i32 10, label %59
    i32 11, label %61
  ]

9:                                                ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([8 x i8], [8 x i8]* @.str.42, i64 0, i64 0))
  br label %67

11:                                               ; preds = %1
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.43, i64 0, i64 0))
  br label %67

13:                                               ; preds = %1
  %14 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %15 = call i64 @unwrap_bool(%struct.SinObj* noundef %14, i8* noundef getelementptr inbounds ([25 x i8], [25 x i8]* @.str.44, i64 0, i64 0))
  store i64 %15, i64* %4, align 8
  %16 = load i64, i64* %4, align 8
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %18, label %20

18:                                               ; preds = %13
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.45, i64 0, i64 0))
  br label %29

20:                                               ; preds = %13
  %21 = load i64, i64* %4, align 8
  %22 = icmp eq i64 %21, 1
  br i1 %22, label %23, label %25

23:                                               ; preds = %20
  %24 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.46, i64 0, i64 0))
  br label %28

25:                                               ; preds = %20
  %26 = load i64, i64* %4, align 8
  %27 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([27 x i8], [27 x i8]* @.str.47, i64 0, i64 0), i64 noundef %26)
  br label %28

28:                                               ; preds = %25, %23
  br label %29

29:                                               ; preds = %28, %18
  br label %67

30:                                               ; preds = %1
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.48, i64 0, i64 0))
  br label %67

32:                                               ; preds = %1
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([13 x i8], [13 x i8]* @.str.49, i64 0, i64 0))
  br label %67

34:                                               ; preds = %1
  %35 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.50, i64 0, i64 0))
  %36 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %37 = call %struct.SinObj* @print_cons(%struct.SinObj* noundef %36)
  %38 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.40, i64 0, i64 0))
  br label %67

39:                                               ; preds = %1
  %40 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %41 = call i64 @unwrap_int(%struct.SinObj* noundef %40, i8* noundef getelementptr inbounds ([25 x i8], [25 x i8]* @.str.52, i64 0, i64 0))
  %42 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.51, i64 0, i64 0), i64 noundef %41)
  br label %67

43:                                               ; preds = %1
  %44 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %45 = call i8* @unwrap_str(%struct.SinObj* noundef %44, i8* noundef getelementptr inbounds ([25 x i8], [25 x i8]* @.str.54, i64 0, i64 0))
  %46 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.53, i64 0, i64 0), i8* noundef %45)
  br label %67

47:                                               ; preds = %1
  %48 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %49 = call i8* @unwrap_sym(%struct.SinObj* noundef %48, i8* noundef getelementptr inbounds ([25 x i8], [25 x i8]* @.str.55, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.53, i64 0, i64 0), i8* noundef %49)
  br label %67

51:                                               ; preds = %1
  %52 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %53 = call %struct.SinObj* @print_vector(%struct.SinObj* noundef %52)
  br label %67

54:                                               ; preds = %1
  %55 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.56, i64 0, i64 0))
  %56 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %57 = call %struct.SinObj* @print_hash(%struct.SinObj* noundef %56)
  %58 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.40, i64 0, i64 0))
  br label %67

59:                                               ; preds = %1
  %60 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([30 x i8], [30 x i8]* @.str.57, i64 0, i64 0))
  br label %67

61:                                               ; preds = %1
  %62 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %63 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %62, i32 0, i32 0
  %64 = load i8*, i8** %63, align 8
  %65 = ptrtoint i8* %64 to i64
  %66 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([34 x i8], [34 x i8]* @.str.58, i64 0, i64 0), i64 noundef %65)
  br label %67

67:                                               ; preds = %1, %61, %59, %54, %51, %47, %43, %39, %34, %32, %30, %29, %11, %9
  %68 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %68
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_println(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.41, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.41, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.41, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_println(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_println(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = call %struct.SinObj* @prim_print(%struct.SinObj* noundef %3)
  %5 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  %6 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %6
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @print_vector(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %8 = call %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([13 x i8], [13 x i8]* @.str.62, i64 0, i64 0))
  store %struct.SinObj* %8, %struct.SinObj** %3, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = call i64 @_get_vector_length(%struct.SinObj* noundef %9)
  store i64 %10, i64* %4, align 8
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @.str.63, i64 0, i64 0))
  store i64 1, i64* %5, align 8
  br label %12

12:                                               ; preds = %30, %1
  %13 = load i64, i64* %5, align 8
  %14 = load i64, i64* %4, align 8
  %15 = icmp ule i64 %13, %14
  br i1 %15, label %16, label %33

16:                                               ; preds = %12
  %17 = load i64, i64* %5, align 8
  %18 = icmp ne i64 %17, 1
  br i1 %18, label %19, label %21

19:                                               ; preds = %16
  %20 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.60, i64 0, i64 0))
  br label %21

21:                                               ; preds = %19, %16
  %22 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %23 = load i64, i64* %5, align 8
  %24 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %22, i64 %23
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i32 0, i32 0
  %26 = load i8*, i8** %25, align 8
  %27 = bitcast i8* %26 to %struct.SinObj*
  store %struct.SinObj* %27, %struct.SinObj** %6, align 8
  %28 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %29 = call %struct.SinObj* @prim_print_aux(%struct.SinObj* noundef %28)
  br label %30

30:                                               ; preds = %21
  %31 = load i64, i64* %5, align 8
  %32 = add i64 %31, 1
  store i64 %32, i64* %5, align 8
  br label %12, !llvm.loop !13

33:                                               ; preds = %12
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.40, i64 0, i64 0))
  %35 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %35
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @print_hash(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.Map*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.Map* @unwrap_hash(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([11 x i8], [11 x i8]* @.str.64, i64 0, i64 0))
  store %struct.Map* %5, %struct.Map** %3, align 8
  br label %6

6:                                                ; preds = %27, %1
  %7 = load %struct.Map*, %struct.Map** %3, align 8
  %8 = icmp ne %struct.Map* %7, null
  br i1 %8, label %9, label %31

9:                                                ; preds = %6
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.50, i64 0, i64 0))
  %11 = load %struct.Map*, %struct.Map** %3, align 8
  %12 = getelementptr inbounds %struct.Map, %struct.Map* %11, i32 0, i32 1
  %13 = load %struct.SinObj*, %struct.SinObj** %12, align 8
  %14 = call %struct.SinObj* @prim_print_aux(%struct.SinObj* noundef %13)
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.61, i64 0, i64 0))
  %16 = load %struct.Map*, %struct.Map** %3, align 8
  %17 = getelementptr inbounds %struct.Map, %struct.Map* %16, i32 0, i32 2
  %18 = load %struct.SinObj*, %struct.SinObj** %17, align 8
  %19 = call %struct.SinObj* @prim_print_aux(%struct.SinObj* noundef %18)
  %20 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.40, i64 0, i64 0))
  %21 = load %struct.Map*, %struct.Map** %3, align 8
  %22 = getelementptr inbounds %struct.Map, %struct.Map* %21, i32 0, i32 0
  %23 = load %struct.Map*, %struct.Map** %22, align 8
  %24 = icmp ne %struct.Map* %23, null
  br i1 %24, label %25, label %27

25:                                               ; preds = %9
  %26 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.60, i64 0, i64 0))
  br label %27

27:                                               ; preds = %25, %9
  %28 = load %struct.Map*, %struct.Map** %3, align 8
  %29 = getelementptr inbounds %struct.Map, %struct.Map* %28, i32 0, i32 0
  %30 = load %struct.Map*, %struct.Map** %29, align 8
  store %struct.Map* %30, %struct.Map** %3, align 8
  br label %6, !llvm.loop !14

31:                                               ; preds = %6
  %32 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %32
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i64 @_get_vector_length(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.79, i64 0, i64 0))
  store %struct.SinObj* %5, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i64 0
  %8 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %7, i32 0, i32 0
  %9 = load i8*, i8** %8, align 8
  %10 = ptrtoint i8* %9 to i64
  ret i64 %10
}

; Function Attrs: cold noreturn nounwind
declare void @llvm.trap() #6

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_halt(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.78, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.78, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.78, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_halt(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_halt(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = call %struct.SinObj* @prim_print(%struct.SinObj* noundef %3)
  %5 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 0
  br i1 %8, label %9, label %11

9:                                                ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  br label %11

11:                                               ; preds = %9, %1
  call void @exit(i32 noundef 0) #11
  unreachable
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_vector(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj, align 8
  %4 = alloca [256 x %struct.SinObj*], align 16
  %5 = alloca i64, align 8
  %6 = alloca %struct.SinObj, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj*, align 8
  %9 = alloca i64, align 8
  %10 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %11 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %12 = bitcast %struct.SinObj* %3 to i8*
  %13 = bitcast %struct.SinObj* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %12, i8* align 8 %13, i64 16, i1 false)
  %14 = bitcast [256 x %struct.SinObj*]* %4 to i8*
  call void @llvm.memset.p0i8.i64(i8* align 16 %14, i8 0, i64 2048, i1 false)
  store i64 0, i64* %5, align 8
  br label %15

15:                                               ; preds = %24, %1
  %16 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %17 = load i32, i32* %16, align 8
  %18 = icmp eq i32 %17, 4
  br i1 %18, label %19, label %22

19:                                               ; preds = %15
  %20 = load i64, i64* %5, align 8
  %21 = icmp ult i64 %20, 256
  br label %22

22:                                               ; preds = %19, %15
  %23 = phi i1 [ false, %15 ], [ %21, %19 ]
  br i1 %23, label %24, label %43

24:                                               ; preds = %22
  call void @_get_both(%struct.SinObj* noundef %3, %struct.SinObj* noundef %6, %struct.SinObj* noundef %7)
  %25 = call %struct.SinObj* @alloc(i64 noundef 1)
  %26 = load i64, i64* %5, align 8
  %27 = getelementptr inbounds [256 x %struct.SinObj*], [256 x %struct.SinObj*]* %4, i64 0, i64 %26
  store %struct.SinObj* %25, %struct.SinObj** %27, align 8
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = load i64, i64* %5, align 8
  %31 = getelementptr inbounds [256 x %struct.SinObj*], [256 x %struct.SinObj*]* %4, i64 0, i64 %30
  %32 = load %struct.SinObj*, %struct.SinObj** %31, align 8
  %33 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %32, i32 0, i32 1
  store i32 %29, i32* %33, align 8
  %34 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 0
  %35 = load i8*, i8** %34, align 8
  %36 = load i64, i64* %5, align 8
  %37 = add i64 %36, 1
  store i64 %37, i64* %5, align 8
  %38 = getelementptr inbounds [256 x %struct.SinObj*], [256 x %struct.SinObj*]* %4, i64 0, i64 %36
  %39 = load %struct.SinObj*, %struct.SinObj** %38, align 8
  %40 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %39, i32 0, i32 0
  store i8* %35, i8** %40, align 8
  %41 = bitcast %struct.SinObj* %3 to i8*
  %42 = bitcast %struct.SinObj* %7 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %41, i8* align 8 %42, i64 16, i1 false)
  br label %15, !llvm.loop !15

43:                                               ; preds = %22
  %44 = load i64, i64* %5, align 8
  %45 = icmp eq i64 %44, 256
  br i1 %45, label %46, label %54

46:                                               ; preds = %43
  %47 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %48 = load i32, i32* %47, align 8
  %49 = icmp ne i32 %48, 1
  br i1 %49, label %50, label %54

50:                                               ; preds = %46
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %52 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([59 x i8], [59 x i8]* @.str.80, i64 0, i64 0))
  %53 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

54:                                               ; preds = %46, %43
  %55 = load i64, i64* %5, align 8
  %56 = add i64 %55, 1
  %57 = call %struct.SinObj* @alloc(i64 noundef %56)
  store %struct.SinObj* %57, %struct.SinObj** %8, align 8
  %58 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %59 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %58, i64 0
  %60 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %59, i32 0, i32 1
  store i32 5, i32* %60, align 8
  %61 = load i64, i64* %5, align 8
  %62 = inttoptr i64 %61 to i64*
  %63 = bitcast i64* %62 to i8*
  %64 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %65 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %64, i64 0
  %66 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %65, i32 0, i32 0
  store i8* %63, i8** %66, align 8
  store i64 0, i64* %9, align 8
  br label %67

67:                                               ; preds = %87, %54
  %68 = load i64, i64* %9, align 8
  %69 = load i64, i64* %5, align 8
  %70 = icmp ult i64 %68, %69
  br i1 %70, label %71, label %90

71:                                               ; preds = %67
  %72 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %73 = load i64, i64* %9, align 8
  %74 = add i64 %73, 1
  %75 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %72, i64 %74
  %76 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %75, i32 0, i32 1
  store i32 11, i32* %76, align 8
  %77 = load i64, i64* %9, align 8
  %78 = getelementptr inbounds [256 x %struct.SinObj*], [256 x %struct.SinObj*]* %4, i64 0, i64 %77
  %79 = load %struct.SinObj*, %struct.SinObj** %78, align 8
  %80 = bitcast %struct.SinObj* %79 to i64*
  %81 = bitcast i64* %80 to i8*
  %82 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %83 = load i64, i64* %9, align 8
  %84 = add i64 %83, 1
  %85 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %82, i64 %84
  %86 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %85, i32 0, i32 0
  store i8* %81, i8** %86, align 8
  br label %87

87:                                               ; preds = %71
  %88 = load i64, i64* %9, align 8
  %89 = add i64 %88, 1
  store i64 %89, i64* %9, align 8
  br label %67, !llvm.loop !16

90:                                               ; preds = %67
  %91 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %91, %struct.SinObj** %10, align 8
  %92 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %93 = bitcast %struct.SinObj* %92 to i64*
  %94 = bitcast i64* %93 to i8*
  %95 = load %struct.SinObj*, %struct.SinObj** %10, align 8
  %96 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %95, i32 0, i32 0
  store i8* %94, i8** %96, align 8
  %97 = load %struct.SinObj*, %struct.SinObj** %10, align 8
  %98 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %97, i32 0, i32 1
  store i32 8, i32* %98, align 8
  %99 = load %struct.SinObj*, %struct.SinObj** %10, align 8
  ret %struct.SinObj* %99
}

; Function Attrs: argmemonly nofree nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #7

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_vector_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.81, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.81, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.81, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_vector_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_vector_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 8
  br i1 %6, label %7, label %9

7:                                                ; preds = %1
  %8 = call %struct.SinObj* @const_init_true()
  br label %11

9:                                                ; preds = %1
  %10 = call %struct.SinObj* @const_init_false()
  br label %11

11:                                               ; preds = %9, %7
  %12 = phi %struct.SinObj* [ %8, %7 ], [ %10, %9 ]
  ret %struct.SinObj* %12
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_vector_45length(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.82, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.82, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.82, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_vector_45length(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_vector_45length(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.83, i64 0, i64 0))
  store %struct.SinObj* %5, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i64 0
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void @bounds_check(%struct.SinObj* noundef %0, i64 noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store i64 %1, i64* %4, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i64 0
  %8 = call i64 @unwrap_int(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([13 x i8], [13 x i8]* @.str.84, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load i64, i64* %4, align 8
  %10 = add nsw i64 %9, 1
  %11 = load i64, i64* %5, align 8
  %12 = icmp sgt i64 %10, %11
  br i1 %12, label %13, label %19

13:                                               ; preds = %2
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = load i64, i64* %4, align 8
  %16 = load i64, i64* %5, align 8
  %17 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([53 x i8], [53 x i8]* @.str.85, i64 0, i64 0), i64 noundef %15, i64 noundef %16)
  %18 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

19:                                               ; preds = %2
  ret void
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_vector_45ref(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.86, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.86, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.86, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.86, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.86, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_vector_45set_33(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  %9 = alloca %struct.SinObj*, align 8
  %10 = alloca %struct.SinObj, align 8
  %11 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %13 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %12, i32 0, i32 1
  %14 = load i32, i32* %13, align 8
  %15 = icmp ne i32 %14, 4
  br i1 %15, label %16, label %20

16:                                               ; preds = %1
  %17 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %18 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.89, i64 0, i64 0))
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

20:                                               ; preds = %1
  %21 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %22 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %21, i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.89, i64 0, i64 0))
  store %struct.SinObj* %22, %struct.SinObj** %3, align 8
  %23 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %24 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %23, i64 0
  %25 = bitcast %struct.SinObj* %4 to i8*
  %26 = bitcast %struct.SinObj* %24 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %25, i8* align 8 %26, i64 16, i1 false)
  %27 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %27, i64 1
  %29 = bitcast %struct.SinObj* %5 to i8*
  %30 = bitcast %struct.SinObj* %28 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %29, i8* align 8 %30, i64 16, i1 false)
  %31 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %32 = load i32, i32* %31, align 8
  %33 = icmp ne i32 %32, 4
  br i1 %33, label %34, label %38

34:                                               ; preds = %20
  %35 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %36 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.90, i64 0, i64 0), i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.89, i64 0, i64 0))
  %37 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

38:                                               ; preds = %20
  %39 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.89, i64 0, i64 0))
  store %struct.SinObj* %39, %struct.SinObj** %6, align 8
  %40 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %41 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %40, i64 0
  %42 = bitcast %struct.SinObj* %7 to i8*
  %43 = bitcast %struct.SinObj* %41 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %42, i8* align 8 %43, i64 16, i1 false)
  %44 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %44, i64 1
  %46 = bitcast %struct.SinObj* %8 to i8*
  %47 = bitcast %struct.SinObj* %45 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %46, i8* align 8 %47, i64 16, i1 false)
  %48 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %49 = load i32, i32* %48, align 8
  %50 = icmp ne i32 %49, 4
  br i1 %50, label %51, label %55

51:                                               ; preds = %38
  %52 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %53 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.91, i64 0, i64 0), i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.89, i64 0, i64 0))
  %54 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

55:                                               ; preds = %38
  %56 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %8, i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.89, i64 0, i64 0))
  store %struct.SinObj* %56, %struct.SinObj** %9, align 8
  %57 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %58 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %57, i64 0
  %59 = bitcast %struct.SinObj* %10 to i8*
  %60 = bitcast %struct.SinObj* %58 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %59, i8* align 8 %60, i64 16, i1 false)
  %61 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %62 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %61, i64 1
  %63 = bitcast %struct.SinObj* %11 to i8*
  %64 = bitcast %struct.SinObj* %62 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %63, i8* align 8 %64, i64 16, i1 false)
  %65 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %66 = load i32, i32* %65, align 8
  %67 = icmp ne i32 %66, 1
  br i1 %67, label %68, label %72

68:                                               ; preds = %55
  %69 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %70 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.92, i64 0, i64 0), i8* noundef getelementptr inbounds ([26 x i8], [26 x i8]* @.str.89, i64 0, i64 0))
  %71 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

72:                                               ; preds = %55
  %73 = call %struct.SinObj* @prim_vector_45set_33(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7, %struct.SinObj* noundef %10)
  ret %struct.SinObj* %73
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_void(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp ne i32 %5, 1
  br i1 %6, label %7, label %11

7:                                                ; preds = %1
  %8 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %9 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([64 x i8], [64 x i8]* @.str.95, i64 0, i64 0), i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.96, i64 0, i64 0))
  %10 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

11:                                               ; preds = %1
  %12 = call %struct.SinObj* @prim_void()
  ret %struct.SinObj* %12
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_void() #2 {
  %1 = call %struct.SinObj* @const_init_void()
  ret %struct.SinObj* %1
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i32 @cons_eq_helper(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  call void @_get_both(%struct.SinObj* noundef %9, %struct.SinObj* noundef %5, %struct.SinObj* noundef %6)
  %10 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  call void @_get_both(%struct.SinObj* noundef %10, %struct.SinObj* noundef %7, %struct.SinObj* noundef %8)
  %11 = call i32 @eq_helper(%struct.SinObj* noundef %5, %struct.SinObj* noundef %7)
  %12 = icmp eq i32 %11, 1
  br i1 %12, label %13, label %16

13:                                               ; preds = %2
  %14 = call i32 @eq_helper(%struct.SinObj* noundef %6, %struct.SinObj* noundef %8)
  %15 = icmp ne i32 %14, 0
  br label %16

16:                                               ; preds = %13, %2
  %17 = phi i1 [ false, %2 ], [ %15, %13 ]
  %18 = zext i1 %17 to i32
  ret i32 %18
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local i32 @vec_eq_helper(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj*, align 8
  %8 = alloca i64, align 8
  %9 = alloca i64, align 8
  %10 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  %11 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %12 = call %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %11, i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.97, i64 0, i64 0))
  store %struct.SinObj* %12, %struct.SinObj** %6, align 8
  %13 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %14 = call %struct.SinObj* @unwrap_vector(%struct.SinObj* noundef %13, i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.98, i64 0, i64 0))
  store %struct.SinObj* %14, %struct.SinObj** %7, align 8
  %15 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %16 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %15, i64 0
  %17 = call i64 @unwrap_int(%struct.SinObj* noundef %16, i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.99, i64 0, i64 0))
  store i64 %17, i64* %8, align 8
  %18 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i64 0
  %20 = call i64 @unwrap_int(%struct.SinObj* noundef %19, i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.100, i64 0, i64 0))
  store i64 %20, i64* %9, align 8
  %21 = load i64, i64* %8, align 8
  %22 = load i64, i64* %9, align 8
  %23 = icmp ne i64 %21, %22
  br i1 %23, label %24, label %25

24:                                               ; preds = %2
  store i32 0, i32* %3, align 4
  br label %45

25:                                               ; preds = %2
  store i64 0, i64* %10, align 8
  br label %26

26:                                               ; preds = %41, %25
  %27 = load i64, i64* %10, align 8
  %28 = load i64, i64* %8, align 8
  %29 = icmp sle i64 %27, %28
  br i1 %29, label %30, label %44

30:                                               ; preds = %26
  %31 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %32 = load i64, i64* %10, align 8
  %33 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %31, i64 %32
  %34 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %35 = load i64, i64* %10, align 8
  %36 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %34, i64 %35
  %37 = call i32 @eq_helper(%struct.SinObj* noundef %33, %struct.SinObj* noundef %36)
  %38 = icmp eq i32 %37, 0
  br i1 %38, label %39, label %40

39:                                               ; preds = %30
  store i32 0, i32* %3, align 4
  br label %45

40:                                               ; preds = %30
  br label %41

41:                                               ; preds = %40
  %42 = load i64, i64* %10, align 8
  %43 = add nsw i64 %42, 1
  store i64 %43, i64* %10, align 8
  br label %26, !llvm.loop !17

44:                                               ; preds = %26
  store i32 1, i32* %3, align 4
  br label %45

45:                                               ; preds = %44, %39, %24
  %46 = load i32, i32* %3, align 4
  ret i32 %46
}

; Function Attrs: nounwind readonly willreturn
declare i32 @strcmp(i8* noundef, i8* noundef) #8

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_eq_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.112, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.112, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.112, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.112, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([16 x i8], [16 x i8]* @.str.112, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim_eq_63(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_eq_63(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %7 = call i32 @eq_helper(%struct.SinObj* noundef %5, %struct.SinObj* noundef %6)
  %8 = icmp eq i32 %7, 1
  %9 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %8)
  ret %struct.SinObj* %9
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @make_predicate(i1 noundef zeroext %0) #2 {
  %2 = alloca i8, align 1
  %3 = zext i1 %0 to i8
  store i8 %3, i8* %2, align 1
  %4 = load i8, i8* %2, align 1
  %5 = trunc i8 %4 to i1
  br i1 %5, label %6, label %8

6:                                                ; preds = %1
  %7 = call %struct.SinObj* @const_init_true()
  br label %10

8:                                                ; preds = %1
  %9 = call %struct.SinObj* @const_init_false()
  br label %10

10:                                               ; preds = %8, %6
  %11 = phi %struct.SinObj* [ %7, %6 ], [ %9, %8 ]
  ret %struct.SinObj* %11
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_eqv_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.113, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.113, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.113, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.113, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.113, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim_eqv_63(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_eqv_63(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %7 = call %struct.SinObj* @prim_eq_63(%struct.SinObj* noundef %5, %struct.SinObj* noundef %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_equal_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.114, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.114, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.114, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.114, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([19 x i8], [19 x i8]* @.str.114, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim_equal_63(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_equal_63(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %7 = call %struct.SinObj* @prim_eq_63(%struct.SinObj* noundef %5, %struct.SinObj* noundef %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_number_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.115, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.115, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.115, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_number_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_number_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = call %struct.SinObj* @prim_integer_63(%struct.SinObj* noundef %3)
  ret %struct.SinObj* %4
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_integer_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 5
  %7 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_integer_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.116, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.116, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.116, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_integer_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_boolean_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.117, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.117, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.117, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_boolean_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_boolean_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 2
  %7 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_void_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.118, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.118, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.118, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_void_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_void_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 0
  %7 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_procedure_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.119, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.119, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.119, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_procedure_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_procedure_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 3
  %7 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_null_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.120, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.120, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.120, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_null_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_null_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 1
  %7 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_cons_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.121, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.121, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.121, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_cons_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_cons_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 4
  %7 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_cons(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.122, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.122, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.122, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.122, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.122, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim_cons(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_cdr(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.123, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.123, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.123, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_cdr(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_cdr(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.124, i64 0, i64 0))
  store %struct.SinObj* %5, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i64 1
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim__45(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %8 = call i64 @unwrap_int(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.125, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.SinObj* noundef %9, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.126, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %5, align 8
  %12 = load i64, i64* %6, align 8
  %13 = sub nsw i64 %11, %12
  %14 = call %struct.SinObj* @const_init_int(i64 noundef %13)
  ret %struct.SinObj* %14
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim__45(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  %9 = alloca %struct.SinObj, align 8
  %10 = alloca %struct.SinObj, align 8
  %11 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %13 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %12, i8* noundef getelementptr inbounds ([8 x i8], [8 x i8]* @.str.127, i64 0, i64 0))
  store %struct.SinObj* %13, %struct.SinObj** %4, align 8
  %14 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %15 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %14, i64 0
  %16 = bitcast %struct.SinObj* %5 to i8*
  %17 = bitcast %struct.SinObj* %15 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %16, i8* align 8 %17, i64 16, i1 false)
  %18 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %18, i64 1
  %20 = bitcast %struct.SinObj* %6 to i8*
  %21 = bitcast %struct.SinObj* %19 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %20, i8* align 8 %21, i64 16, i1 false)
  %22 = call i64 @unwrap_int(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.128, i64 0, i64 0))
  store i64 %22, i64* %7, align 8
  %23 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %24 = load i32, i32* %23, align 8
  %25 = icmp eq i32 %24, 1
  br i1 %25, label %26, label %30

26:                                               ; preds = %1
  %27 = load i64, i64* %7, align 8
  %28 = sub nsw i64 0, %27
  %29 = call %struct.SinObj* @const_init_int(i64 noundef %28)
  store %struct.SinObj* %29, %struct.SinObj** %2, align 8
  br label %47

30:                                               ; preds = %1
  %31 = load i64, i64* %7, align 8
  store i64 %31, i64* %8, align 8
  %32 = bitcast %struct.SinObj* %9 to i8*
  %33 = bitcast %struct.SinObj* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %32, i8* align 8 %33, i64 16, i1 false)
  br label %34

34:                                               ; preds = %38, %30
  %35 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %36 = load i32, i32* %35, align 8
  %37 = icmp ne i32 %36, 1
  br i1 %37, label %38, label %44

38:                                               ; preds = %34
  call void @_get_both(%struct.SinObj* noundef %9, %struct.SinObj* noundef %10, %struct.SinObj* noundef %11)
  %39 = call i64 @unwrap_int(%struct.SinObj* noundef %10, i8* noundef getelementptr inbounds ([20 x i8], [20 x i8]* @.str.129, i64 0, i64 0))
  %40 = load i64, i64* %8, align 8
  %41 = sub nsw i64 %40, %39
  store i64 %41, i64* %8, align 8
  %42 = bitcast %struct.SinObj* %9 to i8*
  %43 = bitcast %struct.SinObj* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %42, i8* align 8 %43, i64 16, i1 false)
  br label %34, !llvm.loop !18

44:                                               ; preds = %34
  %45 = load i64, i64* %8, align 8
  %46 = call %struct.SinObj* @const_init_int(i64 noundef %45)
  store %struct.SinObj* %46, %struct.SinObj** %2, align 8
  br label %47

47:                                               ; preds = %44, %26
  %48 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  ret %struct.SinObj* %48
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim__42(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = call i64 @unwrap_int(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.130, i64 0, i64 0))
  %7 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %8 = call i64 @unwrap_int(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.131, i64 0, i64 0))
  %9 = mul nsw i64 %6, %8
  %10 = call %struct.SinObj* @const_init_int(i64 noundef %9)
  ret %struct.SinObj* %10
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim__42(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp eq i32 %11, 1
  br i1 %12, label %13, label %15

13:                                               ; preds = %1
  %14 = call %struct.SinObj* @const_init_int(i64 noundef 1)
  store %struct.SinObj* %14, %struct.SinObj** %2, align 8
  br label %42

15:                                               ; preds = %1
  %16 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %17 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %16, i32 0, i32 1
  %18 = load i32, i32* %17, align 8
  %19 = icmp eq i32 %18, 4
  br i1 %19, label %20, label %38

20:                                               ; preds = %15
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %21, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.132, i64 0, i64 0))
  store %struct.SinObj* %22, %struct.SinObj** %4, align 8
  %23 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %24 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %23, i64 0
  %25 = bitcast %struct.SinObj* %5 to i8*
  %26 = bitcast %struct.SinObj* %24 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %25, i8* align 8 %26, i64 16, i1 false)
  %27 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %27, i64 1
  %29 = bitcast %struct.SinObj* %6 to i8*
  %30 = bitcast %struct.SinObj* %28 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %29, i8* align 8 %30, i64 16, i1 false)
  %31 = call i64 @unwrap_int(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.133, i64 0, i64 0))
  store i64 %31, i64* %7, align 8
  %32 = call %struct.SinObj* @applyprim__42(%struct.SinObj* noundef %6)
  %33 = call i64 @unwrap_int(%struct.SinObj* noundef %32, i8* noundef getelementptr inbounds ([15 x i8], [15 x i8]* @.str.134, i64 0, i64 0))
  store i64 %33, i64* %8, align 8
  %34 = load i64, i64* %7, align 8
  %35 = load i64, i64* %8, align 8
  %36 = mul nsw i64 %34, %35
  %37 = call %struct.SinObj* @const_init_int(i64 noundef %36)
  store %struct.SinObj* %37, %struct.SinObj** %2, align 8
  br label %42

38:                                               ; preds = %15
  %39 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %40 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([36 x i8], [36 x i8]* @.str.135, i64 0, i64 0))
  %41 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

42:                                               ; preds = %20, %13
  %43 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  ret %struct.SinObj* %43
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim__47(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %7 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %8 = call i64 @unwrap_int(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.136, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.SinObj* noundef %9, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.137, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %5, align 8
  %12 = load i64, i64* %6, align 8
  %13 = sdiv i64 %11, %12
  %14 = call %struct.SinObj* @const_init_int(i64 noundef %13)
  ret %struct.SinObj* %14
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim__61(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.138, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.138, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.138, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.138, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.138, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim__61(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim__61(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %7 = call i64 @unwrap_int(%struct.SinObj* noundef %6, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.139, i64 0, i64 0))
  %8 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %9 = call i64 @unwrap_int(%struct.SinObj* noundef %8, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.140, i64 0, i64 0))
  %10 = icmp eq i64 %7, %9
  br i1 %10, label %11, label %13

11:                                               ; preds = %2
  %12 = call %struct.SinObj* @const_init_true()
  store %struct.SinObj* %12, %struct.SinObj** %3, align 8
  br label %15

13:                                               ; preds = %2
  %14 = call %struct.SinObj* @const_init_false()
  store %struct.SinObj* %14, %struct.SinObj** %3, align 8
  br label %15

15:                                               ; preds = %13, %11
  %16 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  ret %struct.SinObj* %16
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim__60(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = call i64 @unwrap_int(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.141, i64 0, i64 0))
  %7 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %8 = call i64 @unwrap_int(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.142, i64 0, i64 0))
  %9 = icmp slt i64 %6, %8
  %10 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %9)
  ret %struct.SinObj* %10
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim__60_61(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.143, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.143, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.143, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.143, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([17 x i8], [17 x i8]* @.str.143, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim__60_61(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim__60_61(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = call i64 @unwrap_int(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([5 x i8], [5 x i8]* @.str.144, i64 0, i64 0))
  %7 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %8 = call i64 @unwrap_int(%struct.SinObj* noundef %7, i8* noundef getelementptr inbounds ([5 x i8], [5 x i8]* @.str.145, i64 0, i64 0))
  %9 = icmp sle i64 %6, %8
  %10 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %9)
  ret %struct.SinObj* %10
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_not(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.146, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.146, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.146, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_not(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_not(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = call i64 @unwrap_bool(%struct.SinObj* noundef %3, i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.147, i64 0, i64 0))
  %5 = icmp eq i64 %4, 0
  %6 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %5)
  ret %struct.SinObj* %6
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @map_to_sin(%struct.Map* noundef %0) #2 {
  %2 = alloca %struct.Map*, align 8
  %3 = alloca %struct.SinObj*, align 8
  store %struct.Map* %0, %struct.Map** %2, align 8
  %4 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %4, %struct.SinObj** %3, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  store i32 9, i32* %6, align 8
  %7 = load %struct.Map*, %struct.Map** %2, align 8
  %8 = bitcast %struct.Map* %7 to i64*
  %9 = bitcast i64* %8 to i8*
  %10 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %11 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i32 0, i32 0
  store i8* %9, i8** %11, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  ret %struct.SinObj* %12
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_hash_45has_45key_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([30 x i8], [30 x i8]* @.str.148, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %18, i8* noundef getelementptr inbounds ([30 x i8], [30 x i8]* @.str.148, i64 0, i64 0))
  store %struct.SinObj* %19, %struct.SinObj** %3, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %21 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %20, i64 0
  %22 = bitcast %struct.SinObj* %4 to i8*
  %23 = bitcast %struct.SinObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %24, i64 1
  %26 = bitcast %struct.SinObj* %5 to i8*
  %27 = bitcast %struct.SinObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.21, i64 0, i64 0), i8* noundef getelementptr inbounds ([30 x i8], [30 x i8]* @.str.148, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([30 x i8], [30 x i8]* @.str.148, i64 0, i64 0))
  store %struct.SinObj* %36, %struct.SinObj** %6, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %38 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %37, i64 0
  %39 = bitcast %struct.SinObj* %7 to i8*
  %40 = bitcast %struct.SinObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %42 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %41, i64 1
  %43 = bitcast %struct.SinObj* %8 to i8*
  %44 = bitcast %struct.SinObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.22, i64 0, i64 0), i8* noundef getelementptr inbounds ([30 x i8], [30 x i8]* @.str.148, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.SinObj* @prim_hash_45has_45key_63(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7)
  ret %struct.SinObj* %53
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_hash_45has_45key_63(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.Map*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %7 = call %struct.Map* @unwrap_hash(%struct.SinObj* noundef %6, i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.149, i64 0, i64 0))
  store %struct.Map* %7, %struct.Map** %5, align 8
  %8 = load %struct.Map*, %struct.Map** %5, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %10 = call zeroext i1 @map_has_key(%struct.Map* noundef %8, %struct.SinObj* noundef %9)
  %11 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %10)
  ret %struct.SinObj* %11
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_hash(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.Map*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  store %struct.Map* null, %struct.Map** %3, align 8
  br label %8

8:                                                ; preds = %29, %1
  %9 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %10 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 1
  br i1 %12, label %13, label %38

13:                                               ; preds = %8
  %14 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %14, %struct.SinObj** %4, align 8
  %15 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %15, %struct.SinObj** %5, align 8
  %16 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %16, %struct.SinObj** %6, align 8
  %17 = call %struct.SinObj* @alloc(i64 noundef 1)
  store %struct.SinObj* %17, %struct.SinObj** %7, align 8
  %18 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %19 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %20 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  call void @_get_both(%struct.SinObj* noundef %18, %struct.SinObj* noundef %19, %struct.SinObj* noundef %20)
  %21 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i32 0, i32 1
  %23 = load i32, i32* %22, align 8
  %24 = icmp ne i32 %23, 4
  br i1 %24, label %25, label %29

25:                                               ; preds = %13
  %26 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %27 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([33 x i8], [33 x i8]* @.str.150, i64 0, i64 0))
  %28 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

29:                                               ; preds = %13
  %30 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %31 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %32 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  call void @_get_both(%struct.SinObj* noundef %30, %struct.SinObj* noundef %31, %struct.SinObj* noundef %32)
  %33 = load %struct.Map*, %struct.Map** %3, align 8
  %34 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %35 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %36 = call %struct.Map* @map_insert(%struct.Map* noundef %33, %struct.SinObj* noundef %34, %struct.SinObj* noundef %35)
  store %struct.Map* %36, %struct.Map** %3, align 8
  %37 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  store %struct.SinObj* %37, %struct.SinObj** %2, align 8
  br label %8, !llvm.loop !19

38:                                               ; preds = %8
  %39 = load %struct.Map*, %struct.Map** %3, align 8
  %40 = call %struct.SinObj* @map_to_sin(%struct.Map* noundef %39)
  ret %struct.SinObj* %40
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_hash_45keys(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([22 x i8], [22 x i8]* @.str.151, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([22 x i8], [22 x i8]* @.str.151, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([22 x i8], [22 x i8]* @.str.151, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_hash_45keys(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_hash_45keys(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.Map*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %4 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %5 = call %struct.Map* @unwrap_hash(%struct.SinObj* noundef %4, i8* noundef getelementptr inbounds ([10 x i8], [10 x i8]* @.str.152, i64 0, i64 0))
  store %struct.Map* %5, %struct.Map** %3, align 8
  %6 = load %struct.Map*, %struct.Map** %3, align 8
  %7 = call %struct.SinObj* @map_keys(%struct.Map* noundef %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_hash_45ref(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  %9 = alloca %struct.SinObj, align 8
  %10 = alloca %struct.SinObj, align 8
  %11 = alloca %struct.SinObj, align 8
  %12 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* null, %struct.SinObj** %4, align 8
  store %struct.SinObj* null, %struct.SinObj** %5, align 8
  store %struct.SinObj* null, %struct.SinObj** %6, align 8
  %13 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %14 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %13, i32 0, i32 1
  %15 = load i32, i32* %14, align 8
  %16 = icmp eq i32 %15, 4
  br i1 %16, label %17, label %51

17:                                               ; preds = %1
  %18 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  call void @_get_both(%struct.SinObj* noundef %18, %struct.SinObj* noundef %7, %struct.SinObj* noundef %8)
  store %struct.SinObj* %7, %struct.SinObj** %4, align 8
  %19 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %20 = load i32, i32* %19, align 8
  %21 = icmp eq i32 %20, 4
  br i1 %21, label %22, label %50

22:                                               ; preds = %17
  call void @_get_both(%struct.SinObj* noundef %8, %struct.SinObj* noundef %9, %struct.SinObj* noundef %10)
  store %struct.SinObj* %9, %struct.SinObj** %5, align 8
  %23 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i32 0, i32 1
  %24 = load i32, i32* %23, align 8
  %25 = icmp eq i32 %24, 4
  br i1 %25, label %26, label %39

26:                                               ; preds = %22
  call void @_get_both(%struct.SinObj* noundef %10, %struct.SinObj* noundef %11, %struct.SinObj* noundef %12)
  store %struct.SinObj* %11, %struct.SinObj** %6, align 8
  %27 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %12, i32 0, i32 1
  %28 = load i32, i32* %27, align 8
  %29 = icmp eq i32 %28, 1
  br i1 %29, label %30, label %35

30:                                               ; preds = %26
  %31 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %32 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %33 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %34 = call %struct.SinObj* @hash_ref_impl(%struct.SinObj* noundef %31, %struct.SinObj* noundef %32, %struct.SinObj* noundef %33)
  store %struct.SinObj* %34, %struct.SinObj** %2, align 8
  br label %55

35:                                               ; preds = %26
  %36 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %37 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([32 x i8], [32 x i8]* @.str.153, i64 0, i64 0))
  %38 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

39:                                               ; preds = %22
  %40 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %10, i32 0, i32 1
  %41 = load i32, i32* %40, align 8
  %42 = icmp eq i32 %41, 1
  br i1 %42, label %43, label %48

43:                                               ; preds = %39
  %44 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %45 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %46 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %47 = call %struct.SinObj* @hash_ref_impl(%struct.SinObj* noundef %44, %struct.SinObj* noundef %45, %struct.SinObj* noundef %46)
  store %struct.SinObj* %47, %struct.SinObj** %2, align 8
  br label %55

48:                                               ; preds = %39
  br label %49

49:                                               ; preds = %48
  br label %50

50:                                               ; preds = %49, %17
  br label %51

51:                                               ; preds = %50, %1
  %52 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %53 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.154, i64 0, i64 0))
  %54 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

55:                                               ; preds = %43, %30
  %56 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  ret %struct.SinObj* %56
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @hash_ref_impl(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1, %struct.SinObj* noundef %2) #0 {
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj*, align 8
  %8 = alloca %struct.Map*, align 8
  %9 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %5, align 8
  store %struct.SinObj* %1, %struct.SinObj** %6, align 8
  store %struct.SinObj* %2, %struct.SinObj** %7, align 8
  %10 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %11 = call %struct.Map* @unwrap_hash(%struct.SinObj* noundef %10, i8* noundef getelementptr inbounds ([9 x i8], [9 x i8]* @.str.155, i64 0, i64 0))
  store %struct.Map* %11, %struct.Map** %8, align 8
  %12 = load %struct.Map*, %struct.Map** %8, align 8
  %13 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %14 = call %struct.SinObj* @map_get(%struct.Map* noundef %12, %struct.SinObj* noundef %13)
  store %struct.SinObj* %14, %struct.SinObj** %9, align 8
  %15 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %16 = icmp eq %struct.SinObj* %15, null
  br i1 %16, label %17, label %26

17:                                               ; preds = %3
  %18 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %19 = icmp eq %struct.SinObj* %18, null
  br i1 %19, label %20, label %24

20:                                               ; preds = %17
  %21 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %22 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.156, i64 0, i64 0))
  %23 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

24:                                               ; preds = %17
  %25 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  store %struct.SinObj* %25, %struct.SinObj** %4, align 8
  br label %28

26:                                               ; preds = %3
  %27 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  store %struct.SinObj* %27, %struct.SinObj** %4, align 8
  br label %28

28:                                               ; preds = %26, %24
  %29 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  ret %struct.SinObj* %29
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_hash_45ref(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1) #0 {
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %3, align 8
  store %struct.SinObj* %1, %struct.SinObj** %4, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %7 = call %struct.SinObj* @hash_ref_impl(%struct.SinObj* noundef %5, %struct.SinObj* noundef %6, %struct.SinObj* noundef null)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_hash_45set(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.SinObj, align 8
  %8 = alloca %struct.SinObj, align 8
  %9 = alloca %struct.SinObj*, align 8
  %10 = alloca %struct.SinObj, align 8
  %11 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %13 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %12, i32 0, i32 1
  %14 = load i32, i32* %13, align 8
  %15 = icmp ne i32 %14, 4
  br i1 %15, label %16, label %20

16:                                               ; preds = %1
  %17 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %18 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.157, i64 0, i64 0))
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

20:                                               ; preds = %1
  %21 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %22 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %21, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.157, i64 0, i64 0))
  store %struct.SinObj* %22, %struct.SinObj** %3, align 8
  %23 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %24 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %23, i64 0
  %25 = bitcast %struct.SinObj* %4 to i8*
  %26 = bitcast %struct.SinObj* %24 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %25, i8* align 8 %26, i64 16, i1 false)
  %27 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %28 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %27, i64 1
  %29 = bitcast %struct.SinObj* %5 to i8*
  %30 = bitcast %struct.SinObj* %28 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %29, i8* align 8 %30, i64 16, i1 false)
  %31 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %32 = load i32, i32* %31, align 8
  %33 = icmp ne i32 %32, 4
  br i1 %33, label %34, label %38

34:                                               ; preds = %20
  %35 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %36 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.90, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.157, i64 0, i64 0))
  %37 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

38:                                               ; preds = %20
  %39 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.157, i64 0, i64 0))
  store %struct.SinObj* %39, %struct.SinObj** %6, align 8
  %40 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %41 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %40, i64 0
  %42 = bitcast %struct.SinObj* %7 to i8*
  %43 = bitcast %struct.SinObj* %41 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %42, i8* align 8 %43, i64 16, i1 false)
  %44 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %45 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %44, i64 1
  %46 = bitcast %struct.SinObj* %8 to i8*
  %47 = bitcast %struct.SinObj* %45 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %46, i8* align 8 %47, i64 16, i1 false)
  %48 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %8, i32 0, i32 1
  %49 = load i32, i32* %48, align 8
  %50 = icmp ne i32 %49, 4
  br i1 %50, label %51, label %55

51:                                               ; preds = %38
  %52 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %53 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([46 x i8], [46 x i8]* @.str.91, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.157, i64 0, i64 0))
  %54 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

55:                                               ; preds = %38
  %56 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %8, i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.157, i64 0, i64 0))
  store %struct.SinObj* %56, %struct.SinObj** %9, align 8
  %57 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %58 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %57, i64 0
  %59 = bitcast %struct.SinObj* %10 to i8*
  %60 = bitcast %struct.SinObj* %58 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %59, i8* align 8 %60, i64 16, i1 false)
  %61 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %62 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %61, i64 1
  %63 = bitcast %struct.SinObj* %11 to i8*
  %64 = bitcast %struct.SinObj* %62 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %63, i8* align 8 %64, i64 16, i1 false)
  %65 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %11, i32 0, i32 1
  %66 = load i32, i32* %65, align 8
  %67 = icmp ne i32 %66, 1
  br i1 %67, label %68, label %72

68:                                               ; preds = %55
  %69 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %70 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([38 x i8], [38 x i8]* @.str.92, i64 0, i64 0), i8* noundef getelementptr inbounds ([21 x i8], [21 x i8]* @.str.157, i64 0, i64 0))
  %71 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

72:                                               ; preds = %55
  %73 = call %struct.SinObj* @prim_hash_45set(%struct.SinObj* noundef %4, %struct.SinObj* noundef %7, %struct.SinObj* noundef %10)
  ret %struct.SinObj* %73
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_hash_45set(%struct.SinObj* noundef %0, %struct.SinObj* noundef %1, %struct.SinObj* noundef %2) #0 {
  %4 = alloca %struct.SinObj*, align 8
  %5 = alloca %struct.SinObj*, align 8
  %6 = alloca %struct.SinObj*, align 8
  %7 = alloca %struct.Map*, align 8
  %8 = alloca %struct.Map*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %4, align 8
  store %struct.SinObj* %1, %struct.SinObj** %5, align 8
  store %struct.SinObj* %2, %struct.SinObj** %6, align 8
  %9 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %10 = call %struct.Map* @unwrap_hash(%struct.SinObj* noundef %9, i8* noundef getelementptr inbounds ([9 x i8], [9 x i8]* @.str.158, i64 0, i64 0))
  store %struct.Map* %10, %struct.Map** %7, align 8
  %11 = load %struct.Map*, %struct.Map** %7, align 8
  %12 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %13 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %14 = call %struct.Map* @map_insert(%struct.Map* noundef %11, %struct.SinObj* noundef %12, %struct.SinObj* noundef %13)
  store %struct.Map* %14, %struct.Map** %8, align 8
  %15 = load %struct.Map*, %struct.Map** %8, align 8
  %16 = call %struct.SinObj* @map_to_sin(%struct.Map* noundef %15)
  ret %struct.SinObj* %16
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_hash_63(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.159, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.159, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.159, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_hash_63(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define dso_local %struct.SinObj* @prim_hash_63(%struct.SinObj* noundef %0) #2 {
  %2 = alloca %struct.SinObj*, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %3 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %4 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 9
  %7 = call %struct.SinObj* @make_predicate(i1 noundef zeroext %6)
  ret %struct.SinObj* %7
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @applyprim_hash_45count(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.SinObj*, align 8
  %4 = alloca %struct.SinObj, align 8
  %5 = alloca %struct.SinObj, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %6 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %7 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([55 x i8], [55 x i8]* @.str.12, i64 0, i64 0), i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.160, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %16 = call %struct.SinObj* @unwrap_cons(%struct.SinObj* noundef %15, i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.160, i64 0, i64 0))
  store %struct.SinObj* %16, %struct.SinObj** %3, align 8
  %17 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %18 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %17, i64 0
  %19 = bitcast %struct.SinObj* %4 to i8*
  %20 = bitcast %struct.SinObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %22 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %21, i64 1
  %23 = bitcast %struct.SinObj* %5 to i8*
  %24 = bitcast %struct.SinObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.SinObj, %struct.SinObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([31 x i8], [31 x i8]* @.str.11, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @.str.14, i64 0, i64 0), i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.160, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i64 0, i64 0))
  call void @exit(i32 noundef 1) #11
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.SinObj* @prim_hash_45count(%struct.SinObj* noundef %4)
  ret %struct.SinObj* %33
}

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local %struct.SinObj* @prim_hash_45count(%struct.SinObj* noundef %0) #0 {
  %2 = alloca %struct.SinObj*, align 8
  %3 = alloca %struct.Map*, align 8
  %4 = alloca i64, align 8
  store %struct.SinObj* %0, %struct.SinObj** %2, align 8
  %5 = load %struct.SinObj*, %struct.SinObj** %2, align 8
  %6 = call %struct.Map* @unwrap_hash(%struct.SinObj* noundef %5, i8* noundef getelementptr inbounds ([11 x i8], [11 x i8]* @.str.161, i64 0, i64 0))
  store %struct.Map* %6, %struct.Map** %3, align 8
  %7 = load %struct.Map*, %struct.Map** %3, align 8
  %8 = call i64 @map_count(%struct.Map* noundef %7)
  store i64 %8, i64* %4, align 8
  %9 = load i64, i64* %4, align 8
  %10 = call %struct.SinObj* @const_init_int(i64 noundef %9)
  ret %struct.SinObj* %10
}

attributes #0 = { mustprogress noinline optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { mustprogress noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { argmemonly nofree nounwind willreturn }
attributes #5 = { noreturn nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #6 = { cold noreturn nounwind }
attributes #7 = { argmemonly nofree nounwind willreturn writeonly }
attributes #8 = { nounwind readonly willreturn "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #9 = { nounwind }
attributes #10 = { nounwind readonly willreturn }
attributes #11 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Ubuntu clang version 14.0.4-++20220524103116+29f1039a7285-1~exp1~20220524223156.141"}
!6 = distinct !{!6, !7}
!7 = !{!"llvm.loop.mustprogress"}
!8 = distinct !{!8, !7}
!9 = distinct !{!9, !7}
!10 = distinct !{!10, !7}
!11 = distinct !{!11, !7}
!12 = distinct !{!12, !7}
!13 = distinct !{!13, !7}
!14 = distinct !{!14, !7}
!15 = distinct !{!15, !7}
!16 = distinct !{!16, !7}
!17 = distinct !{!17, !7}
!18 = distinct !{!18, !7}
!19 = distinct !{!19, !7}


;;;;;;;End Runtime Code;;;;;;;

; Begin user globals.
; End user globals.

;; Global registers! ;;
; stack record register, linked list of stack records.
@srr = global %struct.SinRecord* null, align 8
; frame pointer register.
@fpr = global %struct.SinObj** null, align 8
; Return Value register
@retr = global %struct.SinObj* null, align 8
; Stack Protection Register
@spr = global %struct.SinObj** null, align 8
;; End global registers! ;;
;; supporting runtime functions ;;
define void @__underflow_handler() naked noreturn {
  %underflow_ret_addr = call void ()* @handle_underflow(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr)
call void %underflow_ret_addr()
  unreachable
}

define void @__continuation_function_handler() naked noreturn {
  ; Make the runtime do all the work!
  %contfnhandler_ret_addr = call void ()* @handle_continuation_function(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr, %struct.SinObj** @retr)
  call void %contfnhandler_ret_addr()
  unreachable
}

define void @__program_finished() naked noreturn {
  ; Dont worry about cleanup, just halt out of here.
  %finished_program_output = load %struct.SinObj*, %struct.SinObj** @retr, align 8
  call %struct.SinObj* @prim_halt(%struct.SinObj* %finished_program_output)
  unreachable
}

define i32 @main() {
  call void @start_runtime(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr)
  %base_fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  store %struct.SinObj* bitcast (void ()* @__program_finished to %struct.SinObj*), %struct.SinObj** %base_fp, align 8
  call void @__main()
  unreachable
}
; Start user program.
define ccc void @gotosub9823() naked noreturn alwaysinline {
  ; Subroutine for __main
  ; Begin function prologue.
  %prologue_fp9877 = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  %1 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 2
  %2 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 3
  %3 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 4
  %4 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 5
  %5 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 6
  %6 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 7
  %7 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 8
  %8 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 9
  %9 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 10
  %10 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 11
  %11 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 12
  %12 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 13
  %13 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 14
  %14 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 15
  %15 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 16
  %16 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 17
  %17 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 18
  %18 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 19
  %19 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 20
  %20 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 21
  %21 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 22
  %22 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 23
  %23 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 24
  %24 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 25
  %25 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 26
  %26 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 27
  %27 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 28
  %28 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 29
  %29 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 30
  %30 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 31
  %31 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 32
  %32 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 33
  %33 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 34
  %34 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 35
  %35 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 36
  %36 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 37
  %37 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 38
  %38 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 39
  %39 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 40
  %40 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 41
  %41 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 42
  %42 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 43
  %43 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 44
  %44 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 45
  %45 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 46
  %46 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 47
  %47 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 48
  %48 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 49
  %49 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 50
  %50 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 51
  %51 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 52
  %52 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 53
  %53 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 54
  %54 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 55
  %55 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 56
  %56 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9877, i64 57
  ; End function prologue.
  ; Laying out epilogue from the callee.
  call void @debug_output_registers(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr, %struct.SinObj** @retr, i64 0)
  %epilogue9878_callee_fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  %epilogue9878_slots_loc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %epilogue9878_callee_fp, i64 1
  %epilogue9878_slots_ptr = load %struct.SinObj*, %struct.SinObj** %epilogue9878_slots_loc, align 8
  %epilogue9878_slots = ptrtoint %struct.SinObj* %epilogue9878_slots_ptr to i64
  %epilogue9878_offset = mul i64 %epilogue9878_slots, -1
  %epilogue9878_caler_fp = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %epilogue9878_callee_fp, i64 %epilogue9878_offset
  store %struct.SinObj** %epilogue9878_caler_fp, %struct.SinObj*** @fpr, align 8
  call void @debug_output_registers(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr, %struct.SinObj** @retr, i64 1)
  ; End Epilogue.
  %sub_prologue9876_retr_val = load %struct.SinObj*, %struct.SinObj** @retr, align 8
  store %struct.SinObj* %sub_prologue9876_retr_val, %struct.SinObj** %51, align 8
  ; Starting apply-prim.
  %applyprim9884_arg = load %struct.SinObj*, %struct.SinObj** %51, align 8
  %applyprim9884_ret = call %struct.SinObj* @applyprim_vector_45ref(%struct.SinObj* %applyprim9884_arg)
  store %struct.SinObj* %applyprim9884_ret, %struct.SinObj** %52, align 8
  ; Starting prim.
  %prim9883_arg_0 = load %struct.SinObj*, %struct.SinObj** %37, align 8
  %prim9883_arg_1 = load %struct.SinObj*, %struct.SinObj** %52, align 8
  %prim9883_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9883_arg_0, %struct.SinObj* %prim9883_arg_1)
  store %struct.SinObj* %prim9883_ret, %struct.SinObj** %53, align 8
  ; Starting prim.
  %prim9882_arg_0 = load %struct.SinObj*, %struct.SinObj** %35, align 8
  %prim9882_arg_1 = load %struct.SinObj*, %struct.SinObj** %53, align 8
  %prim9882_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9882_arg_0, %struct.SinObj* %prim9882_arg_1)
  store %struct.SinObj* %prim9882_ret, %struct.SinObj** %54, align 8
  ; Starting prim.
  %prim9881_arg_0 = load %struct.SinObj*, %struct.SinObj** %25, align 8
  %prim9881_arg_1 = load %struct.SinObj*, %struct.SinObj** %54, align 8
  %prim9881_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9881_arg_0, %struct.SinObj* %prim9881_arg_1)
  store %struct.SinObj* %prim9881_ret, %struct.SinObj** %55, align 8
  ; Starting prim.
  %prim9880_arg_0 = load %struct.SinObj*, %struct.SinObj** %15, align 8
  %prim9880_arg_1 = load %struct.SinObj*, %struct.SinObj** %55, align 8
  %prim9880_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9880_arg_0, %struct.SinObj* %prim9880_arg_1)
  store %struct.SinObj* %prim9880_ret, %struct.SinObj** %56, align 8
  ; Starting tail return.
  %tail_return9879_ret = load %struct.SinObj*, %struct.SinObj** %56, align 8
  store %struct.SinObj* %tail_return9879_ret, %struct.SinObj** @retr, align 8
  %_fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  %_ra_sinobj = load %struct.SinObj*, %struct.SinObj** %_fp, align 8
  %_ra_sinfunc = bitcast %struct.SinObj* %_ra_sinobj to void ()*
  call void %_ra_sinfunc()
  unreachable
}

define ccc void @__main() naked noreturn prefix i64 56 {
  ; Begin function prologue.
  %prologue_fp9826 = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  %1 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 2
  %2 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 3
  %3 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 4
  %4 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 5
  %5 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 6
  %6 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 7
  %7 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 8
  %8 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 9
  %9 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 10
  %10 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 11
  %11 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 12
  %12 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 13
  %13 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 14
  %14 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 15
  %15 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 16
  %16 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 17
  %17 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 18
  %18 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 19
  %19 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 20
  %20 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 21
  %21 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 22
  %22 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 23
  %23 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 24
  %24 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 25
  %25 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 26
  %26 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 27
  %27 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 28
  %28 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 29
  %29 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 30
  %30 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 31
  %31 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 32
  %32 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 33
  %33 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 34
  %34 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 35
  %35 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 36
  %36 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 37
  %37 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 38
  %38 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 39
  %39 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 40
  %40 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 41
  %41 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 42
  %42 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 43
  %43 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 44
  %44 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 45
  %45 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 46
  %46 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 47
  %47 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 48
  %48 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 49
  %49 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 50
  %50 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 51
  %51 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 52
  %52 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 53
  %53 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 54
  %54 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 55
  %55 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 56
  %56 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9826, i64 57
  ; End function prologue.
  ; Starting const-int: 1.
  %ret9875 = call %struct.SinObj* @const_init_int(i64 1)
  store %struct.SinObj* %ret9875, %struct.SinObj** %3, align 8
  ; Starting const-int: 1.
  %ret9874 = call %struct.SinObj* @const_init_int(i64 1)
  store %struct.SinObj* %ret9874, %struct.SinObj** %4, align 8
  ; Starting prim.
  %prim9873_arg_0 = load %struct.SinObj*, %struct.SinObj** %3, align 8
  %prim9873_arg_1 = load %struct.SinObj*, %struct.SinObj** %4, align 8
  %prim9873_ret = call %struct.SinObj* @prim_make_45vector(%struct.SinObj* %prim9873_arg_0, %struct.SinObj* %prim9873_arg_1)
  store %struct.SinObj* %prim9873_ret, %struct.SinObj** %5, align 8
  ; Starting const-int: 0.
  %ret9872 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9872, %struct.SinObj** %6, align 8
  ; Starting const-int: 0.
  %ret9871 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9871, %struct.SinObj** %7, align 8
  ; Starting prim.
  %prim9870_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9870_arg_1 = load %struct.SinObj*, %struct.SinObj** %7, align 8
  %prim9870_ret = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* %prim9870_arg_0, %struct.SinObj* %prim9870_arg_1)
  store %struct.SinObj* %prim9870_ret, %struct.SinObj** %8, align 8
  ; Starting const-int: 3.
  %ret9869 = call %struct.SinObj* @const_init_int(i64 3)
  store %struct.SinObj* %ret9869, %struct.SinObj** %9, align 8
  ; Starting prim.
  %prim9868_arg_0 = load %struct.SinObj*, %struct.SinObj** %8, align 8
  %prim9868_arg_1 = load %struct.SinObj*, %struct.SinObj** %9, align 8
  %prim9868_ret = call %struct.SinObj* @prim__42(%struct.SinObj* %prim9868_arg_0, %struct.SinObj* %prim9868_arg_1)
  store %struct.SinObj* %prim9868_ret, %struct.SinObj** %10, align 8
  ; Starting const-int: 1.
  %ret9867 = call %struct.SinObj* @const_init_int(i64 1)
  store %struct.SinObj* %ret9867, %struct.SinObj** %11, align 8
  ; Starting prim.
  %prim9866_arg_0 = load %struct.SinObj*, %struct.SinObj** %10, align 8
  %prim9866_arg_1 = load %struct.SinObj*, %struct.SinObj** %11, align 8
  %prim9866_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9866_arg_0, %struct.SinObj* %prim9866_arg_1)
  store %struct.SinObj* %prim9866_ret, %struct.SinObj** %12, align 8
  ; Starting prim.
  %prim9865_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9865_arg_1 = load %struct.SinObj*, %struct.SinObj** %6, align 8
  %prim9865_arg_2 = load %struct.SinObj*, %struct.SinObj** %12, align 8
  %prim9865_ret = call %struct.SinObj* @prim_vector_45set_33(%struct.SinObj* %prim9865_arg_0, %struct.SinObj* %prim9865_arg_1, %struct.SinObj* %prim9865_arg_2)
  store %struct.SinObj* %prim9865_ret, %struct.SinObj** %13, align 8
  ; Starting const-int: 0.
  %ret9864 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9864, %struct.SinObj** %14, align 8
  ; Starting prim.
  %prim9863_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9863_arg_1 = load %struct.SinObj*, %struct.SinObj** %14, align 8
  %prim9863_ret = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* %prim9863_arg_0, %struct.SinObj* %prim9863_arg_1)
  store %struct.SinObj* %prim9863_ret, %struct.SinObj** %15, align 8
  ; Starting const-int: 0.
  %ret9862 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9862, %struct.SinObj** %16, align 8
  ; Starting const-int: 0.
  %ret9861 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9861, %struct.SinObj** %17, align 8
  ; Starting prim.
  %prim9860_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9860_arg_1 = load %struct.SinObj*, %struct.SinObj** %17, align 8
  %prim9860_ret = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* %prim9860_arg_0, %struct.SinObj* %prim9860_arg_1)
  store %struct.SinObj* %prim9860_ret, %struct.SinObj** %18, align 8
  ; Starting const-int: 5.
  %ret9859 = call %struct.SinObj* @const_init_int(i64 5)
  store %struct.SinObj* %ret9859, %struct.SinObj** %19, align 8
  ; Starting prim.
  %prim9858_arg_0 = load %struct.SinObj*, %struct.SinObj** %18, align 8
  %prim9858_arg_1 = load %struct.SinObj*, %struct.SinObj** %19, align 8
  %prim9858_ret = call %struct.SinObj* @prim__42(%struct.SinObj* %prim9858_arg_0, %struct.SinObj* %prim9858_arg_1)
  store %struct.SinObj* %prim9858_ret, %struct.SinObj** %20, align 8
  ; Starting const-int: 1.
  %ret9857 = call %struct.SinObj* @const_init_int(i64 1)
  store %struct.SinObj* %ret9857, %struct.SinObj** %21, align 8
  ; Starting prim.
  %prim9856_arg_0 = load %struct.SinObj*, %struct.SinObj** %20, align 8
  %prim9856_arg_1 = load %struct.SinObj*, %struct.SinObj** %21, align 8
  %prim9856_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9856_arg_0, %struct.SinObj* %prim9856_arg_1)
  store %struct.SinObj* %prim9856_ret, %struct.SinObj** %22, align 8
  ; Starting prim.
  %prim9855_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9855_arg_1 = load %struct.SinObj*, %struct.SinObj** %16, align 8
  %prim9855_arg_2 = load %struct.SinObj*, %struct.SinObj** %22, align 8
  %prim9855_ret = call %struct.SinObj* @prim_vector_45set_33(%struct.SinObj* %prim9855_arg_0, %struct.SinObj* %prim9855_arg_1, %struct.SinObj* %prim9855_arg_2)
  store %struct.SinObj* %prim9855_ret, %struct.SinObj** %23, align 8
  ; Starting const-int: 0.
  %ret9854 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9854, %struct.SinObj** %24, align 8
  ; Starting prim.
  %prim9853_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9853_arg_1 = load %struct.SinObj*, %struct.SinObj** %24, align 8
  %prim9853_ret = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* %prim9853_arg_0, %struct.SinObj* %prim9853_arg_1)
  store %struct.SinObj* %prim9853_ret, %struct.SinObj** %25, align 8
  ; Starting const-int: 0.
  %ret9852 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9852, %struct.SinObj** %26, align 8
  ; Starting const-int: 0.
  %ret9851 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9851, %struct.SinObj** %27, align 8
  ; Starting prim.
  %prim9850_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9850_arg_1 = load %struct.SinObj*, %struct.SinObj** %27, align 8
  %prim9850_ret = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* %prim9850_arg_0, %struct.SinObj* %prim9850_arg_1)
  store %struct.SinObj* %prim9850_ret, %struct.SinObj** %28, align 8
  ; Starting const-int: 2.
  %ret9849 = call %struct.SinObj* @const_init_int(i64 2)
  store %struct.SinObj* %ret9849, %struct.SinObj** %29, align 8
  ; Starting prim.
  %prim9848_arg_0 = load %struct.SinObj*, %struct.SinObj** %28, align 8
  %prim9848_arg_1 = load %struct.SinObj*, %struct.SinObj** %29, align 8
  %prim9848_ret = call %struct.SinObj* @prim__42(%struct.SinObj* %prim9848_arg_0, %struct.SinObj* %prim9848_arg_1)
  store %struct.SinObj* %prim9848_ret, %struct.SinObj** %30, align 8
  ; Starting const-int: 77.
  %ret9847 = call %struct.SinObj* @const_init_int(i64 77)
  store %struct.SinObj* %ret9847, %struct.SinObj** %31, align 8
  ; Starting prim.
  %prim9846_arg_0 = load %struct.SinObj*, %struct.SinObj** %30, align 8
  %prim9846_arg_1 = load %struct.SinObj*, %struct.SinObj** %31, align 8
  %prim9846_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9846_arg_0, %struct.SinObj* %prim9846_arg_1)
  store %struct.SinObj* %prim9846_ret, %struct.SinObj** %32, align 8
  ; Starting prim.
  %prim9845_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9845_arg_1 = load %struct.SinObj*, %struct.SinObj** %26, align 8
  %prim9845_arg_2 = load %struct.SinObj*, %struct.SinObj** %32, align 8
  %prim9845_ret = call %struct.SinObj* @prim_vector_45set_33(%struct.SinObj* %prim9845_arg_0, %struct.SinObj* %prim9845_arg_1, %struct.SinObj* %prim9845_arg_2)
  store %struct.SinObj* %prim9845_ret, %struct.SinObj** %33, align 8
  ; Starting const-int: 4.
  %ret9844 = call %struct.SinObj* @const_init_int(i64 4)
  store %struct.SinObj* %ret9844, %struct.SinObj** %34, align 8
  ; Starting alias.
  %alias9843 = load %struct.SinObj*, %struct.SinObj** %34, align 8
  store %struct.SinObj* %alias9843, %struct.SinObj** %35, align 8
  ; Starting const-int: 0.
  %ret9842 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9842, %struct.SinObj** %36, align 8
  ; Starting prim.
  %prim9841_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9841_arg_1 = load %struct.SinObj*, %struct.SinObj** %36, align 8
  %prim9841_ret = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* %prim9841_arg_0, %struct.SinObj* %prim9841_arg_1)
  store %struct.SinObj* %prim9841_ret, %struct.SinObj** %37, align 8
  ; Starting const-int: 0.
  %ret9840 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9840, %struct.SinObj** %38, align 8
  ; Starting const-int: 0.
  %ret9839 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9839, %struct.SinObj** %39, align 8
  ; Starting prim.
  %prim9838_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9838_arg_1 = load %struct.SinObj*, %struct.SinObj** %39, align 8
  %prim9838_ret = call %struct.SinObj* @prim_vector_45ref(%struct.SinObj* %prim9838_arg_0, %struct.SinObj* %prim9838_arg_1)
  store %struct.SinObj* %prim9838_ret, %struct.SinObj** %40, align 8
  ; Starting const-int: 7.
  %ret9837 = call %struct.SinObj* @const_init_int(i64 7)
  store %struct.SinObj* %ret9837, %struct.SinObj** %41, align 8
  ; Starting prim.
  %prim9836_arg_0 = load %struct.SinObj*, %struct.SinObj** %40, align 8
  %prim9836_arg_1 = load %struct.SinObj*, %struct.SinObj** %41, align 8
  %prim9836_ret = call %struct.SinObj* @prim__42(%struct.SinObj* %prim9836_arg_0, %struct.SinObj* %prim9836_arg_1)
  store %struct.SinObj* %prim9836_ret, %struct.SinObj** %42, align 8
  ; Starting const-int: 2.
  %ret9835 = call %struct.SinObj* @const_init_int(i64 2)
  store %struct.SinObj* %ret9835, %struct.SinObj** %43, align 8
  ; Starting prim.
  %prim9834_arg_0 = load %struct.SinObj*, %struct.SinObj** %42, align 8
  %prim9834_arg_1 = load %struct.SinObj*, %struct.SinObj** %43, align 8
  %prim9834_ret = call %struct.SinObj* @prim__43(%struct.SinObj* %prim9834_arg_0, %struct.SinObj* %prim9834_arg_1)
  store %struct.SinObj* %prim9834_ret, %struct.SinObj** %44, align 8
  ; Starting prim.
  %prim9833_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9833_arg_1 = load %struct.SinObj*, %struct.SinObj** %38, align 8
  %prim9833_arg_2 = load %struct.SinObj*, %struct.SinObj** %44, align 8
  %prim9833_ret = call %struct.SinObj* @prim_vector_45set_33(%struct.SinObj* %prim9833_arg_0, %struct.SinObj* %prim9833_arg_1, %struct.SinObj* %prim9833_arg_2)
  store %struct.SinObj* %prim9833_ret, %struct.SinObj** %45, align 8
  ; Starting make-closure.
  %makeclo9832_ret = call %struct.SinObj* @closure_alloc(i64 0, void ()* @clo5043974)
  store %struct.SinObj* %makeclo9832_ret, %struct.SinObj** %46, align 8
  ; Starting const-int: 0.
  %ret9831 = call %struct.SinObj* @const_init_int(i64 0)
  store %struct.SinObj* %ret9831, %struct.SinObj** %47, align 8
  ; Starting const-null.
  %ret9830 = call %struct.SinObj* @const_init_null()
  store %struct.SinObj* %ret9830, %struct.SinObj** %48, align 8
  ; Starting prim.
  %prim9829_arg_0 = load %struct.SinObj*, %struct.SinObj** %47, align 8
  %prim9829_arg_1 = load %struct.SinObj*, %struct.SinObj** %48, align 8
  %prim9829_ret = call %struct.SinObj* @prim_cons(%struct.SinObj* %prim9829_arg_0, %struct.SinObj* %prim9829_arg_1)
  store %struct.SinObj* %prim9829_ret, %struct.SinObj** %49, align 8
  ; Starting prim.
  %prim9828_arg_0 = load %struct.SinObj*, %struct.SinObj** %5, align 8
  %prim9828_arg_1 = load %struct.SinObj*, %struct.SinObj** %49, align 8
  %prim9828_ret = call %struct.SinObj* @prim_cons(%struct.SinObj* %prim9828_arg_0, %struct.SinObj* %prim9828_arg_1)
  store %struct.SinObj* %prim9828_ret, %struct.SinObj** %50, align 8
  ; Starting direct clo-app.
  ; Beginning overflow check.
  %direct_cloapp9827_overflowcheck_prefixdata_proc_ptr = bitcast void ()* @__main to i64*
  %direct_cloapp9827_overflowcheck_prefixdata_nslots_loc = getelementptr inbounds i64, i64* %direct_cloapp9827_overflowcheck_prefixdata_proc_ptr, i64 -1
  %direct_cloapp9827_overflowcheck_nslots = load i64, i64* %direct_cloapp9827_overflowcheck_prefixdata_nslots_loc, align 8
  %direct_cloapp9827_overflowcheck_has_space = call i1 @check_for_overflow(%struct.SinObj*** @fpr, %struct.SinObj*** @spr, i64 %direct_cloapp9827_overflowcheck_nslots)
  br i1 %direct_cloapp9827_overflowcheck_has_space, label %direct_cloapp9827_overflowcheck_does_have_space, label %direct_cloapp9827_overflowcheck_overflow_first
direct_cloapp9827_overflowcheck_overflow_first:
  call void @handle_overflow(%struct.SinRecord** @srr, %struct.SinObj*** @fpr, %struct.SinObj*** @spr, void ()* @__underflow_handler, i64 %direct_cloapp9827_overflowcheck_nslots)
  br label %direct_cloapp9827_overflowcheck_does_have_space
direct_cloapp9827_overflowcheck_does_have_space:
  ; End overflow check.
  %direct_cloapp9827_clo = load %struct.SinObj*, %struct.SinObj** %46, align 8
  %direct_cloapp9827_app_args = load %struct.SinObj*, %struct.SinObj** %50, align 8
  %direct_cloapp9827_prefixdata_proc_ptr = bitcast void ()* @__main to i64*
  %direct_cloapp9827_prefixdata_nslots_loc = getelementptr inbounds i64, i64* %direct_cloapp9827_prefixdata_proc_ptr, i64 -1
  %direct_cloapp9827_nslots = load i64, i64* %direct_cloapp9827_prefixdata_nslots_loc, align 8
  %direct_cloapp9827_nslots_sinobj = inttoptr i64 %direct_cloapp9827_nslots to %struct.SinObj*
  %direct_cloapp9827_caller_fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  %direct_cloapp9827_callee_fp = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %direct_cloapp9827_caller_fp, i64 %direct_cloapp9827_nslots
  store %struct.SinObj** %direct_cloapp9827_callee_fp, %struct.SinObj*** @fpr, align 8
  %direct_cloapp9827_return_addr = bitcast void ()* @gotosub9823 to %struct.SinObj*
  %direct_cloapp9827_size_loc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %direct_cloapp9827_callee_fp, i64 1
  %direct_cloapp9827_arg0_loc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %direct_cloapp9827_callee_fp, i64 2
  %direct_cloapp9827_arg1_loc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %direct_cloapp9827_callee_fp, i64 3
  store %struct.SinObj* %direct_cloapp9827_return_addr, %struct.SinObj** %direct_cloapp9827_callee_fp, align 8
  store %struct.SinObj* %direct_cloapp9827_nslots_sinobj, %struct.SinObj** %direct_cloapp9827_size_loc, align 8
  store %struct.SinObj* %direct_cloapp9827_clo, %struct.SinObj** %direct_cloapp9827_arg0_loc, align 8
  store %struct.SinObj* %direct_cloapp9827_app_args, %struct.SinObj** %direct_cloapp9827_arg1_loc, align 8
  %direct_cloapp9827_funcptr = call void ()* @closure_get_fn_part(%struct.SinObj* %direct_cloapp9827_clo)
  call void %direct_cloapp9827_funcptr()
  unreachable
}

define ccc void @clo5043974() naked noreturn prefix i64 2 {
  ; Begin function prologue.
  %prologue_fp9824 = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  %1 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9824, i64 2
  %2 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %prologue_fp9824, i64 3
  ; End function prologue.
  ; Starting tail return.
  %tail_return9825_ret = load %struct.SinObj*, %struct.SinObj** %2, align 8
  store %struct.SinObj* %tail_return9825_ret, %struct.SinObj** @retr, align 8
  %_fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
  %_ra_sinobj = load %struct.SinObj*, %struct.SinObj** %_fp, align 8
  %_ra_sinfunc = bitcast %struct.SinObj* %_ra_sinobj to void ()*
  call void %_ra_sinfunc()
  unreachable
}

; End user program.
