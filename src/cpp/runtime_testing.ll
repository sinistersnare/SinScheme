; This is me playing around in LLVM IR trying
; to get the ABI right.


; LLVM IR for the morestack, to be inserted by the compiler.
; after this function is called, @fpr is invalid, it does not point to a
; return address of a stack frame. It must be updated to do so.
; TODO: what we should do is move a few frames from the old stack into
; the new one to avoid thrashing between stacks
; (continuously returning and then having to make a new one)
define void morestack() {
    ; get current global register values
    %cur_stack_record = load %struct.SinFrame*, %struct.SinFrame** @srr
    %cur_frame = load %struct.SinObj**, %struct.SinObj*** @fpr
    ; create a new stack record (node in the linked list)
    %new_stack_record = call %struct.SinFrame* @make_frame(%struct.SinFrame* %cur_stack_record. i8* %cur_frame)
    ; get the address of the srr part of a SinFrame
    %stack_addr = getelementptr inbounds %struct.SinFrame, %struct.SinFrame* %new_stack_record, i32 0, i32 0
    ; deref it to get the address of the start of the stack (the new fp)
    %new_fp = load s64*, s64** %stack_addr, align 8
    ; set the created node as the head of the global @srr linked list.
    ; and the new fp to the global fp register.
    store %struct.SinFrame* %new_stack_record, %struct.SinFrame** @srr
    store s64* %new_fp, s64** @fpr
}

; a stack record is an object with 4 fields:
; 1. The pointer to the call-stack object
; 2. The pointer to the next stack record (this struct is a linked list)
; 3. The size of the stack object
; 4. The return address of the stack record

; stack record register, linked list of stack records.
@srr = global %struct.SinFrame* null, align 8
; frame pointer register.
@fpr = global %struct.SinObj** null, align 8

define i32 main() {
    call void @morestack() ; sets up stack
    call void @start_program() ; starts GC / runtime
    ; arguments are left uninitialized, as main doesnt have any.
    %curfp = load i64*, i64** @fpr, align 8
    %raloc = getelementptr inbounds i64, i64* %curfp, i64 0
    %ra = i8* blockaddress(@main, %program_finished)
    %raint = ptrtoint i64* %ra to i64
    store i64 %retaddrint, i64* %raloc
    br label %main
main:
    %main_curfp = load i64*, i64** @fpr, align 8
    %main_loc0_loc = getelementptr inbounds i64, i64* %main_curfp, i64 3
    %main_loc1_loc = getelementptr inbounds i64, i64* %main_curfp, i64 4

    %main_loc0_val = call %struct.SinObj* @const_init_int(7)
    %main_loc1_val = call %struct.SinObj* @const_init_null()
    store i64

    %main_call0_arglistnil = call %struct.SinObj* @const_init_null()
    %main_call0_arglist0 = call @struct.SinObj* @prim_cons(%struct.SinObj* %main_loc1, %struct.SinObj* %main_call0_arglistnil)
    %main_call0_arglist = call @struct.SinObj* @prim_cons(%struct.SinObj* %main_loc0, %struct.SinObj* %main_call0_arglist0)
    %main_call0_funcloc = i8* blockaddress(@main, %f)
    %main_call0_clo = call %struct.SinObj* @closure_alloc(i64, 2, i8* %funcloc)
    %main_call0
    %arg0 = call %struct.SinObj* @const_init_int(7)
    %arg1 = call %struct.SinObj* @const_init_null()
    %arglistnil = call %struct.SinObj* @const_init_null()
    %arglist1 = call %struct.SinObj* @prim_cons(arg1, arglistnil)
    %arglist = call %struct.SinObj* @prim_cons(arg0, arglist1)
    %g = call %struct.SinObj* @const_init_bool(false)
    %h = call %struct.SinObj* @const_init_void()
    ; TODO: need to have closure_alloc take void* and not u64.
    %funcloc = i8* blockaddress(@main, %f)
    %clo = call %struct.SinObj* @closure_alloc(i64 2, i8* %funcloc)
    call void @closure_place_freevar(%struct.SinObj* %clo, %struct.SinObj* %g, 0)
    call void @closure_place_freevar(%struct.SinObj* %clo, %struct.SinObj* %h, 1)

    %fnpart = call i8* closure_get_fn_part(%struct.SinObj* %clo)
    ; check that theres enough stack space,
    ;       call morestack() if not.
    ; * put return address `blockaddress(@main, %mainret)` on stack.
    ; * Put env on stack
    ; * put arglist on stack
    ; * `indirectbr` to %fnpart
    %retaddr = i8* blockaddress(@main, %mainret)
    indirectbr %fnpart [ALL_FUNCS_LOL]
program_finished:
    ; dont worry about cleaning up main, just exit the program
    ret i32 0
f:
}


  %curfp = load i64*, i64** @fpr, align 8
  %sizeloc
  %raloc = getelementptr inbounds i64, i64* %curfp, i64 0
  %7 = getelementptr inbounds i64, i64* %6, i64 10
  store i64* %7, i64** %3, align 8
  %8 = load i64*, i64** %3, align 8
  store i64 5, i64* %8, align 8

