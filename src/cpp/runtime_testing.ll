;
; This is me playing around in LLVM IR trying
; to get the ABI right.

; a stack record is an object with 5 fields:
; 1. The pointer to the call-stack object
; 2. The pointer to the next stack record (this struct is a linked list)
; 3. The size of the stack object
; 4. The offset (in #slots from base) to the FP of the top frame.
; 5. The return address to code of that frame.

;; Global registers ;;

; stack record register, linked list of stack records.
@srr = global %struct.SinFrame* null, align 8
; frame pointer register.
@fpr = global %struct.SinObj** null, align 8
; Return Value register
@retr = global %struct.SinObj* null, align 8
; Stack Protection Register
@spr = global @struct.SinObj* null, align 8

; LLVM IR for the morestack function, to be inserted by the compiler.
; TODO: what we should do is move a few frames from the old stack into
; the new one to avoid thrashing between stacks
; (continuously returning and then having to make a new one)
define void @morestack() {
    ; get current global register values
    %cur_stack_record = load %struct.SinFrame*, %struct.SinFrame** @srr
    %cur_frame = load %struct.SinObj**, %struct.SinObj*** @fpr
    ; create a new stack record (node in the linked list)
    %new_stack_record = call %struct.SinFrame* @make_frame(%struct.SinFrame* %cur_stack_record. %struct.SinObj** %cur_frame)
    ; get the address of the srr part of a SinFrame
    %stack_addr = getelementptr inbounds %struct.SinFrame, %struct.SinFrame* %new_stack_record, i64 0, i64 0
    ; deref it to get the address of the start of the stack (the new fp)
    %new_fp = load %struct.SinObj**, %struct.SinObj*** %stack_addr, align 8
    ; set the created node as the head of the global @srr linked list.
    ; and the new fp to the global fp register.
    store %struct.SinFrame* %new_stack_record, %struct.SinFrame** @srr
    store %struct.SinObj** %new_fp, %struct.SinObj*** @fpr
}

define void @splitstack() {
    ; do size math.
    ; n2 = size of old stack record
    ; n3 = size of new stack record
    ; %csr = *@srr
    ; %csr->ret_addr = @fpr[0]
    ; %csr->size = n2
    ; %nsr = ...
    ; %nsr->next = %csr
    ; %nsr->size = n3
    ; *@srr = %nsr
    ; %handler = i8* blockaddress(@main, %underflow)
    ; @fpr[0] = %handler


    ; get the current stack record
    ; fill its last slot `ret_addr` with @fpr[0]
    ; calculate new sizes of stack records
    ; set new size of current stack record
    ; make a new stack record
    ; new_stack_record[0] = @fpr
    ; new_stack_record[1] = *@srr
    ; *@srr = new_stack_record
}


define i32 main() {
    call void @morestack() ; sets up stack
    call void @start_program() ; starts GC / runtime
    ; arguments are left uninitialized, as main doesnt have any.
    %curfp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
    %raloc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %curfp, i64 0
    %ra = i8* blockaddress(@main, %program_finished)
    %racast = bitcast i8* %ra to %struct.SinObj*
    store %struct.SinObj* %racast, %struct.SinObj** %raloc
    br label %main
main:
    ; TODO: can %main_curfp be used throughout the whole of main? Even after returning from other funcs?
    %main_curfp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
    ; Get local slots
    %main_loc0_loc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %main_curfp, i64 3
    %main_loc1_loc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %main_curfp, i64 4
    %main_loc2_loc = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %main_curfp, i64 5

    %main_loc0_val = call %struct.SinObj* @const_init_int(7)
    store %struct.SinObj* %main_loc0_val, %struct.SinObj** %main_loc0_loc, align 8
    %main_loc1_val = call %struct.SinObj* @const_init_null()
    store %struct.SinObj* %main_loc1_val, %struct.SinObj** %main_loc1_loc, align 8

    %main_call0_arglistnil = call %struct.SinObj* @const_init_null()
    %main_call0_arglist = call @struct.SinObj* @prim_cons(%struct.SinObj* %main_loc0_loc, %struct.SinObj* %main_call0_arglist0)
    %main_call0_funcloc = i8* blockaddress(@main, %f)
    %main_call0_clo = call %struct.SinObj* @closure_alloc(i64 1, i8* %funcloc)
    %main_temp0 = load %struct.SinObj*, %struct.SinObj** %main_loc1_loc, align 8
    call void @closure_place_freevar(%struct.SinObj* %clo, %struct.SinObj* %main_temp0, i64 0)

    %main_call0_fnpart = call i8* closure_get_fn_part(%struct.SinObj* %main_call0_clo)
    %main_call0_ra = i8* blockaddress(@main, %main_call0_ret)
    %main_call0_racast = bitcast i8* %main_call0_ra to %struct.SinObj*
    %main_call0_temp_fpr = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
    ; TODO: check that theres enough stack space,
    ;       call morestack() if not.
    ; 6 based on main call-frame size
    %main_call0_new_fpr = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %main_call0_temp_fpr, i64 6
    %main_call0_new_arg0 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %main_call0_temp_fpr, i64 7
    %main_call0_new_arg1 = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %main_call0_temp_fpr, i64 8
    store %struct.SinObj** %main_call0_new_fpr, %struct.SinObj*** @fpr, align 8
    ; setup call-frame
    store %struct.SinObj* %main_call0_racast, %struct.SinObj** %main_call0_new_fpr, align 8
    store %struct.SinObj* %main_call0_clo, %struct.SinObj** %main_call0_new_arg0, align 8
    store %struct.SinObj* %main_call0_arglist, %struct.SinObj** %main_call0_new_arg1, align 8
    ; call the function.
    indirectbr i8* %main_call0_funcloc, [TODO: ALL FUNCTIONS!?]
main_call0_ret:
    ; ...
    %main_call1_oldfp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
    ; move @fpr back to `main` call-frame.
    %main_call1_curfp = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %main_call1_oldfp, i64 -6
    store %struct.SinObj** %main_call1_curfp, %struct.SinObj*** @fpr, align 8
    %main_call1_retval = load %struct.SinObj*, %struct.SinObj** @retr, align 8
    store %struct.SinObj* %main_call1_retval, %struct.SinObj** %main_loc2_loc, align 8

    %main_call1_ra = load %struct.SinObj*, %struct.SinObj** %main_call1_curfp, align 8
    indirectbr %main_call1_ra, [TODO: ALL RETURN ADDRESS SET (includes %program_finished)]
f:
    %f_curfp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
    ; ...
    ; this next line isnt representative of real output,
    ; just wanted to get the return ABI stuff correct.
    %f_call0_retval = call %struct.SinObj* @const_init_int(12)
    %f_call0_ra = load %struct.SinObj*, %struct.SinObj** %f_curfp, align 8
    store %struct.SinObj* %f_call0_retval, %struct.SinObj** @retr, align 8
    indirectbr %f_call0_ra, [TODO: HOW DO WE COMPUTE THIS SET? SET OF ALL RETURN ADDRS???? MUST INCLUDE %program_finished]
program_finished:
    ; dont worry about cleaning up main, just exit the program
    ret i32 0
underflow_handler:
    ...
continuation_function_handler:
    ;; Put arguments in registers
    %fp = load %struct.SinObj**, %struct.SinObj*** @fpr, align 8
    %clo_arg_loc = getelementptr %struct.SinObj*, %struct.SinObj** %fp, i64 arg0-pos
    %ret_arglist_loc = getelementptr %struct.SinObj*, %struct.SinObj** %fp, i64 arg1-pos
    %clo_arg = load %struct.SinObj*, %struct.SinObj** %clo_arg_loc, align 8
    %ret_arglist = load %struct.SinObj*, %struct.SinObj** %ret_arglist_loc, align 8
    %ret_arg = call @struct.SinObj* @prim_car(%ret_arglist)
    ;; Get the continuation parts
    ; (called get_env_part, but this is a continuation, not a closure!)
    %cont_sr_ptr = call @closure_get_env_part(%arg0)
    %cont_sr = bitcast i8* %cont_sr_ptr to %struct.SinRecord*
    %cont_stack_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cont_sr, i64 0, i32 0
    %cont_next_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cont_sr, i64 0, i32 1
    %cont_size_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cont_sr, i64 0, i32 2
    %cont_offset_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cont_sr, i64 0, i32 3

    %cont_stack = load %struct.SinObj**, %struct.SinObj*** %cont_stack_loc, align 8
    %cont_next = load %struct.SinRecord*, %struct.SinRecord** %cont_next_loc, align 8
    %cont_size = load i64, i64* %cont_size_loc, align 8
    %cont_offset = load i64, i64* %cont_offset_loc, align 8

    ;; get current stack segment
    %cur_sr = load %struct.SinRecord*, %struct.SinRecord** @srr
    %cur_stack_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cur_sr, i64 0, i32 0
    %cur_next_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cur_sr, i64 0, i32 1
    %cur_size_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cur_sr, i64 0, i32 2
    %cur_ra_loc = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %cur_sr, i64 0, i32 4

    %cur_stack = load %struct.SinObj**, %struct.SinObj*** %cur_stack_loc, align 8
    %ret_addr = load i8*, i8** %cur_ra_loc, align 8

    ;;; copy the stack onto the current stack
    store volatile %struct.SinRecord* %cont_next, %struct.SinRecord** %cur_next_loc, align 8
    call void @copy_stack(%struct.SinObj** %cont_stack, i64 %cont_size, %struct.SinObj*** %cur_stack)
    ; TODO: ensure that %cont_size < %cur_size

    ;;; Set @fpr using offset.
    %new_fp = getelementptr inbounds %struct.SinObj*, %struct.SinObj** %cur_stack, i64 %cont_offset
    store volatile %struct.SinObj** %new_fp, %struct.SinObj*** @fpr, align 8

    ;; add return value to @retr
    store volatile %struct.SinObj* %ret_arg, %struct.SinObj** @retr, align 8

    ; go into the RA, get the return spot?
    indirectbr i8* %retaddr [TODO: ALL RETURN ADDRS]
}

