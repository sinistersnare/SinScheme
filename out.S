	.text
	.file	"runtime.cpp"
	.globl	insert_update                   # -- Begin function insert_update
	.p2align	4, 0x90
	.type	insert_update,@function
insert_update:                          # @insert_update
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	cmpq	$0, -16(%rbp)
	jne	.LBB0_2
# %bb.1:
	movl	$24, %edi
	callq	malloc@PLT
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	$0, (%rax)
	movq	-24(%rbp), %rcx
	movq	-40(%rbp), %rax
	movq	%rcx, 8(%rax)
	movq	-32(%rbp), %rcx
	movq	-40(%rbp), %rax
	movq	%rcx, 16(%rax)
	movq	-40(%rbp), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB0_6
.LBB0_2:
	movl	$24, %edi
	callq	malloc@PLT
	movq	%rax, -48(%rbp)
	movq	-16(%rbp), %rax
	movq	8(%rax), %rdi
	movq	-24(%rbp), %rsi
	callq	eq_helper
	cmpl	$1, %eax
	jne	.LBB0_4
# %bb.3:
	movq	-24(%rbp), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, 8(%rax)
	movq	-32(%rbp), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, 16(%rax)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdi
	callq	insert_copy
	movq	%rax, %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, (%rax)
	jmp	.LBB0_5
.LBB0_4:
	movq	-16(%rbp), %rax
	movq	8(%rax), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, 8(%rax)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, 16(%rax)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdi
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdx
	callq	insert_update
	movq	%rax, %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, (%rax)
.LBB0_5:
	movq	-48(%rbp), %rax
	movq	%rax, -8(%rbp)
.LBB0_6:
	movq	-8(%rbp), %rax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	insert_update, .Lfunc_end0-insert_update
	.cfi_endproc
                                        # -- End function
	.globl	eq_helper                       # -- Begin function eq_helper
	.p2align	4, 0x90
	.type	eq_helper,@function
eq_helper:                              # @eq_helper
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	-16(%rbp), %rax
	movl	8(%rax), %eax
	movq	-24(%rbp), %rcx
	cmpl	8(%rcx), %eax
	je	.LBB1_2
# %bb.1:
	movl	$0, -4(%rbp)
	jmp	.LBB1_25
.LBB1_2:
	movq	-16(%rbp), %rax
	movl	8(%rax), %eax
                                        # kill: def $rax killed $eax
	movq	%rax, -64(%rbp)                 # 8-byte Spill
	subq	$12, %rax
	ja	.LBB1_24
# %bb.26:
	movq	-64(%rbp), %rax                 # 8-byte Reload
	leaq	.LJTI1_0(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmpq	*%rax
.LBB1_3:
	movl	$1, -4(%rbp)
	jmp	.LBB1_25
.LBB1_4:
	movl	$1, -4(%rbp)
	jmp	.LBB1_25
.LBB1_5:
	movq	-16(%rbp), %rdi
	leaq	.L.str.101(%rip), %rsi
	callq	unwrap_bool
	movq	%rax, -72(%rbp)                 # 8-byte Spill
	movq	-24(%rbp), %rdi
	leaq	.L.str.102(%rip), %rsi
	callq	unwrap_bool
	movq	%rax, %rcx
	movq	-72(%rbp), %rax                 # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB1_7
# %bb.6:
	movl	$1, -4(%rbp)
	jmp	.LBB1_25
.LBB1_7:
	movl	$0, -4(%rbp)
	jmp	.LBB1_25
.LBB1_8:
	movl	$0, -4(%rbp)
	jmp	.LBB1_25
.LBB1_9:
	movl	$0, -4(%rbp)
	jmp	.LBB1_25
.LBB1_10:
	movq	-16(%rbp), %rdi
	movq	-24(%rbp), %rsi
	callq	cons_eq_helper
	movl	%eax, -4(%rbp)
	jmp	.LBB1_25
.LBB1_11:
	movq	-16(%rbp), %rdi
	leaq	.L.str.103(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -80(%rbp)                 # 8-byte Spill
	movq	-24(%rbp), %rdi
	leaq	.L.str.104(%rip), %rsi
	callq	unwrap_int
	movq	%rax, %rcx
	movq	-80(%rbp), %rax                 # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB1_13
# %bb.12:
	movl	$1, -4(%rbp)
	jmp	.LBB1_25
.LBB1_13:
	movl	$0, -4(%rbp)
	jmp	.LBB1_25
.LBB1_14:
	movq	-16(%rbp), %rdi
	leaq	.L.str.105(%rip), %rsi
	callq	unwrap_str
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	leaq	.L.str.106(%rip), %rsi
	callq	unwrap_str
	movq	%rax, -40(%rbp)
	movq	-32(%rbp), %rdi
	movq	-40(%rbp), %rsi
	callq	strcmp@PLT
	cmpl	$0, %eax
	jne	.LBB1_16
# %bb.15:
	movl	$1, -4(%rbp)
	jmp	.LBB1_25
.LBB1_16:
	movl	$0, -4(%rbp)
	jmp	.LBB1_25
.LBB1_17:
	movq	-16(%rbp), %rdi
	leaq	.L.str.107(%rip), %rsi
	callq	unwrap_sym
	movq	%rax, -48(%rbp)
	movq	-24(%rbp), %rdi
	leaq	.L.str.108(%rip), %rsi
	callq	unwrap_sym
	movq	%rax, -56(%rbp)
	movq	-48(%rbp), %rdi
	movq	-56(%rbp), %rsi
	callq	strcmp@PLT
	cmpl	$0, %eax
	jne	.LBB1_19
# %bb.18:
	movl	$1, -4(%rbp)
	jmp	.LBB1_25
.LBB1_19:
	movl	$0, -4(%rbp)
	jmp	.LBB1_25
.LBB1_20:
	movq	-16(%rbp), %rdi
	movq	-24(%rbp), %rsi
	callq	vec_eq_helper
	movl	%eax, -4(%rbp)
	jmp	.LBB1_25
.LBB1_21:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.109(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB1_22:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.110(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB1_23:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.111(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB1_24:
	ud2
.LBB1_25:
	movl	-4(%rbp), %eax
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end1:
	.size	eq_helper, .Lfunc_end1-eq_helper
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	2
.LJTI1_0:
	.long	.LBB1_3-.LJTI1_0
	.long	.LBB1_4-.LJTI1_0
	.long	.LBB1_5-.LJTI1_0
	.long	.LBB1_9-.LJTI1_0
	.long	.LBB1_10-.LJTI1_0
	.long	.LBB1_11-.LJTI1_0
	.long	.LBB1_14-.LJTI1_0
	.long	.LBB1_17-.LJTI1_0
	.long	.LBB1_20-.LJTI1_0
	.long	.LBB1_21-.LJTI1_0
	.long	.LBB1_22-.LJTI1_0
	.long	.LBB1_23-.LJTI1_0
	.long	.LBB1_8-.LJTI1_0
                                        # -- End function
	.text
	.globl	insert_copy                     # -- Begin function insert_copy
	.p2align	4, 0x90
	.type	insert_copy,@function
insert_copy:                            # @insert_copy
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jne	.LBB2_2
# %bb.1:
	movq	$0, -8(%rbp)
	jmp	.LBB2_3
.LBB2_2:
	movl	$24, %edi
	callq	malloc@PLT
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	8(%rax), %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, 8(%rax)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, 16(%rax)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdi
	callq	insert_copy
	movq	%rax, %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-24(%rbp), %rax
	movq	%rax, -8(%rbp)
.LBB2_3:
	movq	-8(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end2:
	.size	insert_copy, .Lfunc_end2-insert_copy
	.cfi_endproc
                                        # -- End function
	.globl	map_insert                      # -- Begin function map_insert
	.p2align	4, 0x90
	.type	map_insert,@function
map_insert:                             # @map_insert
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	movq	-24(%rbp), %rdx
	callq	insert_update
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end3:
	.size	map_insert, .Lfunc_end3-map_insert
	.cfi_endproc
                                        # -- End function
	.globl	map_keys                        # -- Begin function map_keys
	.p2align	4, 0x90
	.type	map_keys,@function
map_keys:                               # @map_keys
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	cmpq	$0, -16(%rbp)
	jne	.LBB4_2
# %bb.1:
	callq	const_init_null
	movq	%rax, -8(%rbp)
	jmp	.LBB4_3
.LBB4_2:
	movq	-16(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdi
	callq	map_keys
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %rsi
	callq	prim_cons
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -8(%rbp)
.LBB4_3:
	movq	-8(%rbp), %rax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end4:
	.size	map_keys, .Lfunc_end4-map_keys
	.cfi_endproc
                                        # -- End function
	.globl	const_init_null                 # -- Begin function const_init_null
	.p2align	4, 0x90
	.type	const_init_null,@function
const_init_null:                        # @const_init_null
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$1, %edi
	callq	alloc_atomic
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	$0, (%rax)
	movq	-8(%rbp), %rax
	movl	$1, 8(%rax)
	movq	-8(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end5:
	.size	const_init_null, .Lfunc_end5-const_init_null
	.cfi_endproc
                                        # -- End function
	.globl	prim_cons                       # -- Begin function prim_cons
	.p2align	4, 0x90
	.type	prim_cons,@function
prim_cons:                              # @prim_cons
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$2, %edi
	callq	alloc
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	(%rcx), %rdx
	movq	%rdx, (%rax)
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rax)
	movq	-16(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	8(%rcx), %rcx
	movq	%rcx, 24(%rax)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-32(%rbp), %rax
	movl	$4, 8(%rax)
	movq	-32(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end6:
	.size	prim_cons, .Lfunc_end6-prim_cons
	.cfi_endproc
                                        # -- End function
	.globl	map_get                         # -- Begin function map_get
	.p2align	4, 0x90
	.type	map_get,@function
map_get:                                # @map_get
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
.LBB7_1:                                # =>This Inner Loop Header: Depth=1
	cmpq	$0, -16(%rbp)
	je	.LBB7_5
# %bb.2:                                #   in Loop: Header=BB7_1 Depth=1
	movq	-24(%rbp), %rdi
	movq	-16(%rbp), %rax
	movq	8(%rax), %rsi
	callq	eq_helper
	cmpl	$1, %eax
	jne	.LBB7_4
# %bb.3:
	movq	-16(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB7_6
.LBB7_4:                                #   in Loop: Header=BB7_1 Depth=1
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -16(%rbp)
	jmp	.LBB7_1
.LBB7_5:
	movq	$0, -8(%rbp)
.LBB7_6:
	movq	-8(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end7:
	.size	map_get, .Lfunc_end7-map_get
	.cfi_endproc
                                        # -- End function
	.globl	map_has_key                     # -- Begin function map_has_key
	.p2align	4, 0x90
	.type	map_has_key,@function
map_has_key:                            # @map_has_key
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
.LBB8_1:                                # =>This Inner Loop Header: Depth=1
	cmpq	$0, -16(%rbp)
	je	.LBB8_5
# %bb.2:                                #   in Loop: Header=BB8_1 Depth=1
	movq	-16(%rbp), %rax
	movq	8(%rax), %rdi
	movq	-24(%rbp), %rsi
	callq	eq_helper
	cmpl	$1, %eax
	jne	.LBB8_4
# %bb.3:
	movb	$1, -1(%rbp)
	jmp	.LBB8_6
.LBB8_4:                                #   in Loop: Header=BB8_1 Depth=1
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -16(%rbp)
	jmp	.LBB8_1
.LBB8_5:
	movb	$0, -1(%rbp)
.LBB8_6:
	movb	-1(%rbp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end8:
	.size	map_has_key, .Lfunc_end8-map_has_key
	.cfi_endproc
                                        # -- End function
	.globl	map_count                       # -- Begin function map_count
	.p2align	4, 0x90
	.type	map_count,@function
map_count:                              # @map_count
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	$0, -16(%rbp)
.LBB9_1:                                # =>This Inner Loop Header: Depth=1
	cmpq	$0, -8(%rbp)
	je	.LBB9_3
# %bb.2:                                #   in Loop: Header=BB9_1 Depth=1
	movq	-16(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB9_1
.LBB9_3:
	movq	-16(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end9:
	.size	map_count, .Lfunc_end9-map_count
	.cfi_endproc
                                        # -- End function
	.globl	debug_output_registers          # -- Begin function debug_output_registers
	.p2align	4, 0x90
	.type	debug_output_registers,@function
debug_output_registers:                 # @debug_output_registers
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8, -40(%rbp)
	movq	-40(%rbp), %rsi
	leaq	.L.str(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.1(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movq	(%rax), %rsi
	movq	-16(%rbp), %rax
	movq	(%rax), %rdx
	movq	-24(%rbp), %rax
	movq	(%rax), %rcx
	movq	-32(%rbp), %rax
	movq	(%rax), %r8
	leaq	.L.str.2(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-32(%rbp), %rax
	cmpq	$0, (%rax)
	je	.LBB10_2
# %bb.1:
	leaq	.L.str.3(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-32(%rbp), %rax
	movq	(%rax), %rdi
	callq	prim_display
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
.LBB10_2:
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end10:
	.size	debug_output_registers, .Lfunc_end10-debug_output_registers
	.cfi_endproc
                                        # -- End function
	.globl	prim_display                    # -- Begin function prim_display
	.p2align	4, 0x90
	.type	prim_display,@function
prim_display:                           # @prim_display
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	prim_print
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end11:
	.size	prim_display, .Lfunc_end11-prim_display
	.cfi_endproc
                                        # -- End function
	.globl	alloc                           # -- Begin function alloc
	.p2align	4, 0x90
	.type	alloc,@function
alloc:                                  # @alloc
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	shlq	$4, %rdi
	callq	malloc@PLT
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end12:
	.size	alloc, .Lfunc_end12-alloc
	.cfi_endproc
                                        # -- End function
	.globl	alloc_atomic                    # -- Begin function alloc_atomic
	.p2align	4, 0x90
	.type	alloc_atomic,@function
alloc_atomic:                           # @alloc_atomic
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	alloc
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end13:
	.size	alloc_atomic, .Lfunc_end13-alloc_atomic
	.cfi_endproc
                                        # -- End function
	.globl	_get_both                       # -- Begin function _get_both
	.p2align	4, 0x90
	.type	_get_both,@function
_get_both:                              # @_get_both
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.5(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	-16(%rbp), %rax
	movq	(%rcx), %rdx
	movq	%rdx, (%rax)
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%rax)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	16(%rcx), %rdx
	movq	%rdx, (%rax)
	movq	24(%rcx), %rcx
	movq	%rcx, 8(%rax)
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end14:
	.size	_get_both, .Lfunc_end14-_get_both
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_cons                     # -- Begin function unwrap_cons
	.p2align	4, 0x90
	.type	unwrap_cons,@function
unwrap_cons:                            # @unwrap_cons
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB15_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.26(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB15_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end15:
	.size	unwrap_cons, .Lfunc_end15-unwrap_cons
	.cfi_endproc
                                        # -- End function
	.globl	const_init_int                  # -- Begin function const_init_int
	.p2align	4, 0x90
	.type	const_init_int,@function
const_init_int:                         # @const_init_int
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	$1, %edi
	callq	alloc_atomic
	movq	%rax, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-16(%rbp), %rax
	movl	$5, 8(%rax)
	movq	-16(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end16:
	.size	const_init_int, .Lfunc_end16-const_init_int
	.cfi_endproc
                                        # -- End function
	.globl	const_init_void                 # -- Begin function const_init_void
	.p2align	4, 0x90
	.type	const_init_void,@function
const_init_void:                        # @const_init_void
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$1, %edi
	callq	alloc_atomic
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	$0, (%rax)
	movq	-8(%rbp), %rax
	movl	$0, 8(%rax)
	movq	-8(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end17:
	.size	const_init_void, .Lfunc_end17-const_init_void
	.cfi_endproc
                                        # -- End function
	.globl	const_init_true                 # -- Begin function const_init_true
	.p2align	4, 0x90
	.type	const_init_true,@function
const_init_true:                        # @const_init_true
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$1, %edi
	callq	alloc_atomic
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movl	$1, %ecx
	movq	%rcx, (%rax)
	movq	-8(%rbp), %rax
	movl	$2, 8(%rax)
	movq	-8(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end18:
	.size	const_init_true, .Lfunc_end18-const_init_true
	.cfi_endproc
                                        # -- End function
	.globl	const_init_false                # -- Begin function const_init_false
	.p2align	4, 0x90
	.type	const_init_false,@function
const_init_false:                       # @const_init_false
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	$1, %edi
	callq	alloc_atomic
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	$0, (%rax)
	movq	-8(%rbp), %rax
	movl	$2, 8(%rax)
	movq	-8(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end19:
	.size	const_init_false, .Lfunc_end19-const_init_false
	.cfi_endproc
                                        # -- End function
	.globl	const_init_string               # -- Begin function const_init_string
	.p2align	4, 0x90
	.type	const_init_string,@function
const_init_string:                      # @const_init_string
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-16(%rbp), %rax
	movl	$6, 8(%rax)
	movq	-16(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end20:
	.size	const_init_string, .Lfunc_end20-const_init_string
	.cfi_endproc
                                        # -- End function
	.globl	const_init_symbol               # -- Begin function const_init_symbol
	.p2align	4, 0x90
	.type	const_init_symbol,@function
const_init_symbol:                      # @const_init_symbol
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -16(%rbp)
	movq	-8(%rbp), %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-16(%rbp), %rax
	movl	$7, 8(%rax)
	movq	-16(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end21:
	.size	const_init_symbol, .Lfunc_end21-const_init_symbol
	.cfi_endproc
                                        # -- End function
	.globl	make_record                     # -- Begin function make_record
	.p2align	4, 0x90
	.type	make_record,@function
make_record:                            # @make_record
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$40, %edi
	callq	malloc@PLT
	movq	%rax, -24(%rbp)
	movl	$4096, %edi                     # imm = 0x1000
	movl	$8, %esi
	callq	calloc@PLT
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rcx
	addq	$32768, %rcx                    # imm = 0x8000
	addq	$-800, %rcx                     # imm = 0xFCE0
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-24(%rbp), %rax
	movq	$512, 16(%rax)                  # imm = 0x200
	movq	-8(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, 8(%rax)
	movq	-24(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end22:
	.size	make_record, .Lfunc_end22-make_record
	.cfi_endproc
                                        # -- End function
	.globl	split_record                    # -- Begin function split_record
	.p2align	4, 0x90
	.type	split_record,@function
split_record:                           # @split_record
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8, -40(%rbp)
	movl	$40, %edi
	callq	malloc@PLT
	movq	%rax, -48(%rbp)
	movq	-16(%rbp), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-8(%rbp), %rax
	movq	16(%rax), %rcx
	subq	-24(%rbp), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, 16(%rax)
	movq	-8(%rbp), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, 8(%rax)
	movq	-24(%rbp), %rcx
	movq	-8(%rbp), %rax
	movq	%rcx, 16(%rax)
	movq	-32(%rbp), %rcx
	movq	-8(%rbp), %rax
	movq	%rcx, 24(%rax)
	movq	-40(%rbp), %rcx
	movq	-8(%rbp), %rax
	movq	%rcx, 32(%rax)
	movq	-48(%rbp), %rax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end23:
	.size	split_record, .Lfunc_end23-split_record
	.cfi_endproc
                                        # -- End function
	.globl	make_continuation_closure       # -- Begin function make_continuation_closure
	.p2align	4, 0x90
	.type	make_continuation_closure,@function
make_continuation_closure:              # @make_continuation_closure
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$2, %edi
	callq	alloc
	movq	%rax, -24(%rbp)
	movl	$11, -32(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -40(%rbp)
	movl	$12, -48(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	%rcx, (%rax)
	movq	-32(%rbp), %rcx
	movq	%rcx, 8(%rax)
	movq	-24(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	%rcx, 16(%rax)
	movq	-48(%rbp), %rcx
	movq	%rcx, 24(%rax)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movl	$3, 8(%rax)
	movq	-24(%rbp), %rcx
	movq	-64(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-64(%rbp), %rax
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end24:
	.size	make_continuation_closure, .Lfunc_end24-make_continuation_closure
	.cfi_endproc
                                        # -- End function
	.globl	closure_alloc                   # -- Begin function closure_alloc
	.p2align	4, 0x90
	.type	closure_alloc,@function
closure_alloc:                          # @closure_alloc
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$2, %edi
	callq	alloc
	movq	%rax, -24(%rbp)
	movl	$11, -32(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -40(%rbp)
	movl	$11, -48(%rbp)
	movq	-8(%rbp), %rdi
	callq	const_init_int
	movq	%rax, -72(%rbp)                 # 8-byte Spill
	xorl	%eax, %eax
	movl	%eax, %edi
	callq	const_init_int
	movq	-72(%rbp), %rdi                 # 8-byte Reload
	movq	%rax, %rsi
	callq	prim_make_45vector
	movq	%rax, -56(%rbp)
	movq	-24(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	%rcx, (%rax)
	movq	-32(%rbp), %rcx
	movq	%rcx, 8(%rax)
	movq	-24(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	%rcx, 16(%rax)
	movq	-48(%rbp), %rcx
	movq	%rcx, 24(%rax)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movl	$3, 8(%rax)
	movq	-24(%rbp), %rcx
	movq	-64(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-64(%rbp), %rax
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end25:
	.size	closure_alloc, .Lfunc_end25-closure_alloc
	.cfi_endproc
                                        # -- End function
	.globl	prim_make_45vector              # -- Begin function prim_make_45vector
	.p2align	4, 0x90
	.type	prim_make_45vector,@function
prim_make_45vector:                     # @prim_make_45vector
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.23(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	addq	$1, %rdi
	callq	alloc
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movl	$5, 8(%rax)
	movq	-24(%rbp), %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, (%rax)
	movq	$1, -40(%rbp)
.LBB26_1:                               # =>This Inner Loop Header: Depth=1
	movq	-40(%rbp), %rax
	cmpq	-24(%rbp), %rax
	ja	.LBB26_4
# %bb.2:                                #   in Loop: Header=BB26_1 Depth=1
	movl	$1, %edi
	callq	alloc
	movq	%rax, -48(%rbp)
	movq	-16(%rbp), %rax
	movl	8(%rax), %ecx
	movq	-48(%rbp), %rax
	movl	%ecx, 8(%rax)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movl	$11, 8(%rax)
	movq	-48(%rbp), %rcx
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	shlq	$4, %rdx
	addq	%rdx, %rax
	movq	%rcx, (%rax)
# %bb.3:                                #   in Loop: Header=BB26_1 Depth=1
	movq	-40(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -40(%rbp)
	jmp	.LBB26_1
.LBB26_4:
	movl	$1, %edi
	callq	alloc
	movq	%rax, -56(%rbp)
	movq	-32(%rbp), %rcx
	movq	-56(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-56(%rbp), %rax
	movl	$8, 8(%rax)
	movq	-56(%rbp), %rax
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end26:
	.size	prim_make_45vector, .Lfunc_end26-prim_make_45vector
	.cfi_endproc
                                        # -- End function
	.globl	closure_place_freevar           # -- Begin function closure_place_freevar
	.p2align	4, 0x90
	.type	closure_place_freevar,@function
closure_place_freevar:                  # @closure_place_freevar
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.6(%rip), %rsi
	callq	unwrap_clo
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rdi
	callq	const_init_int
	movq	%rax, -48(%rbp)
	movq	-40(%rbp), %rdi
	movq	-48(%rbp), %rsi
	movq	-16(%rbp), %rdx
	callq	prim_vector_45set_33
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end27:
	.size	closure_place_freevar, .Lfunc_end27-closure_place_freevar
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_clo                      # -- Begin function unwrap_clo
	.p2align	4, 0x90
	.type	unwrap_clo,@function
unwrap_clo:                             # @unwrap_clo
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$3, 8(%rax)
	je	.LBB28_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.28(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB28_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end28:
	.size	unwrap_clo, .Lfunc_end28-unwrap_clo
	.cfi_endproc
                                        # -- End function
	.globl	prim_vector_45set_33            # -- Begin function prim_vector_45set_33
	.p2align	4, 0x90
	.type	prim_vector_45set_33,@function
prim_vector_45set_33:                   # @prim_vector_45set_33
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.93(%rip), %rsi
	callq	unwrap_vector
	movq	%rax, -32(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.94(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -40(%rbp)
	movq	-32(%rbp), %rdi
	movq	-40(%rbp), %rsi
	callq	bounds_check
	movl	$1, %edi
	callq	alloc
	movq	%rax, -48(%rbp)
	movq	-24(%rbp), %rax
	movl	8(%rax), %ecx
	movq	-48(%rbp), %rax
	movl	%ecx, 8(%rax)
	movq	-24(%rbp), %rax
	movq	(%rax), %rcx
	movq	-48(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-48(%rbp), %rcx
	movq	-32(%rbp), %rax
	movq	-40(%rbp), %rdx
	addq	$1, %rdx
	shlq	$4, %rdx
	addq	%rdx, %rax
	movq	%rcx, (%rax)
	callq	const_init_void
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end29:
	.size	prim_vector_45set_33, .Lfunc_end29-prim_vector_45set_33
	.cfi_endproc
                                        # -- End function
	.globl	closure_get_env_part            # -- Begin function closure_get_env_part
	.p2align	4, 0x90
	.type	closure_get_env_part,@function
closure_get_env_part:                   # @closure_get_env_part
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.7(%rip), %rsi
	callq	unwrap_clo
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end30:
	.size	closure_get_env_part, .Lfunc_end30-closure_get_env_part
	.cfi_endproc
                                        # -- End function
	.globl	closure_get_fn_part             # -- Begin function closure_get_fn_part
	.p2align	4, 0x90
	.type	closure_get_fn_part,@function
closure_get_fn_part:                    # @closure_get_fn_part
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.8(%rip), %rsi
	callq	unwrap_clo
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end31:
	.size	closure_get_fn_part, .Lfunc_end31-closure_get_fn_part
	.cfi_endproc
                                        # -- End function
	.globl	check_for_overflow              # -- Begin function check_for_overflow
	.p2align	4, 0x90
	.type	check_for_overflow,@function
check_for_overflow:                     # @check_for_overflow
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	-24(%rbp), %rcx
	shlq	$3, %rcx
	addq	%rcx, %rax
	movq	-16(%rbp), %rcx
	movq	(%rcx), %rcx
	cmpq	%rcx, %rax
	setl	%al
	andb	$1, %al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end32:
	.size	check_for_overflow, .Lfunc_end32-check_for_overflow
	.cfi_endproc
                                        # -- End function
	.globl	callcc_at_base                  # -- Begin function callcc_at_base
	.p2align	4, 0x90
	.type	callcc_at_base,@function
callcc_at_base:                         # @callcc_at_base
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	-16(%rbp), %rcx
	cmpq	(%rcx), %rax
	sete	%al
	andb	$1, %al
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end33:
	.size	callcc_at_base, .Lfunc_end33-callcc_at_base
	.cfi_endproc
                                        # -- End function
	.globl	handle_overflow                 # -- Begin function handle_overflow
	.p2align	4, 0x90
	.type	handle_overflow,@function
handle_overflow:                        # @handle_overflow
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$112, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8, -40(%rbp)
	movq	$0, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -56(%rbp)
	movb	$0, -57(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -72(%rbp)
	movl	$0, -76(%rbp)
.LBB34_1:                               # =>This Inner Loop Header: Depth=1
	cmpl	$4, -76(%rbp)
	jge	.LBB34_6
# %bb.2:                                #   in Loop: Header=BB34_1 Depth=1
	movq	-72(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -48(%rbp)
	cmpq	$-1, -48(%rbp)
	jne	.LBB34_4
# %bb.3:
	movb	$1, -57(%rbp)
	jmp	.LBB34_6
.LBB34_4:                               #   in Loop: Header=BB34_1 Depth=1
	movq	-48(%rbp), %rax
	addq	-56(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-48(%rbp), %rdx
	movq	-72(%rbp), %rax
	xorl	%ecx, %ecx
                                        # kill: def $rcx killed $ecx
	subq	%rdx, %rcx
	shlq	$3, %rcx
	addq	%rcx, %rax
	movq	%rax, -72(%rbp)
# %bb.5:                                #   in Loop: Header=BB34_1 Depth=1
	movl	-76(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -76(%rbp)
	jmp	.LBB34_1
.LBB34_6:
	testb	$1, -57(%rbp)
	je	.LBB34_11
# %bb.7:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -88(%rbp)
	movq	-88(%rbp), %rax
	movq	8(%rax), %rdi
	movq	-24(%rbp), %rsi
	callq	make_record
	movq	%rax, -96(%rbp)
	movq	-72(%rbp), %rax
	movq	-88(%rbp), %rcx
	cmpq	(%rcx), %rax
	jne	.LBB34_9
# %bb.8:
	jmp	.LBB34_10
.LBB34_9:
	leaq	.L.str.9(%rip), %rdi
	leaq	.L.str.10(%rip), %rsi
	movl	$249, %edx
	leaq	.L__PRETTY_FUNCTION__.handle_overflow(%rip), %rcx
	callq	__assert_fail@PLT
.LBB34_10:
	movq	-96(%rbp), %rax
	movq	(%rax), %rdi
	movq	-72(%rbp), %rsi
	movq	-56(%rbp), %rdx
	shlq	$3, %rdx
	callq	memcpy@PLT
	movq	-96(%rbp), %rcx
	movq	-8(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-96(%rbp), %rax
	movq	(%rax), %rcx
	movq	-56(%rbp), %rax
	shlq	$3, %rax
	addq	%rax, %rcx
	xorl	%eax, %eax
                                        # kill: def $rax killed $eax
	subq	-40(%rbp), %rax
	shlq	$3, %rax
	addq	%rax, %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	jmp	.LBB34_12
.LBB34_11:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rdi
	movq	-24(%rbp), %rsi
	callq	make_record
	movq	%rax, -112(%rbp)
	movq	-72(%rbp), %rax
	movq	(%rax), %rcx
	movq	-104(%rbp), %rax
	movq	%rcx, 32(%rax)
	movq	-72(%rbp), %rax
	movq	-104(%rbp), %rcx
	movq	(%rcx), %rcx
	subq	%rcx, %rax
	movl	$8, %ecx
	cqto
	idivq	%rcx
	movq	%rax, %rcx
	movq	-104(%rbp), %rax
	movq	%rcx, 16(%rax)
	movq	-72(%rbp), %rax
	movq	8(%rax), %rcx
	movq	-104(%rbp), %rax
	movq	%rcx, 24(%rax)
	movq	-112(%rbp), %rax
	movq	(%rax), %rdi
	movq	-72(%rbp), %rsi
	movq	-56(%rbp), %rdx
	shlq	$3, %rdx
	callq	memcpy@PLT
	movq	-32(%rbp), %rcx
	movq	-112(%rbp), %rax
	movq	(%rax), %rax
	movq	%rcx, (%rax)
	movq	-112(%rbp), %rax
	movq	(%rax), %rax
	movq	$-1, %rcx
	movq	%rcx, 8(%rax)
	movq	-112(%rbp), %rcx
	movq	-8(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-112(%rbp), %rax
	movq	(%rax), %rcx
	movq	-56(%rbp), %rax
	shlq	$3, %rax
	addq	%rax, %rcx
	xorl	%eax, %eax
                                        # kill: def $rax killed $eax
	subq	-40(%rbp), %rax
	shlq	$3, %rax
	addq	%rax, %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
.LBB34_12:
	addq	$112, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end34:
	.size	handle_overflow, .Lfunc_end34-handle_overflow
	.cfi_endproc
                                        # -- End function
	.globl	handle_underflow                # -- Begin function handle_underflow
	.p2align	4, 0x90
	.type	handle_underflow,@function
handle_underflow:                       # @handle_underflow
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	8(%rax), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	8(%rax), %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, 8(%rax)
	movq	-32(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rax, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rax, -56(%rbp)
	movq	-40(%rbp), %rax
	movq	24(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-40(%rbp), %rax
	movq	32(%rax), %rax
	movq	%rax, -72(%rbp)
	movq	-56(%rbp), %rax
	cmpq	-48(%rbp), %rax
	jl	.LBB35_2
# %bb.1:
	movl	$4096, %edi                     # imm = 0x1000
	movl	$8, %esi
	callq	calloc@PLT
	movq	%rax, %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-32(%rbp), %rax
	movq	(%rax), %rcx
	addq	$32768, %rcx                    # imm = 0x8000
	addq	$-800, %rcx                     # imm = 0xFCE0
	movq	-24(%rbp), %rax
	movq	%rcx, (%rax)
.LBB35_2:
	movq	-32(%rbp), %rax
	movq	(%rax), %rdi
	movq	-40(%rbp), %rax
	movq	(%rax), %rsi
	movq	-56(%rbp), %rdx
	shlq	$3, %rdx
	callq	memcpy@PLT
	movq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movq	-56(%rbp), %rax
	subq	-64(%rbp), %rax
	shlq	$3, %rax
	addq	%rax, %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-72(%rbp), %rax
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end35:
	.size	handle_underflow, .Lfunc_end35-handle_underflow
	.cfi_endproc
                                        # -- End function
	.globl	handle_continuation_function    # -- Begin function handle_continuation_function
	.p2align	4, 0x90
	.type	handle_continuation_function,@function
handle_continuation_function:           # @handle_continuation_function
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	24(%rax), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rdi
	callq	prim_car
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rcx
	movq	-32(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-40(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rdi
	callq	closure_get_env_part
	movq	%rax, -72(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -80(%rbp)
	movq	-72(%rbp), %rax
	movq	16(%rax), %rax
	addq	$3, %rax
	movq	-80(%rbp), %rcx
	cmpq	16(%rcx), %rax
	jl	.LBB36_2
# %bb.1:
	movq	-72(%rbp), %rax
	movq	8(%rax), %rdi
	movq	-24(%rbp), %rsi
	callq	make_record
	movq	%rax, -88(%rbp)
	movq	-88(%rbp), %rax
	movq	%rax, -80(%rbp)
	movq	-88(%rbp), %rcx
	movq	-8(%rbp), %rax
	movq	%rcx, (%rax)
.LBB36_2:
	movq	-80(%rbp), %rax
	movq	(%rax), %rdi
	movq	-72(%rbp), %rax
	movq	(%rax), %rsi
	movq	-72(%rbp), %rax
	movq	16(%rax), %rdx
	shlq	$3, %rdx
	callq	memcpy@PLT
	movq	-80(%rbp), %rax
	movq	(%rax), %rcx
	movq	-72(%rbp), %rax
	movq	16(%rax), %rax
	movq	-72(%rbp), %rdx
	subq	24(%rdx), %rax
	shlq	$3, %rax
	addq	%rax, %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-72(%rbp), %rax
	movq	32(%rax), %rax
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end36:
	.size	handle_continuation_function, .Lfunc_end36-handle_continuation_function
	.cfi_endproc
                                        # -- End function
	.globl	prim_car                        # -- Begin function prim_car
	.p2align	4, 0x90
	.type	prim_car,@function
prim_car:                               # @prim_car
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.15(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end37:
	.size	prim_car, .Lfunc_end37-prim_car
	.cfi_endproc
                                        # -- End function
	.globl	start_runtime                   # -- Begin function start_runtime
	.p2align	4, 0x90
	.type	start_runtime,@function
start_runtime:                          # @start_runtime
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-24(%rbp), %rsi
	xorl	%eax, %eax
	movl	%eax, %edi
	callq	make_record
	movq	%rax, %rcx
	movq	-8(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	(%rax), %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end38:
	.size	start_runtime, .Lfunc_end38-start_runtime
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_car                   # -- Begin function applyprim_car
	.p2align	4, 0x90
	.type	applyprim_car,@function
applyprim_car:                          # @applyprim_car
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB39_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.13(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB39_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.13(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB39_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.13(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB39_4:
	leaq	-32(%rbp), %rdi
	callq	prim_car
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end39:
	.size	applyprim_car, .Lfunc_end39-applyprim_car
	.cfi_endproc
                                        # -- End function
	.globl	prim__43                        # -- Begin function prim__43
	.p2align	4, 0x90
	.type	prim__43,@function
prim__43:                               # @prim__43
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.16(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.17(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	addq	-32(%rbp), %rdi
	callq	const_init_int
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end40:
	.size	prim__43, .Lfunc_end40-prim__43
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_int                      # -- Begin function unwrap_int
	.p2align	4, 0x90
	.type	unwrap_int,@function
unwrap_int:                             # @unwrap_int
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$5, 8(%rax)
	je	.LBB41_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.29(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB41_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end41:
	.size	unwrap_int, .Lfunc_end41-unwrap_int
	.cfi_endproc
                                        # -- End function
	.globl	applyprim__43                   # -- Begin function applyprim__43
	.p2align	4, 0x90
	.type	applyprim__43,@function
applyprim__43:                          # @applyprim__43
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movl	8(%rax), %eax
	movl	%eax, -12(%rbp)
	cmpl	$4, -12(%rbp)
	je	.LBB42_3
# %bb.1:
	cmpl	$1, -12(%rbp)
	je	.LBB42_3
# %bb.2:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	-12(%rbp), %edi
	callq	get_type_name
	movq	%rax, %rsi
	leaq	.L.str.18(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB42_3:
	movq	$0, -24(%rbp)
.LBB42_4:                               # =>This Inner Loop Header: Depth=1
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	jne	.LBB42_6
# %bb.5:                                #   in Loop: Header=BB42_4 Depth=1
	movq	-8(%rbp), %rdi
	leaq	-40(%rbp), %rsi
	leaq	-56(%rbp), %rdx
	callq	_get_both
	leaq	-40(%rbp), %rdi
	leaq	.L.str.19(%rip), %rsi
	callq	unwrap_int
	addq	-24(%rbp), %rax
	movq	%rax, -24(%rbp)
	leaq	-56(%rbp), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB42_4
.LBB42_6:
	movq	-24(%rbp), %rdi
	callq	const_init_int
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end42:
	.size	applyprim__43, .Lfunc_end42-applyprim__43
	.cfi_endproc
                                        # -- End function
	.globl	get_type_name                   # -- Begin function get_type_name
	.p2align	4, 0x90
	.type	get_type_name,@function
get_type_name:                          # @get_type_name
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movl	%edi, -12(%rbp)
	movl	-12(%rbp), %eax
                                        # kill: def $rax killed $eax
	movq	%rax, -24(%rbp)                 # 8-byte Spill
	subq	$12, %rax
	ja	.LBB43_14
# %bb.16:
	movq	-24(%rbp), %rax                 # 8-byte Reload
	leaq	.LJTI43_0(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmpq	*%rax
.LBB43_1:
	leaq	.L.str.65(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_2:
	leaq	.L.str.66(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_3:
	leaq	.L.str.67(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_4:
	leaq	.L.str.68(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_5:
	leaq	.L.str.69(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_6:
	leaq	.L.str.70(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_7:
	leaq	.L.str.71(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_8:
	leaq	.L.str.72(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_9:
	leaq	.L.str.73(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_10:
	leaq	.L.str.74(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_11:
	leaq	.L.str.75(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_12:
	leaq	.L.str.76(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_13:
	leaq	.L.str.77(%rip), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB43_15
.LBB43_14:
	ud2
.LBB43_15:
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end43:
	.size	get_type_name, .Lfunc_end43-get_type_name
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	2
.LJTI43_0:
	.long	.LBB43_1-.LJTI43_0
	.long	.LBB43_2-.LJTI43_0
	.long	.LBB43_3-.LJTI43_0
	.long	.LBB43_5-.LJTI43_0
	.long	.LBB43_6-.LJTI43_0
	.long	.LBB43_7-.LJTI43_0
	.long	.LBB43_8-.LJTI43_0
	.long	.LBB43_9-.LJTI43_0
	.long	.LBB43_10-.LJTI43_0
	.long	.LBB43_11-.LJTI43_0
	.long	.LBB43_12-.LJTI43_0
	.long	.LBB43_13-.LJTI43_0
	.long	.LBB43_4-.LJTI43_0
                                        # -- End function
	.text
	.globl	applyprim_make_45vector         # -- Begin function applyprim_make_45vector
	.p2align	4, 0x90
	.type	applyprim_make_45vector,@function
applyprim_make_45vector:                # @applyprim_make_45vector
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB44_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.20(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB44_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.20(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB44_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.20(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB44_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.20(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB44_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.20(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB44_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim_make_45vector
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end44:
	.size	applyprim_make_45vector, .Lfunc_end44-applyprim_make_45vector
	.cfi_endproc
                                        # -- End function
	.globl	closure_env_get                 # -- Begin function closure_env_get
	.p2align	4, 0x90
	.type	closure_env_get,@function
closure_env_get:                        # @closure_env_get
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.24(%rip), %rsi
	callq	unwrap_clo
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)                 # 8-byte Spill
	movq	-16(%rbp), %rdi
	callq	const_init_int
	movq	-40(%rbp), %rdi                 # 8-byte Reload
	movq	%rax, %rsi
	callq	prim_vector_45ref
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end45:
	.size	closure_env_get, .Lfunc_end45-closure_env_get
	.cfi_endproc
                                        # -- End function
	.globl	prim_vector_45ref               # -- Begin function prim_vector_45ref
	.p2align	4, 0x90
	.type	prim_vector_45ref,@function
prim_vector_45ref:                      # @prim_vector_45ref
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.87(%rip), %rsi
	callq	unwrap_vector
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.88(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %rsi
	callq	bounds_check
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rcx
	addq	$1, %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movq	(%rax), %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end46:
	.size	prim_vector_45ref, .Lfunc_end46-prim_vector_45ref
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_hash                     # -- Begin function unwrap_hash
	.p2align	4, 0x90
	.type	unwrap_hash,@function
unwrap_hash:                            # @unwrap_hash
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$9, 8(%rax)
	je	.LBB47_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.25(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB47_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end47:
	.size	unwrap_hash, .Lfunc_end47-unwrap_hash
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_vector                   # -- Begin function unwrap_vector
	.p2align	4, 0x90
	.type	unwrap_vector,@function
unwrap_vector:                          # @unwrap_vector
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$8, 8(%rax)
	je	.LBB48_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.27(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB48_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end48:
	.size	unwrap_vector, .Lfunc_end48-unwrap_vector
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_bool                     # -- Begin function unwrap_bool
	.p2align	4, 0x90
	.type	unwrap_bool,@function
unwrap_bool:                            # @unwrap_bool
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$2, 8(%rax)
	je	.LBB49_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.30(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB49_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end49:
	.size	unwrap_bool, .Lfunc_end49-unwrap_bool
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_str                      # -- Begin function unwrap_str
	.p2align	4, 0x90
	.type	unwrap_str,@function
unwrap_str:                             # @unwrap_str
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$6, 8(%rax)
	je	.LBB50_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.31(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB50_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end50:
	.size	unwrap_str, .Lfunc_end50-unwrap_str
	.cfi_endproc
                                        # -- End function
	.globl	unwrap_sym                      # -- Begin function unwrap_sym
	.p2align	4, 0x90
	.type	unwrap_sym,@function
unwrap_sym:                             # @unwrap_sym
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$7, 8(%rax)
	je	.LBB51_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rax
	movl	8(%rax), %esi
	movq	-16(%rbp), %rdx
	leaq	.L.str.32(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB51_2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end51:
	.size	unwrap_sym, .Lfunc_end51-unwrap_sym
	.cfi_endproc
                                        # -- End function
	.globl	is_truthy_value                 # -- Begin function is_truthy_value
	.p2align	4, 0x90
	.type	is_truthy_value,@function
is_truthy_value:                        # @is_truthy_value
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rcx
	xorl	%eax, %eax
                                        # kill: def $al killed $al killed $eax
	cmpl	$2, 8(%rcx)
	movb	%al, -9(%rbp)                   # 1-byte Spill
	jne	.LBB52_2
# %bb.1:
	movq	-8(%rbp), %rdi
	leaq	.L.str.33(%rip), %rsi
	callq	unwrap_bool
	cmpq	$0, %rax
	sete	%al
	movb	%al, -9(%rbp)                   # 1-byte Spill
.LBB52_2:
	movb	-9(%rbp), %al                   # 1-byte Reload
	andb	$1, %al
	addb	$-1, %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end52:
	.size	is_truthy_value, .Lfunc_end52-is_truthy_value
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_display               # -- Begin function applyprim_display
	.p2align	4, 0x90
	.type	applyprim_display,@function
applyprim_display:                      # @applyprim_display
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB53_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.34(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB53_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.34(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB53_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.34(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB53_4:
	leaq	-32(%rbp), %rdi
	callq	prim_display
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end53:
	.size	applyprim_display, .Lfunc_end53-applyprim_display
	.cfi_endproc
                                        # -- End function
	.globl	prim_print                      # -- Begin function prim_print
	.p2align	4, 0x90
	.type	prim_print,@function
prim_print:                             # @prim_print
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movl	8(%rax), %eax
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %eax
                                        # kill: def $rax killed $eax
	movq	%rax, -24(%rbp)                 # 8-byte Spill
	subq	$12, %rax
	ja	.LBB54_6
# %bb.7:
	movq	-24(%rbp), %rax                 # 8-byte Reload
	leaq	.LJTI54_0(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmpq	*%rax
.LBB54_1:
	leaq	.L.str.36(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB54_6
.LBB54_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.38(%rip), %rsi
	callq	unwrap_sym
	movq	%rax, %rsi
	leaq	.L.str.37(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB54_6
.LBB54_3:
	jmp	.LBB54_6
.LBB54_4:
	leaq	.L.str.39(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rdi
	callq	print_cons
	leaq	.L.str.40(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB54_6
.LBB54_5:
	movq	-8(%rbp), %rdi
	callq	prim_print_aux
.LBB54_6:
	callq	const_init_void
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end54:
	.size	prim_print, .Lfunc_end54-prim_print
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	2
.LJTI54_0:
	.long	.LBB54_3-.LJTI54_0
	.long	.LBB54_1-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_4-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_2-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
	.long	.LBB54_5-.LJTI54_0
                                        # -- End function
	.text
	.globl	applyprim_print                 # -- Begin function applyprim_print
	.p2align	4, 0x90
	.type	applyprim_print,@function
applyprim_print:                        # @applyprim_print
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB55_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.35(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB55_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.35(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB55_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.35(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB55_4:
	leaq	-32(%rbp), %rdi
	callq	prim_print
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end55:
	.size	applyprim_print, .Lfunc_end55-applyprim_print
	.cfi_endproc
                                        # -- End function
	.globl	print_cons                      # -- Begin function print_cons
	.p2align	4, 0x90
	.type	print_cons,@function
print_cons:                             # @print_cons
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.59(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	addq	$16, %rax
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	callq	prim_print_aux
	movq	-32(%rbp), %rax
	movl	8(%rax), %eax
                                        # kill: def $rax killed $eax
	movq	%rax, -40(%rbp)                 # 8-byte Spill
	subq	$12, %rax
	ja	.LBB56_5
# %bb.6:
	movq	-40(%rbp), %rax                 # 8-byte Reload
	leaq	.LJTI56_0(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmpq	*%rax
.LBB56_1:
	jmp	.LBB56_5
.LBB56_2:
	leaq	.L.str.42(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB56_5
.LBB56_3:
	leaq	.L.str.60(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-32(%rbp), %rdi
	callq	print_cons
	jmp	.LBB56_5
.LBB56_4:
	leaq	.L.str.61(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-32(%rbp), %rdi
	callq	prim_print_aux
.LBB56_5:
	callq	const_init_void
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end56:
	.size	print_cons, .Lfunc_end56-print_cons
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	2
.LJTI56_0:
	.long	.LBB56_2-.LJTI56_0
	.long	.LBB56_1-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_3-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
	.long	.LBB56_4-.LJTI56_0
                                        # -- End function
	.text
	.globl	prim_print_aux                  # -- Begin function prim_print_aux
	.p2align	4, 0x90
	.type	prim_print_aux,@function
prim_print_aux:                         # @prim_print_aux
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movl	8(%rax), %eax
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %eax
                                        # kill: def $rax killed $eax
	movq	%rax, -32(%rbp)                 # 8-byte Spill
	subq	$12, %rax
	ja	.LBB57_20
# %bb.21:
	movq	-32(%rbp), %rax                 # 8-byte Reload
	leaq	.LJTI57_0(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	addq	%rcx, %rax
	jmpq	*%rax
.LBB57_1:
	leaq	.L.str.42(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_2:
	leaq	.L.str.43(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_3:
	movq	-8(%rbp), %rdi
	leaq	.L.str.44(%rip), %rsi
	callq	unwrap_bool
	movq	%rax, -24(%rbp)
	cmpq	$0, -24(%rbp)
	jne	.LBB57_5
# %bb.4:
	leaq	.L.str.45(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_9
.LBB57_5:
	cmpq	$1, -24(%rbp)
	jne	.LBB57_7
# %bb.6:
	leaq	.L.str.46(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_8
.LBB57_7:
	movq	-24(%rbp), %rsi
	leaq	.L.str.47(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
.LBB57_8:
	jmp	.LBB57_9
.LBB57_9:
	jmp	.LBB57_20
.LBB57_10:
	leaq	.L.str.48(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_11:
	leaq	.L.str.49(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_12:
	leaq	.L.str.50(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rdi
	callq	print_cons
	leaq	.L.str.40(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_13:
	movq	-8(%rbp), %rdi
	leaq	.L.str.52(%rip), %rsi
	callq	unwrap_int
	movq	%rax, %rsi
	leaq	.L.str.51(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_14:
	movq	-8(%rbp), %rdi
	leaq	.L.str.54(%rip), %rsi
	callq	unwrap_str
	movq	%rax, %rsi
	leaq	.L.str.53(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_15:
	movq	-8(%rbp), %rdi
	leaq	.L.str.55(%rip), %rsi
	callq	unwrap_sym
	movq	%rax, %rsi
	leaq	.L.str.53(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_16:
	movq	-8(%rbp), %rdi
	callq	print_vector
	jmp	.LBB57_20
.LBB57_17:
	leaq	.L.str.56(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-8(%rbp), %rdi
	callq	print_hash
	leaq	.L.str.40(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_18:
	leaq	.L.str.57(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	jmp	.LBB57_20
.LBB57_19:
	movq	-8(%rbp), %rax
	movq	(%rax), %rsi
	leaq	.L.str.58(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
.LBB57_20:
	callq	const_init_void
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end57:
	.size	prim_print_aux, .Lfunc_end57-prim_print_aux
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.p2align	2
.LJTI57_0:
	.long	.LBB57_1-.LJTI57_0
	.long	.LBB57_2-.LJTI57_0
	.long	.LBB57_3-.LJTI57_0
	.long	.LBB57_11-.LJTI57_0
	.long	.LBB57_12-.LJTI57_0
	.long	.LBB57_13-.LJTI57_0
	.long	.LBB57_14-.LJTI57_0
	.long	.LBB57_15-.LJTI57_0
	.long	.LBB57_16-.LJTI57_0
	.long	.LBB57_17-.LJTI57_0
	.long	.LBB57_18-.LJTI57_0
	.long	.LBB57_19-.LJTI57_0
	.long	.LBB57_10-.LJTI57_0
                                        # -- End function
	.text
	.globl	applyprim_println               # -- Begin function applyprim_println
	.p2align	4, 0x90
	.type	applyprim_println,@function
applyprim_println:                      # @applyprim_println
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB58_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.41(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB58_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.41(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB58_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.41(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB58_4:
	leaq	-32(%rbp), %rdi
	callq	prim_println
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end58:
	.size	applyprim_println, .Lfunc_end58-applyprim_println
	.cfi_endproc
                                        # -- End function
	.globl	prim_println                    # -- Begin function prim_println
	.p2align	4, 0x90
	.type	prim_println,@function
prim_println:                           # @prim_println
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	prim_print
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	callq	const_init_void
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end59:
	.size	prim_println, .Lfunc_end59-prim_println
	.cfi_endproc
                                        # -- End function
	.globl	print_vector                    # -- Begin function print_vector
	.p2align	4, 0x90
	.type	print_vector,@function
print_vector:                           # @print_vector
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.62(%rip), %rsi
	callq	unwrap_vector
	movq	%rax, -16(%rbp)
	movq	-8(%rbp), %rdi
	callq	_get_vector_length
	movq	%rax, -24(%rbp)
	leaq	.L.str.63(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	$1, -32(%rbp)
.LBB60_1:                               # =>This Inner Loop Header: Depth=1
	movq	-32(%rbp), %rax
	cmpq	-24(%rbp), %rax
	ja	.LBB60_6
# %bb.2:                                #   in Loop: Header=BB60_1 Depth=1
	cmpq	$1, -32(%rbp)
	je	.LBB60_4
# %bb.3:                                #   in Loop: Header=BB60_1 Depth=1
	leaq	.L.str.60(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
.LBB60_4:                               #   in Loop: Header=BB60_1 Depth=1
	movq	-16(%rbp), %rax
	movq	-32(%rbp), %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movq	(%rax), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdi
	callq	prim_print_aux
# %bb.5:                                #   in Loop: Header=BB60_1 Depth=1
	movq	-32(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -32(%rbp)
	jmp	.LBB60_1
.LBB60_6:
	leaq	.L.str.40(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	callq	const_init_void
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end60:
	.size	print_vector, .Lfunc_end60-print_vector
	.cfi_endproc
                                        # -- End function
	.globl	print_hash                      # -- Begin function print_hash
	.p2align	4, 0x90
	.type	print_hash,@function
print_hash:                             # @print_hash
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.64(%rip), %rsi
	callq	unwrap_hash
	movq	%rax, -16(%rbp)
.LBB61_1:                               # =>This Inner Loop Header: Depth=1
	cmpq	$0, -16(%rbp)
	je	.LBB61_5
# %bb.2:                                #   in Loop: Header=BB61_1 Depth=1
	leaq	.L.str.50(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-16(%rbp), %rax
	movq	8(%rax), %rdi
	callq	prim_print_aux
	leaq	.L.str.61(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-16(%rbp), %rax
	movq	16(%rax), %rdi
	callq	prim_print_aux
	leaq	.L.str.40(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-16(%rbp), %rax
	cmpq	$0, (%rax)
	je	.LBB61_4
# %bb.3:                                #   in Loop: Header=BB61_1 Depth=1
	leaq	.L.str.60(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
.LBB61_4:                               #   in Loop: Header=BB61_1 Depth=1
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -16(%rbp)
	jmp	.LBB61_1
.LBB61_5:
	callq	const_init_void
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end61:
	.size	print_hash, .Lfunc_end61-print_hash
	.cfi_endproc
                                        # -- End function
	.globl	_get_vector_length              # -- Begin function _get_vector_length
	.p2align	4, 0x90
	.type	_get_vector_length,@function
_get_vector_length:                     # @_get_vector_length
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.79(%rip), %rsi
	callq	unwrap_vector
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end62:
	.size	_get_vector_length, .Lfunc_end62-_get_vector_length
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_halt                  # -- Begin function applyprim_halt
	.p2align	4, 0x90
	.type	applyprim_halt,@function
applyprim_halt:                         # @applyprim_halt
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB63_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.78(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB63_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.78(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB63_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.78(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB63_4:
	leaq	-32(%rbp), %rdi
	callq	prim_halt
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end63:
	.size	applyprim_halt, .Lfunc_end63-applyprim_halt
	.cfi_endproc
                                        # -- End function
	.globl	prim_halt                       # -- Begin function prim_halt
	.p2align	4, 0x90
	.type	prim_halt,@function
prim_halt:                              # @prim_halt
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	prim_print
	movq	-8(%rbp), %rax
	cmpl	$0, 8(%rax)
	je	.LBB64_2
# %bb.1:
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
.LBB64_2:
	xorl	%edi, %edi
	callq	exit@PLT
.Lfunc_end64:
	.size	prim_halt, .Lfunc_end64-prim_halt
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_vector                # -- Begin function applyprim_vector
	.p2align	4, 0x90
	.type	applyprim_vector,@function
applyprim_vector:                       # @applyprim_vector
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$2160, %rsp                     # imm = 0x870
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -24(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -16(%rbp)
	leaq	-2080(%rbp), %rdi
	xorl	%esi, %esi
	movl	$2048, %edx                     # imm = 0x800
	callq	memset@PLT
	movq	$0, -2088(%rbp)
.LBB65_1:                               # =>This Inner Loop Header: Depth=1
	xorl	%eax, %eax
                                        # kill: def $al killed $al killed $eax
	cmpl	$4, -16(%rbp)
	movb	%al, -2145(%rbp)                # 1-byte Spill
	jne	.LBB65_3
# %bb.2:                                #   in Loop: Header=BB65_1 Depth=1
	cmpq	$256, -2088(%rbp)               # imm = 0x100
	setb	%al
	movb	%al, -2145(%rbp)                # 1-byte Spill
.LBB65_3:                               #   in Loop: Header=BB65_1 Depth=1
	movb	-2145(%rbp), %al                # 1-byte Reload
	testb	$1, %al
	jne	.LBB65_4
	jmp	.LBB65_5
.LBB65_4:                               #   in Loop: Header=BB65_1 Depth=1
	leaq	-24(%rbp), %rdi
	leaq	-2104(%rbp), %rsi
	leaq	-2120(%rbp), %rdx
	callq	_get_both
	movl	$1, %edi
	callq	alloc
	movq	%rax, %rcx
	movq	-2088(%rbp), %rax
	movq	%rcx, -2080(%rbp,%rax,8)
	movl	-2096(%rbp), %ecx
	movq	-2088(%rbp), %rax
	movq	-2080(%rbp,%rax,8), %rax
	movl	%ecx, 8(%rax)
	movq	-2104(%rbp), %rcx
	movq	-2088(%rbp), %rax
	movq	%rax, %rdx
	addq	$1, %rdx
	movq	%rdx, -2088(%rbp)
	movq	-2080(%rbp,%rax,8), %rax
	movq	%rcx, (%rax)
	movq	-2120(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-2112(%rbp), %rax
	movq	%rax, -16(%rbp)
	jmp	.LBB65_1
.LBB65_5:
	cmpq	$256, -2088(%rbp)               # imm = 0x100
	jne	.LBB65_8
# %bb.6:
	cmpl	$1, -16(%rbp)
	je	.LBB65_8
# %bb.7:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.80(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB65_8:
	movq	-2088(%rbp), %rdi
	addq	$1, %rdi
	callq	alloc
	movq	%rax, -2128(%rbp)
	movq	-2128(%rbp), %rax
	movl	$5, 8(%rax)
	movq	-2088(%rbp), %rcx
	movq	-2128(%rbp), %rax
	movq	%rcx, (%rax)
	movq	$0, -2136(%rbp)
.LBB65_9:                               # =>This Inner Loop Header: Depth=1
	movq	-2136(%rbp), %rax
	cmpq	-2088(%rbp), %rax
	jae	.LBB65_12
# %bb.10:                               #   in Loop: Header=BB65_9 Depth=1
	movq	-2128(%rbp), %rax
	movq	-2136(%rbp), %rcx
	addq	$1, %rcx
	shlq	$4, %rcx
	addq	%rcx, %rax
	movl	$11, 8(%rax)
	movq	-2136(%rbp), %rax
	movq	-2080(%rbp,%rax,8), %rcx
	movq	-2128(%rbp), %rax
	movq	-2136(%rbp), %rdx
	addq	$1, %rdx
	shlq	$4, %rdx
	addq	%rdx, %rax
	movq	%rcx, (%rax)
# %bb.11:                               #   in Loop: Header=BB65_9 Depth=1
	movq	-2136(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -2136(%rbp)
	jmp	.LBB65_9
.LBB65_12:
	movl	$1, %edi
	callq	alloc
	movq	%rax, -2144(%rbp)
	movq	-2128(%rbp), %rcx
	movq	-2144(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-2144(%rbp), %rax
	movl	$8, 8(%rax)
	movq	-2144(%rbp), %rax
	addq	$2160, %rsp                     # imm = 0x870
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end65:
	.size	applyprim_vector, .Lfunc_end65-applyprim_vector
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_vector_63             # -- Begin function applyprim_vector_63
	.p2align	4, 0x90
	.type	applyprim_vector_63,@function
applyprim_vector_63:                    # @applyprim_vector_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB66_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.81(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB66_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.81(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB66_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.81(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB66_4:
	leaq	-32(%rbp), %rdi
	callq	prim_vector_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end66:
	.size	applyprim_vector_63, .Lfunc_end66-applyprim_vector_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_vector_63                  # -- Begin function prim_vector_63
	.p2align	4, 0x90
	.type	prim_vector_63,@function
prim_vector_63:                         # @prim_vector_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$8, 8(%rax)
	jne	.LBB67_2
# %bb.1:
	callq	const_init_true
	movq	%rax, -16(%rbp)                 # 8-byte Spill
	jmp	.LBB67_3
.LBB67_2:
	callq	const_init_false
	movq	%rax, -16(%rbp)                 # 8-byte Spill
.LBB67_3:
	movq	-16(%rbp), %rax                 # 8-byte Reload
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end67:
	.size	prim_vector_63, .Lfunc_end67-prim_vector_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_vector_45length       # -- Begin function applyprim_vector_45length
	.p2align	4, 0x90
	.type	applyprim_vector_45length,@function
applyprim_vector_45length:              # @applyprim_vector_45length
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB68_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.82(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB68_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.82(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB68_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.82(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB68_4:
	leaq	-32(%rbp), %rdi
	callq	prim_vector_45length
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end68:
	.size	applyprim_vector_45length, .Lfunc_end68-applyprim_vector_45length
	.cfi_endproc
                                        # -- End function
	.globl	prim_vector_45length            # -- Begin function prim_vector_45length
	.p2align	4, 0x90
	.type	prim_vector_45length,@function
prim_vector_45length:                   # @prim_vector_45length
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.83(%rip), %rsi
	callq	unwrap_vector
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end69:
	.size	prim_vector_45length, .Lfunc_end69-prim_vector_45length
	.cfi_endproc
                                        # -- End function
	.globl	bounds_check                    # -- Begin function bounds_check
	.p2align	4, 0x90
	.type	bounds_check,@function
bounds_check:                           # @bounds_check
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.84(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	addq	$1, %rax
	cmpq	-24(%rbp), %rax
	jle	.LBB70_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movq	-16(%rbp), %rsi
	movq	-24(%rbp), %rdx
	leaq	.L.str.85(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB70_2:
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end70:
	.size	bounds_check, .Lfunc_end70-bounds_check
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_vector_45ref          # -- Begin function applyprim_vector_45ref
	.p2align	4, 0x90
	.type	applyprim_vector_45ref,@function
applyprim_vector_45ref:                 # @applyprim_vector_45ref
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB71_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.86(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB71_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.86(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB71_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.86(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB71_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.86(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB71_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.86(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB71_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim_vector_45ref
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end71:
	.size	applyprim_vector_45ref, .Lfunc_end71-applyprim_vector_45ref
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_vector_45set_33       # -- Begin function applyprim_vector_45set_33
	.p2align	4, 0x90
	.type	applyprim_vector_45set_33,@function
applyprim_vector_45set_33:              # @applyprim_vector_45set_33
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB72_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.89(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB72_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.89(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB72_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.90(%rip), %rdi
	leaq	.L.str.89(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB72_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.89(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$4, -80(%rbp)
	je	.LBB72_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.91(%rip), %rdi
	leaq	.L.str.89(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB72_6:
	leaq	-88(%rbp), %rdi
	leaq	.L.str.89(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -112(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -104(%rbp)
	movq	-96(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -128(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -120(%rbp)
	cmpl	$1, -120(%rbp)
	je	.LBB72_8
# %bb.7:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.92(%rip), %rdi
	leaq	.L.str.89(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB72_8:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	leaq	-112(%rbp), %rdx
	callq	prim_vector_45set_33
	addq	$128, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end72:
	.size	applyprim_vector_45set_33, .Lfunc_end72-applyprim_vector_45set_33
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_void                  # -- Begin function applyprim_void
	.p2align	4, 0x90
	.type	applyprim_void,@function
applyprim_void:                         # @applyprim_void
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$1, 8(%rax)
	je	.LBB73_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.95(%rip), %rdi
	leaq	.L.str.96(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB73_2:
	callq	prim_void
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end73:
	.size	applyprim_void, .Lfunc_end73-applyprim_void
	.cfi_endproc
                                        # -- End function
	.globl	prim_void                       # -- Begin function prim_void
	.p2align	4, 0x90
	.type	prim_void,@function
prim_void:                              # @prim_void
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	callq	const_init_void
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end74:
	.size	prim_void, .Lfunc_end74-prim_void
	.cfi_endproc
                                        # -- End function
	.globl	cons_eq_helper                  # -- Begin function cons_eq_helper
	.p2align	4, 0x90
	.type	cons_eq_helper,@function
cons_eq_helper:                         # @cons_eq_helper
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	-32(%rbp), %rsi
	leaq	-48(%rbp), %rdx
	callq	_get_both
	movq	-16(%rbp), %rdi
	leaq	-64(%rbp), %rsi
	leaq	-80(%rbp), %rdx
	callq	_get_both
	leaq	-32(%rbp), %rdi
	leaq	-64(%rbp), %rsi
	callq	eq_helper
	movl	%eax, %ecx
	xorl	%eax, %eax
                                        # kill: def $al killed $al killed $eax
	cmpl	$1, %ecx
	movb	%al, -81(%rbp)                  # 1-byte Spill
	jne	.LBB75_2
# %bb.1:
	leaq	-48(%rbp), %rdi
	leaq	-80(%rbp), %rsi
	callq	eq_helper
	cmpl	$0, %eax
	setne	%al
	movb	%al, -81(%rbp)                  # 1-byte Spill
.LBB75_2:
	movb	-81(%rbp), %al                  # 1-byte Reload
	andb	$1, %al
	movzbl	%al, %eax
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end75:
	.size	cons_eq_helper, .Lfunc_end75-cons_eq_helper
	.cfi_endproc
                                        # -- End function
	.globl	vec_eq_helper                   # -- Begin function vec_eq_helper
	.p2align	4, 0x90
	.type	vec_eq_helper,@function
vec_eq_helper:                          # @vec_eq_helper
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.97(%rip), %rsi
	callq	unwrap_vector
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	leaq	.L.str.98(%rip), %rsi
	callq	unwrap_vector
	movq	%rax, -40(%rbp)
	movq	-32(%rbp), %rdi
	leaq	.L.str.99(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -48(%rbp)
	movq	-40(%rbp), %rdi
	leaq	.L.str.100(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -56(%rbp)
	movq	-48(%rbp), %rax
	cmpq	-56(%rbp), %rax
	je	.LBB76_2
# %bb.1:
	movl	$0, -4(%rbp)
	jmp	.LBB76_9
.LBB76_2:
	movq	$0, -64(%rbp)
.LBB76_3:                               # =>This Inner Loop Header: Depth=1
	movq	-64(%rbp), %rax
	cmpq	-48(%rbp), %rax
	jg	.LBB76_8
# %bb.4:                                #   in Loop: Header=BB76_3 Depth=1
	movq	-32(%rbp), %rdi
	movq	-64(%rbp), %rax
	shlq	$4, %rax
	addq	%rax, %rdi
	movq	-40(%rbp), %rsi
	movq	-64(%rbp), %rax
	shlq	$4, %rax
	addq	%rax, %rsi
	callq	eq_helper
	cmpl	$0, %eax
	jne	.LBB76_6
# %bb.5:
	movl	$0, -4(%rbp)
	jmp	.LBB76_9
.LBB76_6:                               #   in Loop: Header=BB76_3 Depth=1
	jmp	.LBB76_7
.LBB76_7:                               #   in Loop: Header=BB76_3 Depth=1
	movq	-64(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -64(%rbp)
	jmp	.LBB76_3
.LBB76_8:
	movl	$1, -4(%rbp)
.LBB76_9:
	movl	-4(%rbp), %eax
	addq	$64, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end76:
	.size	vec_eq_helper, .Lfunc_end76-vec_eq_helper
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_eq_63                 # -- Begin function applyprim_eq_63
	.p2align	4, 0x90
	.type	applyprim_eq_63,@function
applyprim_eq_63:                        # @applyprim_eq_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB77_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.112(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB77_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.112(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB77_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.112(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB77_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.112(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB77_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.112(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB77_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim_eq_63
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end77:
	.size	applyprim_eq_63, .Lfunc_end77-applyprim_eq_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_eq_63                      # -- Begin function prim_eq_63
	.p2align	4, 0x90
	.type	prim_eq_63,@function
prim_eq_63:                             # @prim_eq_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	callq	eq_helper
	cmpl	$1, %eax
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end78:
	.size	prim_eq_63, .Lfunc_end78-prim_eq_63
	.cfi_endproc
                                        # -- End function
	.globl	make_predicate                  # -- Begin function make_predicate
	.p2align	4, 0x90
	.type	make_predicate,@function
make_predicate:                         # @make_predicate
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movb	%dil, %al
	andb	$1, %al
	movb	%al, -1(%rbp)
	testb	$1, -1(%rbp)
	je	.LBB79_2
# %bb.1:
	callq	const_init_true
	movq	%rax, -16(%rbp)                 # 8-byte Spill
	jmp	.LBB79_3
.LBB79_2:
	callq	const_init_false
	movq	%rax, -16(%rbp)                 # 8-byte Spill
.LBB79_3:
	movq	-16(%rbp), %rax                 # 8-byte Reload
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end79:
	.size	make_predicate, .Lfunc_end79-make_predicate
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_eqv_63                # -- Begin function applyprim_eqv_63
	.p2align	4, 0x90
	.type	applyprim_eqv_63,@function
applyprim_eqv_63:                       # @applyprim_eqv_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB80_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.113(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB80_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.113(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB80_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.113(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB80_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.113(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB80_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.113(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB80_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim_eqv_63
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end80:
	.size	applyprim_eqv_63, .Lfunc_end80-applyprim_eqv_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_eqv_63                     # -- Begin function prim_eqv_63
	.p2align	4, 0x90
	.type	prim_eqv_63,@function
prim_eqv_63:                            # @prim_eqv_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	callq	prim_eq_63
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end81:
	.size	prim_eqv_63, .Lfunc_end81-prim_eqv_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_equal_63              # -- Begin function applyprim_equal_63
	.p2align	4, 0x90
	.type	applyprim_equal_63,@function
applyprim_equal_63:                     # @applyprim_equal_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB82_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.114(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB82_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.114(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB82_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.114(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB82_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.114(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB82_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.114(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB82_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim_equal_63
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end82:
	.size	applyprim_equal_63, .Lfunc_end82-applyprim_equal_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_equal_63                   # -- Begin function prim_equal_63
	.p2align	4, 0x90
	.type	prim_equal_63,@function
prim_equal_63:                          # @prim_equal_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	callq	prim_eq_63
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end83:
	.size	prim_equal_63, .Lfunc_end83-prim_equal_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_number_63             # -- Begin function applyprim_number_63
	.p2align	4, 0x90
	.type	applyprim_number_63,@function
applyprim_number_63:                    # @applyprim_number_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB84_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.115(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB84_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.115(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB84_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.115(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB84_4:
	leaq	-32(%rbp), %rdi
	callq	prim_number_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end84:
	.size	applyprim_number_63, .Lfunc_end84-applyprim_number_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_number_63                  # -- Begin function prim_number_63
	.p2align	4, 0x90
	.type	prim_number_63,@function
prim_number_63:                         # @prim_number_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	prim_integer_63
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end85:
	.size	prim_number_63, .Lfunc_end85-prim_number_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_integer_63                 # -- Begin function prim_integer_63
	.p2align	4, 0x90
	.type	prim_integer_63,@function
prim_integer_63:                        # @prim_integer_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$5, 8(%rax)
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end86:
	.size	prim_integer_63, .Lfunc_end86-prim_integer_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_integer_63            # -- Begin function applyprim_integer_63
	.p2align	4, 0x90
	.type	applyprim_integer_63,@function
applyprim_integer_63:                   # @applyprim_integer_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB87_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.116(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB87_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.116(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB87_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.116(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB87_4:
	leaq	-32(%rbp), %rdi
	callq	prim_integer_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end87:
	.size	applyprim_integer_63, .Lfunc_end87-applyprim_integer_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_boolean_63            # -- Begin function applyprim_boolean_63
	.p2align	4, 0x90
	.type	applyprim_boolean_63,@function
applyprim_boolean_63:                   # @applyprim_boolean_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB88_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.117(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB88_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.117(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB88_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.117(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB88_4:
	leaq	-32(%rbp), %rdi
	callq	prim_boolean_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end88:
	.size	applyprim_boolean_63, .Lfunc_end88-applyprim_boolean_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_boolean_63                 # -- Begin function prim_boolean_63
	.p2align	4, 0x90
	.type	prim_boolean_63,@function
prim_boolean_63:                        # @prim_boolean_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$2, 8(%rax)
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end89:
	.size	prim_boolean_63, .Lfunc_end89-prim_boolean_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_void_63               # -- Begin function applyprim_void_63
	.p2align	4, 0x90
	.type	applyprim_void_63,@function
applyprim_void_63:                      # @applyprim_void_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB90_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.118(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB90_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.118(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB90_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.118(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB90_4:
	leaq	-32(%rbp), %rdi
	callq	prim_void_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end90:
	.size	applyprim_void_63, .Lfunc_end90-applyprim_void_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_void_63                    # -- Begin function prim_void_63
	.p2align	4, 0x90
	.type	prim_void_63,@function
prim_void_63:                           # @prim_void_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$0, 8(%rax)
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end91:
	.size	prim_void_63, .Lfunc_end91-prim_void_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_procedure_63          # -- Begin function applyprim_procedure_63
	.p2align	4, 0x90
	.type	applyprim_procedure_63,@function
applyprim_procedure_63:                 # @applyprim_procedure_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB92_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.119(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB92_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.119(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB92_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.119(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB92_4:
	leaq	-32(%rbp), %rdi
	callq	prim_procedure_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end92:
	.size	applyprim_procedure_63, .Lfunc_end92-applyprim_procedure_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_procedure_63               # -- Begin function prim_procedure_63
	.p2align	4, 0x90
	.type	prim_procedure_63,@function
prim_procedure_63:                      # @prim_procedure_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$3, 8(%rax)
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end93:
	.size	prim_procedure_63, .Lfunc_end93-prim_procedure_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_null_63               # -- Begin function applyprim_null_63
	.p2align	4, 0x90
	.type	applyprim_null_63,@function
applyprim_null_63:                      # @applyprim_null_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB94_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.120(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB94_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.120(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB94_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.120(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB94_4:
	leaq	-32(%rbp), %rdi
	callq	prim_null_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end94:
	.size	applyprim_null_63, .Lfunc_end94-applyprim_null_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_null_63                    # -- Begin function prim_null_63
	.p2align	4, 0x90
	.type	prim_null_63,@function
prim_null_63:                           # @prim_null_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$1, 8(%rax)
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end95:
	.size	prim_null_63, .Lfunc_end95-prim_null_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_cons_63               # -- Begin function applyprim_cons_63
	.p2align	4, 0x90
	.type	applyprim_cons_63,@function
applyprim_cons_63:                      # @applyprim_cons_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB96_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.121(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB96_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.121(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB96_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.121(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB96_4:
	leaq	-32(%rbp), %rdi
	callq	prim_cons_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end96:
	.size	applyprim_cons_63, .Lfunc_end96-applyprim_cons_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_cons_63                    # -- Begin function prim_cons_63
	.p2align	4, 0x90
	.type	prim_cons_63,@function
prim_cons_63:                           # @prim_cons_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end97:
	.size	prim_cons_63, .Lfunc_end97-prim_cons_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_cons                  # -- Begin function applyprim_cons
	.p2align	4, 0x90
	.type	applyprim_cons,@function
applyprim_cons:                         # @applyprim_cons
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB98_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.122(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB98_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.122(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB98_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.122(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB98_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.122(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB98_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.122(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB98_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim_cons
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end98:
	.size	applyprim_cons, .Lfunc_end98-applyprim_cons
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_cdr                   # -- Begin function applyprim_cdr
	.p2align	4, 0x90
	.type	applyprim_cdr,@function
applyprim_cdr:                          # @applyprim_cdr
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB99_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.123(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB99_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.123(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB99_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.123(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB99_4:
	leaq	-32(%rbp), %rdi
	callq	prim_cdr
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end99:
	.size	applyprim_cdr, .Lfunc_end99-applyprim_cdr
	.cfi_endproc
                                        # -- End function
	.globl	prim_cdr                        # -- Begin function prim_cdr
	.p2align	4, 0x90
	.type	prim_cdr,@function
prim_cdr:                               # @prim_cdr
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.124(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	addq	$16, %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end100:
	.size	prim_cdr, .Lfunc_end100-prim_cdr
	.cfi_endproc
                                        # -- End function
	.globl	prim__45                        # -- Begin function prim__45
	.p2align	4, 0x90
	.type	prim__45,@function
prim__45:                               # @prim__45
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.125(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.126(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	subq	-32(%rbp), %rdi
	callq	const_init_int
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end101:
	.size	prim__45, .Lfunc_end101-prim__45
	.cfi_endproc
                                        # -- End function
	.globl	applyprim__45                   # -- Begin function applyprim__45
	.p2align	4, 0x90
	.type	applyprim__45,@function
applyprim__45:                          # @applyprim__45
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.127(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -40(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -56(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -48(%rbp)
	leaq	-40(%rbp), %rdi
	leaq	.L.str.128(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -64(%rbp)
	cmpl	$1, -48(%rbp)
	jne	.LBB102_2
# %bb.1:
	xorl	%eax, %eax
	movl	%eax, %edi
	subq	-64(%rbp), %rdi
	callq	const_init_int
	movq	%rax, -8(%rbp)
	jmp	.LBB102_6
.LBB102_2:
	movq	-64(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-56(%rbp), %rax
	movq	%rax, -88(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -80(%rbp)
.LBB102_3:                              # =>This Inner Loop Header: Depth=1
	cmpl	$1, -80(%rbp)
	je	.LBB102_5
# %bb.4:                                #   in Loop: Header=BB102_3 Depth=1
	leaq	-88(%rbp), %rdi
	leaq	-104(%rbp), %rsi
	leaq	-120(%rbp), %rdx
	callq	_get_both
	leaq	-104(%rbp), %rdi
	leaq	.L.str.129(%rip), %rsi
	callq	unwrap_int
	movq	%rax, %rcx
	movq	-72(%rbp), %rax
	subq	%rcx, %rax
	movq	%rax, -72(%rbp)
	movq	-120(%rbp), %rax
	movq	%rax, -88(%rbp)
	movq	-112(%rbp), %rax
	movq	%rax, -80(%rbp)
	jmp	.LBB102_3
.LBB102_5:
	movq	-72(%rbp), %rdi
	callq	const_init_int
	movq	%rax, -8(%rbp)
.LBB102_6:
	movq	-8(%rbp), %rax
	addq	$128, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end102:
	.size	applyprim__45, .Lfunc_end102-applyprim__45
	.cfi_endproc
                                        # -- End function
	.globl	prim__42                        # -- Begin function prim__42
	.p2align	4, 0x90
	.type	prim__42,@function
prim__42:                               # @prim__42
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.130(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)                 # 8-byte Spill
	movq	-16(%rbp), %rdi
	leaq	.L.str.131(%rip), %rsi
	callq	unwrap_int
	movq	-24(%rbp), %rdi                 # 8-byte Reload
	imulq	%rax, %rdi
	callq	const_init_int
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end103:
	.size	prim__42, .Lfunc_end103-prim__42
	.cfi_endproc
                                        # -- End function
	.globl	applyprim__42                   # -- Begin function applyprim__42
	.p2align	4, 0x90
	.type	applyprim__42,@function
applyprim__42:                          # @applyprim__42
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rax
	cmpl	$1, 8(%rax)
	jne	.LBB104_2
# %bb.1:
	movl	$1, %edi
	callq	const_init_int
	movq	%rax, -8(%rbp)
	jmp	.LBB104_5
.LBB104_2:
	movq	-16(%rbp), %rax
	cmpl	$4, 8(%rax)
	jne	.LBB104_4
# %bb.3:
	movq	-16(%rbp), %rdi
	leaq	.L.str.132(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -40(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -56(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -48(%rbp)
	leaq	-40(%rbp), %rdi
	leaq	.L.str.133(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -64(%rbp)
	leaq	-56(%rbp), %rdi
	callq	applyprim__42
	movq	%rax, %rdi
	leaq	.L.str.134(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -72(%rbp)
	movq	-64(%rbp), %rdi
	imulq	-72(%rbp), %rdi
	callq	const_init_int
	movq	%rax, -8(%rbp)
	jmp	.LBB104_5
.LBB104_4:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.135(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB104_5:
	movq	-8(%rbp), %rax
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end104:
	.size	applyprim__42, .Lfunc_end104-applyprim__42
	.cfi_endproc
                                        # -- End function
	.globl	prim__47                        # -- Begin function prim__47
	.p2align	4, 0x90
	.type	prim__47,@function
prim__47:                               # @prim__47
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.136(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.137(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rax
	cqto
	idivq	-32(%rbp)
	movq	%rax, %rdi
	callq	const_init_int
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end105:
	.size	prim__47, .Lfunc_end105-prim__47
	.cfi_endproc
                                        # -- End function
	.globl	applyprim__61                   # -- Begin function applyprim__61
	.p2align	4, 0x90
	.type	applyprim__61,@function
applyprim__61:                          # @applyprim__61
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB106_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.138(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB106_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.138(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB106_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.138(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB106_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.138(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB106_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.138(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB106_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim__61
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end106:
	.size	applyprim__61, .Lfunc_end106-applyprim__61
	.cfi_endproc
                                        # -- End function
	.globl	prim__61                        # -- Begin function prim__61
	.p2align	4, 0x90
	.type	prim__61,@function
prim__61:                               # @prim__61
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.139(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -32(%rbp)                 # 8-byte Spill
	movq	-24(%rbp), %rdi
	leaq	.L.str.140(%rip), %rsi
	callq	unwrap_int
	movq	%rax, %rcx
	movq	-32(%rbp), %rax                 # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB107_2
# %bb.1:
	callq	const_init_true
	movq	%rax, -8(%rbp)
	jmp	.LBB107_3
.LBB107_2:
	callq	const_init_false
	movq	%rax, -8(%rbp)
.LBB107_3:
	movq	-8(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end107:
	.size	prim__61, .Lfunc_end107-prim__61
	.cfi_endproc
                                        # -- End function
	.globl	prim__60                        # -- Begin function prim__60
	.p2align	4, 0x90
	.type	prim__60,@function
prim__60:                               # @prim__60
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.141(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)                 # 8-byte Spill
	movq	-16(%rbp), %rdi
	leaq	.L.str.142(%rip), %rsi
	callq	unwrap_int
	movq	%rax, %rcx
	movq	-24(%rbp), %rax                 # 8-byte Reload
	cmpq	%rcx, %rax
	setl	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end108:
	.size	prim__60, .Lfunc_end108-prim__60
	.cfi_endproc
                                        # -- End function
	.globl	applyprim__60_61                # -- Begin function applyprim__60_61
	.p2align	4, 0x90
	.type	applyprim__60_61,@function
applyprim__60_61:                       # @applyprim__60_61
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB109_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.143(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB109_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.143(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB109_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.143(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB109_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.143(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB109_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.143(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB109_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim__60_61
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end109:
	.size	applyprim__60_61, .Lfunc_end109-applyprim__60_61
	.cfi_endproc
                                        # -- End function
	.globl	prim__60_61                     # -- Begin function prim__60_61
	.p2align	4, 0x90
	.type	prim__60_61,@function
prim__60_61:                            # @prim__60_61
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.144(%rip), %rsi
	callq	unwrap_int
	movq	%rax, -24(%rbp)                 # 8-byte Spill
	movq	-16(%rbp), %rdi
	leaq	.L.str.145(%rip), %rsi
	callq	unwrap_int
	movq	%rax, %rcx
	movq	-24(%rbp), %rax                 # 8-byte Reload
	cmpq	%rcx, %rax
	setle	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end110:
	.size	prim__60_61, .Lfunc_end110-prim__60_61
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_not                   # -- Begin function applyprim_not
	.p2align	4, 0x90
	.type	applyprim_not,@function
applyprim_not:                          # @applyprim_not
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB111_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.146(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB111_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.146(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB111_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.146(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB111_4:
	leaq	-32(%rbp), %rdi
	callq	prim_not
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end111:
	.size	applyprim_not, .Lfunc_end111-applyprim_not
	.cfi_endproc
                                        # -- End function
	.globl	prim_not                        # -- Begin function prim_not
	.p2align	4, 0x90
	.type	prim_not,@function
prim_not:                               # @prim_not
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.147(%rip), %rsi
	callq	unwrap_bool
	cmpq	$0, %rax
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end112:
	.size	prim_not, .Lfunc_end112-prim_not
	.cfi_endproc
                                        # -- End function
	.globl	map_to_sin                      # -- Begin function map_to_sin
	.p2align	4, 0x90
	.type	map_to_sin,@function
map_to_sin:                             # @map_to_sin
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movl	$9, 8(%rax)
	movq	-8(%rbp), %rcx
	movq	-16(%rbp), %rax
	movq	%rcx, (%rax)
	movq	-16(%rbp), %rax
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end113:
	.size	map_to_sin, .Lfunc_end113-map_to_sin
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_hash_45has_45key_63   # -- Begin function applyprim_hash_45has_45key_63
	.p2align	4, 0x90
	.type	applyprim_hash_45has_45key_63,@function
applyprim_hash_45has_45key_63:          # @applyprim_hash_45has_45key_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$96, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB114_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.148(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB114_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.148(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB114_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.21(%rip), %rdi
	leaq	.L.str.148(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB114_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.148(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$1, -80(%rbp)
	je	.LBB114_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.22(%rip), %rdi
	leaq	.L.str.148(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB114_6:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	callq	prim_hash_45has_45key_63
	addq	$96, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end114:
	.size	applyprim_hash_45has_45key_63, .Lfunc_end114-applyprim_hash_45has_45key_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_hash_45has_45key_63        # -- Begin function prim_hash_45has_45key_63
	.p2align	4, 0x90
	.type	prim_hash_45has_45key_63,@function
prim_hash_45has_45key_63:               # @prim_hash_45has_45key_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.149(%rip), %rsi
	callq	unwrap_hash
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	movq	-16(%rbp), %rsi
	callq	map_has_key
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end115:
	.size	prim_hash_45has_45key_63, .Lfunc_end115-prim_hash_45has_45key_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_hash                  # -- Begin function applyprim_hash
	.p2align	4, 0x90
	.type	applyprim_hash,@function
applyprim_hash:                         # @applyprim_hash
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	$0, -16(%rbp)
.LBB116_1:                              # =>This Inner Loop Header: Depth=1
	movq	-8(%rbp), %rax
	cmpl	$1, 8(%rax)
	je	.LBB116_5
# %bb.2:                                #   in Loop: Header=BB116_1 Depth=1
	movl	$1, %edi
	callq	alloc
	movq	%rax, -24(%rbp)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -32(%rbp)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -40(%rbp)
	movl	$1, %edi
	callq	alloc
	movq	%rax, -48(%rbp)
	movq	-8(%rbp), %rdi
	movq	-24(%rbp), %rsi
	movq	-32(%rbp), %rdx
	callq	_get_both
	movq	-32(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB116_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.150(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB116_4:                              #   in Loop: Header=BB116_1 Depth=1
	movq	-32(%rbp), %rdi
	movq	-40(%rbp), %rsi
	movq	-48(%rbp), %rdx
	callq	_get_both
	movq	-16(%rbp), %rdi
	movq	-24(%rbp), %rsi
	movq	-40(%rbp), %rdx
	callq	map_insert
	movq	%rax, -16(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB116_1
.LBB116_5:
	movq	-16(%rbp), %rdi
	callq	map_to_sin
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end116:
	.size	applyprim_hash, .Lfunc_end116-applyprim_hash
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_hash_45keys           # -- Begin function applyprim_hash_45keys
	.p2align	4, 0x90
	.type	applyprim_hash_45keys,@function
applyprim_hash_45keys:                  # @applyprim_hash_45keys
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB117_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.151(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB117_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.151(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB117_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.151(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB117_4:
	leaq	-32(%rbp), %rdi
	callq	prim_hash_45keys
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end117:
	.size	applyprim_hash_45keys, .Lfunc_end117-applyprim_hash_45keys
	.cfi_endproc
                                        # -- End function
	.globl	prim_hash_45keys                # -- Begin function prim_hash_45keys
	.p2align	4, 0x90
	.type	prim_hash_45keys,@function
prim_hash_45keys:                       # @prim_hash_45keys
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.152(%rip), %rsi
	callq	unwrap_hash
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rdi
	callq	map_keys
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end118:
	.size	prim_hash_45keys, .Lfunc_end118-prim_hash_45keys
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_hash_45ref            # -- Begin function applyprim_hash_45ref
	.p2align	4, 0x90
	.type	applyprim_hash_45ref,@function
applyprim_hash_45ref:                   # @applyprim_hash_45ref
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$144, %rsp
	movq	%rdi, -16(%rbp)
	movq	$0, -24(%rbp)
	movq	$0, -32(%rbp)
	movq	$0, -40(%rbp)
	movq	-16(%rbp), %rax
	cmpl	$4, 8(%rax)
	jne	.LBB119_11
# %bb.1:
	movq	-16(%rbp), %rdi
	leaq	-56(%rbp), %rsi
	leaq	-72(%rbp), %rdx
	callq	_get_both
	leaq	-56(%rbp), %rax
	movq	%rax, -24(%rbp)
	cmpl	$4, -64(%rbp)
	jne	.LBB119_10
# %bb.2:
	leaq	-72(%rbp), %rdi
	leaq	-88(%rbp), %rsi
	leaq	-104(%rbp), %rdx
	callq	_get_both
	leaq	-88(%rbp), %rax
	movq	%rax, -32(%rbp)
	cmpl	$4, -96(%rbp)
	jne	.LBB119_6
# %bb.3:
	leaq	-104(%rbp), %rdi
	leaq	-120(%rbp), %rsi
	leaq	-136(%rbp), %rdx
	callq	_get_both
	leaq	-120(%rbp), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -128(%rbp)
	jne	.LBB119_5
# %bb.4:
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %rsi
	movq	-40(%rbp), %rdx
	callq	hash_ref_impl
	movq	%rax, -8(%rbp)
	jmp	.LBB119_12
.LBB119_5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.153(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB119_6:
	cmpl	$1, -96(%rbp)
	jne	.LBB119_8
# %bb.7:
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %rsi
	movq	-40(%rbp), %rdx
	callq	hash_ref_impl
	movq	%rax, -8(%rbp)
	jmp	.LBB119_12
.LBB119_8:
	jmp	.LBB119_9
.LBB119_9:
	jmp	.LBB119_10
.LBB119_10:
	jmp	.LBB119_11
.LBB119_11:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.154(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB119_12:
	movq	-8(%rbp), %rax
	addq	$144, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end119:
	.size	applyprim_hash_45ref, .Lfunc_end119-applyprim_hash_45ref
	.cfi_endproc
                                        # -- End function
	.globl	hash_ref_impl                   # -- Begin function hash_ref_impl
	.p2align	4, 0x90
	.type	hash_ref_impl,@function
hash_ref_impl:                          # @hash_ref_impl
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	-16(%rbp), %rdi
	leaq	.L.str.155(%rip), %rsi
	callq	unwrap_hash
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdi
	movq	-24(%rbp), %rsi
	callq	map_get
	movq	%rax, -48(%rbp)
	cmpq	$0, -48(%rbp)
	jne	.LBB120_4
# %bb.1:
	cmpq	$0, -32(%rbp)
	jne	.LBB120_3
# %bb.2:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.156(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB120_3:
	movq	-32(%rbp), %rax
	movq	%rax, -8(%rbp)
	jmp	.LBB120_5
.LBB120_4:
	movq	-48(%rbp), %rax
	movq	%rax, -8(%rbp)
.LBB120_5:
	movq	-8(%rbp), %rax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end120:
	.size	hash_ref_impl, .Lfunc_end120-hash_ref_impl
	.cfi_endproc
                                        # -- End function
	.globl	prim_hash_45ref                 # -- Begin function prim_hash_45ref
	.p2align	4, 0x90
	.type	prim_hash_45ref,@function
prim_hash_45ref:                        # @prim_hash_45ref
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	xorl	%eax, %eax
	movl	%eax, %edx
	callq	hash_ref_impl
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end121:
	.size	prim_hash_45ref, .Lfunc_end121-prim_hash_45ref
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_hash_45set            # -- Begin function applyprim_hash_45set
	.p2align	4, 0x90
	.type	applyprim_hash_45set,@function
applyprim_hash_45set:                   # @applyprim_hash_45set
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB122_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.157(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB122_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.157(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$4, -40(%rbp)
	je	.LBB122_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.90(%rip), %rdi
	leaq	.L.str.157(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB122_4:
	leaq	-48(%rbp), %rdi
	leaq	.L.str.157(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -88(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -80(%rbp)
	cmpl	$4, -80(%rbp)
	je	.LBB122_6
# %bb.5:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.91(%rip), %rdi
	leaq	.L.str.157(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB122_6:
	leaq	-88(%rbp), %rdi
	leaq	.L.str.157(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -112(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -104(%rbp)
	movq	-96(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -128(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -120(%rbp)
	cmpl	$1, -120(%rbp)
	je	.LBB122_8
# %bb.7:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.92(%rip), %rdi
	leaq	.L.str.157(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB122_8:
	leaq	-32(%rbp), %rdi
	leaq	-72(%rbp), %rsi
	leaq	-112(%rbp), %rdx
	callq	prim_hash_45set
	addq	$128, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end122:
	.size	applyprim_hash_45set, .Lfunc_end122-applyprim_hash_45set
	.cfi_endproc
                                        # -- End function
	.globl	prim_hash_45set                 # -- Begin function prim_hash_45set
	.p2align	4, 0x90
	.type	prim_hash_45set,@function
prim_hash_45set:                        # @prim_hash_45set
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.158(%rip), %rsi
	callq	unwrap_hash
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	movq	-16(%rbp), %rsi
	movq	-24(%rbp), %rdx
	callq	map_insert
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdi
	callq	map_to_sin
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end123:
	.size	prim_hash_45set, .Lfunc_end123-prim_hash_45set
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_hash_63               # -- Begin function applyprim_hash_63
	.p2align	4, 0x90
	.type	applyprim_hash_63,@function
applyprim_hash_63:                      # @applyprim_hash_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB124_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.159(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB124_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.159(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB124_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.159(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB124_4:
	leaq	-32(%rbp), %rdi
	callq	prim_hash_63
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end124:
	.size	applyprim_hash_63, .Lfunc_end124-applyprim_hash_63
	.cfi_endproc
                                        # -- End function
	.globl	prim_hash_63                    # -- Begin function prim_hash_63
	.p2align	4, 0x90
	.type	prim_hash_63,@function
prim_hash_63:                           # @prim_hash_63
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$9, 8(%rax)
	sete	%al
	movzbl	%al, %edi
	andl	$1, %edi
	callq	make_predicate
	addq	$16, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end125:
	.size	prim_hash_63, .Lfunc_end125-prim_hash_63
	.cfi_endproc
                                        # -- End function
	.globl	applyprim_hash_45count          # -- Begin function applyprim_hash_45count
	.p2align	4, 0x90
	.type	applyprim_hash_45count,@function
applyprim_hash_45count:                 # @applyprim_hash_45count
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	cmpl	$4, 8(%rax)
	je	.LBB126_2
# %bb.1:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.12(%rip), %rdi
	leaq	.L.str.160(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB126_2:
	movq	-8(%rbp), %rdi
	leaq	.L.str.160(%rip), %rsi
	callq	unwrap_cons
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -32(%rbp)
	movq	8(%rax), %rax
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	24(%rax), %rax
	movq	%rax, -40(%rbp)
	cmpl	$1, -40(%rbp)
	je	.LBB126_4
# %bb.3:
	leaq	.L.str.11(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.14(%rip), %rdi
	leaq	.L.str.160(%rip), %rsi
	movb	$0, %al
	callq	printf@PLT
	leaq	.L.str.4(%rip), %rdi
	movb	$0, %al
	callq	printf@PLT
	movl	$1, %edi
	callq	exit@PLT
.LBB126_4:
	leaq	-32(%rbp), %rdi
	callq	prim_hash_45count
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end126:
	.size	applyprim_hash_45count, .Lfunc_end126-applyprim_hash_45count
	.cfi_endproc
                                        # -- End function
	.globl	prim_hash_45count               # -- Begin function prim_hash_45count
	.p2align	4, 0x90
	.type	prim_hash_45count,@function
prim_hash_45count:                      # @prim_hash_45count
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	leaq	.L.str.161(%rip), %rsi
	callq	unwrap_hash
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rdi
	callq	map_count
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	callq	const_init_int
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end127:
	.size	prim_hash_45count, .Lfunc_end127-prim_hash_45count
	.cfi_endproc
                                        # -- End function
	.globl	__underflow_handler             # -- Begin function __underflow_handler
	.p2align	4, 0x90
	.type	__underflow_handler,@function
__underflow_handler:                    # @__underflow_handler
	.cfi_startproc
# %bb.0:
	movq	srr@GOTPCREL(%rip), %rdi
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	spr@GOTPCREL(%rip), %rdx
	callq	handle_underflow
	callq	*%rax
.Lfunc_end128:
	.size	__underflow_handler, .Lfunc_end128-__underflow_handler
	.cfi_endproc
                                        # -- End function
	.globl	__continuation_function_handler # -- Begin function __continuation_function_handler
	.p2align	4, 0x90
	.type	__continuation_function_handler,@function
__continuation_function_handler:        # @__continuation_function_handler
	.cfi_startproc
# %bb.0:
	movq	srr@GOTPCREL(%rip), %rdi
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	spr@GOTPCREL(%rip), %rdx
	movq	retr@GOTPCREL(%rip), %rcx
	callq	handle_continuation_function
	callq	*%rax
.Lfunc_end129:
	.size	__continuation_function_handler, .Lfunc_end129-__continuation_function_handler
	.cfi_endproc
                                        # -- End function
	.globl	__program_finished              # -- Begin function __program_finished
	.p2align	4, 0x90
	.type	__program_finished,@function
__program_finished:                     # @__program_finished
	.cfi_startproc
# %bb.0:
	movq	retr@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	callq	prim_halt
.Lfunc_end130:
	.size	__program_finished, .Lfunc_end130-__program_finished
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	srr@GOTPCREL(%rip), %rdi
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	spr@GOTPCREL(%rip), %rdx
	callq	start_runtime
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	__program_finished@GOTPCREL(%rip), %rcx
	movq	%rcx, (%rax)
	callq	__main@PLT
.Lfunc_end131:
	.size	main, .Lfunc_end131-main
	.cfi_endproc
                                        # -- End function
	.globl	gotosub9824                     # -- Begin function gotosub9824
	.p2align	4, 0x90
	.type	gotosub9824,@function
gotosub9824:                            # @gotosub9824
	.cfi_startproc
# %bb.0:
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rsp)                    # 8-byte Spill
	movq	fpr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rdx
	movq	8(%rdx), %rcx
	imulq	$-1, %rcx, %rcx
	shlq	$3, %rcx
	addq	%rcx, %rdx
	movq	fpr@GOTPCREL(%rip), %rcx
	movq	%rdx, (%rcx)
	movq	retr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, 160(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 168(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 176(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 184(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 192(%rax)
	movq	112(%rax), %rdi
	movq	192(%rax), %rsi
	callq	prim__43
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 200(%rax)
	movq	200(%rax), %rcx
	movq	retr@GOTPCREL(%rip), %rax
	movq	%rcx, (%rax)
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	callq	*(%rax)
.Lfunc_end132:
	.size	gotosub9824, .Lfunc_end132-gotosub9824
	.cfi_endproc
                                        # -- End function
	.globl	gotosub9823                     # -- Begin function gotosub9823
	.p2align	4, 0x90
	.type	gotosub9823,@function
gotosub9823:                            # @gotosub9823
	.cfi_startproc
# %bb.0:
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rax, %rcx
	addq	$104, %rcx
	movq	%rcx, 8(%rsp)                   # 8-byte Spill
	movq	%rax, %rcx
	addq	$144, %rcx
	movq	%rcx, 16(%rsp)                  # 8-byte Spill
	movq	fpr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rdx
	movq	8(%rdx), %rcx
	imulq	$-1, %rcx, %rcx
	shlq	$3, %rcx
	addq	%rcx, %rdx
	movq	fpr@GOTPCREL(%rip), %rcx
	movq	%rdx, (%rcx)
	movq	retr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, 112(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 120(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 128(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 136(%rax)
	xorl	%eax, %eax
	movl	%eax, %edi
	movq	helper@GOTPCREL(%rip), %rsi
	callq	closure_alloc
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 144(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 152(%rax)
	movq	__main@GOTPCREL(%rip), %rax
	movq	-8(%rax), %rdx
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	movq	fpr@GOTPCREL(%rip), %rdi
	movq	spr@GOTPCREL(%rip), %rsi
	callq	check_for_overflow
	testb	$1, %al
	jne	.LBB133_2
# %bb.1:                                # %direct_cloapp9871_overflowcheck_overflow_first
	movq	32(%rsp), %r8                   # 8-byte Reload
	movq	srr@GOTPCREL(%rip), %rdi
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	spr@GOTPCREL(%rip), %rdx
	movq	__underflow_handler@GOTPCREL(%rip), %rcx
	callq	handle_overflow
.LBB133_2:                              # %direct_cloapp9871_overflowcheck_does_have_space
	movq	8(%rsp), %rax                   # 8-byte Reload
	movq	16(%rsp), %rcx                  # 8-byte Reload
	movq	(%rcx), %rdi
	movq	(%rax), %rdx
	movq	__main@GOTPCREL(%rip), %rax
	movq	-8(%rax), %rcx
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rcx, %rsi
	shlq	$3, %rsi
	movq	%rax, %r8
	addq	%rsi, %r8
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	%r8, (%rsi)
	movq	gotosub9824@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax,%rcx,8)
	movq	%rcx, 8(%rax,%rcx,8)
	movq	%rdi, 16(%rax,%rcx,8)
	movq	%rdx, 24(%rax,%rcx,8)
	callq	closure_get_fn_part
	callq	*%rax
.Lfunc_end133:
	.size	gotosub9823, .Lfunc_end133-gotosub9823
	.cfi_endproc
                                        # -- End function
	.globl	__main                          # -- Begin function __main
	.p2align	4, 0x90
	.type	__main,@function
	.quad	24                              # @__main
                                        # 0x18
__main:
	.cfi_startproc
# %bb.0:
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rax, %rcx
	addq	$96, %rcx
	movq	%rcx, 8(%rsp)                   # 8-byte Spill
	addq	$104, %rax
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 32(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 40(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 48(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 56(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 64(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 72(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 80(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 88(%rax)
	xorl	%eax, %eax
	movl	%eax, %edi
	movq	callee@GOTPCREL(%rip), %rsi
	callq	closure_alloc
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 96(%rax)
	callq	const_init_null
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 104(%rax)
	movq	__main@GOTPCREL(%rip), %rax
	movq	-8(%rax), %rdx
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	movq	fpr@GOTPCREL(%rip), %rdi
	movq	spr@GOTPCREL(%rip), %rsi
	callq	check_for_overflow
	testb	$1, %al
	jne	.LBB134_2
# %bb.1:                                # %direct_cloapp9857_overflowcheck_overflow_first
	movq	32(%rsp), %r8                   # 8-byte Reload
	movq	srr@GOTPCREL(%rip), %rdi
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	spr@GOTPCREL(%rip), %rdx
	movq	__underflow_handler@GOTPCREL(%rip), %rcx
	callq	handle_overflow
.LBB134_2:                              # %direct_cloapp9857_overflowcheck_does_have_space
	movq	16(%rsp), %rax                  # 8-byte Reload
	movq	8(%rsp), %rcx                   # 8-byte Reload
	movq	(%rcx), %rdi
	movq	(%rax), %rdx
	movq	__main@GOTPCREL(%rip), %rax
	movq	-8(%rax), %rcx
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rcx, %rsi
	shlq	$3, %rsi
	movq	%rax, %r8
	addq	%rsi, %r8
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	%r8, (%rsi)
	movq	gotosub9823@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax,%rcx,8)
	movq	%rcx, 8(%rax,%rcx,8)
	movq	%rdi, 16(%rax,%rcx,8)
	movq	%rdx, 24(%rax,%rcx,8)
	callq	closure_get_fn_part
	callq	*%rax
.Lfunc_end134:
	.size	__main, .Lfunc_end134-__main
	.cfi_endproc
                                        # -- End function
	.globl	gotosub9825                     # -- Begin function gotosub9825
	.p2align	4, 0x90
	.type	gotosub9825,@function
gotosub9825:                            # @gotosub9825
	.cfi_startproc
# %bb.0:
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rsp)                    # 8-byte Spill
	movq	fpr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rdx
	movq	8(%rdx), %rcx
	imulq	$-1, %rcx, %rcx
	shlq	$3, %rcx
	addq	%rcx, %rdx
	movq	fpr@GOTPCREL(%rip), %rcx
	movq	%rdx, (%rcx)
	movq	retr@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, 112(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 120(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 128(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 136(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 144(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 152(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 160(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 168(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 176(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 184(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 192(%rax)
	movq	112(%rax), %rdi
	movq	192(%rax), %rsi
	callq	prim__43
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 200(%rax)
	movq	200(%rax), %rcx
	movq	retr@GOTPCREL(%rip), %rax
	movq	%rcx, (%rax)
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	callq	*(%rax)
.Lfunc_end135:
	.size	gotosub9825, .Lfunc_end135-gotosub9825
	.cfi_endproc
                                        # -- End function
	.globl	callee                          # -- Begin function callee
	.p2align	4, 0x90
	.type	callee,@function
	.quad	24                              # @callee
                                        # 0x18
callee:
	.cfi_startproc
# %bb.0:
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, 24(%rsp)                  # 8-byte Spill
	movq	%rax, %rcx
	addq	$96, %rcx
	movq	%rcx, 8(%rsp)                   # 8-byte Spill
	addq	$104, %rax
	movq	%rax, 16(%rsp)                  # 8-byte Spill
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 32(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 40(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 48(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 56(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 64(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 72(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 80(%rax)
	movl	$5, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 88(%rax)
	xorl	%eax, %eax
	movl	%eax, %edi
	movq	helper@GOTPCREL(%rip), %rsi
	callq	closure_alloc
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 96(%rax)
	callq	const_init_null
	movq	%rax, %rcx
	movq	24(%rsp), %rax                  # 8-byte Reload
	movq	%rcx, 104(%rax)
	movq	callee@GOTPCREL(%rip), %rax
	movq	-8(%rax), %rdx
	movq	%rdx, 32(%rsp)                  # 8-byte Spill
	movq	fpr@GOTPCREL(%rip), %rdi
	movq	spr@GOTPCREL(%rip), %rsi
	callq	check_for_overflow
	testb	$1, %al
	jne	.LBB136_2
# %bb.1:                                # %direct_cloapp9830_overflowcheck_overflow_first
	movq	32(%rsp), %r8                   # 8-byte Reload
	movq	srr@GOTPCREL(%rip), %rdi
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	spr@GOTPCREL(%rip), %rdx
	movq	__underflow_handler@GOTPCREL(%rip), %rcx
	callq	handle_overflow
.LBB136_2:                              # %direct_cloapp9830_overflowcheck_does_have_space
	movq	16(%rsp), %rax                  # 8-byte Reload
	movq	8(%rsp), %rcx                   # 8-byte Reload
	movq	(%rcx), %rdi
	movq	(%rax), %rdx
	movq	callee@GOTPCREL(%rip), %rax
	movq	-8(%rax), %rcx
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rcx, %rsi
	shlq	$3, %rsi
	movq	%rax, %r8
	addq	%rsi, %r8
	movq	fpr@GOTPCREL(%rip), %rsi
	movq	%r8, (%rsi)
	movq	gotosub9825@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax,%rcx,8)
	movq	%rcx, 8(%rax,%rcx,8)
	movq	%rdi, 16(%rax,%rcx,8)
	movq	%rdx, 24(%rax,%rcx,8)
	callq	closure_get_fn_part
	callq	*%rax
.Lfunc_end136:
	.size	callee, .Lfunc_end136-callee
	.cfi_endproc
                                        # -- End function
	.globl	helper                          # -- Begin function helper
	.p2align	4, 0x90
	.type	helper,@function
	.quad	3                               # @helper
                                        # 0x3
helper:
	.cfi_startproc
# %bb.0:
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, (%rsp)                    # 8-byte Spill
	movl	$12, %edi
	callq	const_init_int
	movq	%rax, %rcx
	movq	(%rsp), %rax                    # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	32(%rax), %rcx
	movq	retr@GOTPCREL(%rip), %rax
	movq	%rcx, (%rax)
	movq	fpr@GOTPCREL(%rip), %rax
	movq	(%rax), %rax
	callq	*(%rax)
.Lfunc_end137:
	.size	helper, .Lfunc_end137-helper
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object                  # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Debug number: %ld\n"
	.size	.L.str, 19

	.type	.L.str.1,@object                # @.str.1
.L.str.1:
	.asciz	"Register Values:\n"
	.size	.L.str.1, 18

	.type	.L.str.2,@object                # @.str.2
.L.str.2:
	.asciz	"srr:`%p`,fpr:`%p`,spr:`%p`,retr:`%p`\n"
	.size	.L.str.2, 38

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"Current @retr: "
	.size	.L.str.3, 16

	.type	.L.str.4,@object                # @.str.4
.L.str.4:
	.asciz	"\n"
	.size	.L.str.4, 2

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"_get_both"
	.size	.L.str.5, 10

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"closure_place_freevar"
	.size	.L.str.6, 22

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"closure_get_env_part"
	.size	.L.str.7, 21

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"closure_get_fn_part"
	.size	.L.str.8, 20

	.type	.L.str.9,@object                # @.str.9
.L.str.9:
	.asciz	"descent == full->stack"
	.size	.L.str.9, 23

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"./src/cpp/runtime.cpp"
	.size	.L.str.10, 22

	.type	.L__PRETTY_FUNCTION__.handle_overflow,@object # @__PRETTY_FUNCTION__.handle_overflow
.L__PRETTY_FUNCTION__.handle_overflow:
	.asciz	"void handle_overflow(SinRecord **, SinObj ***, SinObj ***, SinFunc, s64)"
	.size	.L__PRETTY_FUNCTION__.handle_overflow, 73

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"Fatal library run-time error: "
	.size	.L.str.11, 31

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"Expected cons but got something else for function '%s'"
	.size	.L.str.12, 55

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"applyprim_car"
	.size	.L.str.13, 14

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"function '%s' only takes 1 argument."
	.size	.L.str.14, 37

	.type	.L.str.15,@object               # @.str.15
.L.str.15:
	.asciz	"car"
	.size	.L.str.15, 4

	.type	.L.str.16,@object               # @.str.16
.L.str.16:
	.asciz	"+ a"
	.size	.L.str.16, 4

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"+ b"
	.size	.L.str.17, 4

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"Expected Cons in apply +, but got %s"
	.size	.L.str.18, 37

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"apply + final"
	.size	.L.str.19, 14

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"applyprim_make_45vector"
	.size	.L.str.20, 24

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"Function '%s' expected 2 arguments but got 1."
	.size	.L.str.21, 46

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"Function '%s' only takes 2 arguments."
	.size	.L.str.22, 38

	.type	.L.str.23,@object               # @.str.23
.L.str.23:
	.asciz	"make-vector"
	.size	.L.str.23, 12

	.type	.L.str.24,@object               # @.str.24
.L.str.24:
	.asciz	"closure_env_get"
	.size	.L.str.24, 16

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"unwrap_hash takes a Hash object! Got %d in fn %s"
	.size	.L.str.25, 49

	.type	.L.str.26,@object               # @.str.26
.L.str.26:
	.asciz	"unwrap_cons takes a Cons object! Got %d in fn %s"
	.size	.L.str.26, 49

	.type	.L.str.27,@object               # @.str.27
.L.str.27:
	.asciz	"unwrap_vector takes a Vector object! Got %d in fn %s"
	.size	.L.str.27, 53

	.type	.L.str.28,@object               # @.str.28
.L.str.28:
	.asciz	"unwrap_clo takes a Closure object! Got %d in fn %s"
	.size	.L.str.28, 51

	.type	.L.str.29,@object               # @.str.29
.L.str.29:
	.asciz	"unwrap_int takes an Int object! Got %d in fn %s"
	.size	.L.str.29, 48

	.type	.L.str.30,@object               # @.str.30
.L.str.30:
	.asciz	"unwrap_bool takes a Bool object! Got %d in fn %s"
	.size	.L.str.30, 49

	.type	.L.str.31,@object               # @.str.31
.L.str.31:
	.asciz	"unwrap_str takes a Str object! Got %d in fn %s"
	.size	.L.str.31, 47

	.type	.L.str.32,@object               # @.str.32
.L.str.32:
	.asciz	"unwrap_sym takes a Sym object! Got %d in fn %s"
	.size	.L.str.32, 47

	.type	.L.str.33,@object               # @.str.33
.L.str.33:
	.asciz	"is_truthy_value"
	.size	.L.str.33, 16

	.type	.L.str.34,@object               # @.str.34
.L.str.34:
	.asciz	"applyprim_display"
	.size	.L.str.34, 18

	.type	.L.str.35,@object               # @.str.35
.L.str.35:
	.asciz	"applyprim_print"
	.size	.L.str.35, 16

	.type	.L.str.36,@object               # @.str.36
.L.str.36:
	.asciz	"'()"
	.size	.L.str.36, 4

	.type	.L.str.37,@object               # @.str.37
.L.str.37:
	.asciz	"'%s"
	.size	.L.str.37, 4

	.type	.L.str.38,@object               # @.str.38
.L.str.38:
	.asciz	"prim_print Sym case."
	.size	.L.str.38, 21

	.type	.L.str.39,@object               # @.str.39
.L.str.39:
	.asciz	"'("
	.size	.L.str.39, 3

	.type	.L.str.40,@object               # @.str.40
.L.str.40:
	.asciz	")"
	.size	.L.str.40, 2

	.type	.L.str.41,@object               # @.str.41
.L.str.41:
	.asciz	"applyprim_println"
	.size	.L.str.41, 18

	.type	.L.str.42,@object               # @.str.42
.L.str.42:
	.asciz	"#<void>"
	.size	.L.str.42, 8

	.type	.L.str.43,@object               # @.str.43
.L.str.43:
	.asciz	"()"
	.size	.L.str.43, 3

	.type	.L.str.44,@object               # @.str.44
.L.str.44:
	.asciz	"prim_print_aux bool case"
	.size	.L.str.44, 25

	.type	.L.str.45,@object               # @.str.45
.L.str.45:
	.asciz	"#f"
	.size	.L.str.45, 3

	.type	.L.str.46,@object               # @.str.46
.L.str.46:
	.asciz	"#t"
	.size	.L.str.46, 3

	.type	.L.str.47,@object               # @.str.47
.L.str.47:
	.asciz	"Unknown Boolean value: %lu"
	.size	.L.str.47, 27

	.type	.L.str.48,@object               # @.str.48
.L.str.48:
	.asciz	"#<continuation>"
	.size	.L.str.48, 16

	.type	.L.str.49,@object               # @.str.49
.L.str.49:
	.asciz	"#<procedure>"
	.size	.L.str.49, 13

	.type	.L.str.50,@object               # @.str.50
.L.str.50:
	.asciz	"("
	.size	.L.str.50, 2

	.type	.L.str.51,@object               # @.str.51
.L.str.51:
	.asciz	"%ld"
	.size	.L.str.51, 4

	.type	.L.str.52,@object               # @.str.52
.L.str.52:
	.asciz	"prim_print_aux Int case."
	.size	.L.str.52, 25

	.type	.L.str.53,@object               # @.str.53
.L.str.53:
	.asciz	"%s"
	.size	.L.str.53, 3

	.type	.L.str.54,@object               # @.str.54
.L.str.54:
	.asciz	"prim_print_aux Str case."
	.size	.L.str.54, 25

	.type	.L.str.55,@object               # @.str.55
.L.str.55:
	.asciz	"prim_print_aux Sym case."
	.size	.L.str.55, 25

	.type	.L.str.56,@object               # @.str.56
.L.str.56:
	.asciz	"#hash("
	.size	.L.str.56, 7

	.type	.L.str.57,@object               # @.str.57
.L.str.57:
	.asciz	"Sets not currently supported!"
	.size	.L.str.57, 30

	.type	.L.str.58,@object               # @.str.58
.L.str.58:
	.asciz	"(print v); unrecognized value %lu"
	.size	.L.str.58, 34

	.type	.L.str.59,@object               # @.str.59
.L.str.59:
	.asciz	"print_cons"
	.size	.L.str.59, 11

	.type	.L.str.60,@object               # @.str.60
.L.str.60:
	.asciz	" "
	.size	.L.str.60, 2

	.type	.L.str.61,@object               # @.str.61
.L.str.61:
	.asciz	" . "
	.size	.L.str.61, 4

	.type	.L.str.62,@object               # @.str.62
.L.str.62:
	.asciz	"print_vector"
	.size	.L.str.62, 13

	.type	.L.str.63,@object               # @.str.63
.L.str.63:
	.asciz	"#("
	.size	.L.str.63, 3

	.type	.L.str.64,@object               # @.str.64
.L.str.64:
	.asciz	"print_hash"
	.size	.L.str.64, 11

	.type	.L.str.65,@object               # @.str.65
.L.str.65:
	.asciz	"Void"
	.size	.L.str.65, 5

	.type	.L.str.66,@object               # @.str.66
.L.str.66:
	.asciz	"Null"
	.size	.L.str.66, 5

	.type	.L.str.67,@object               # @.str.67
.L.str.67:
	.asciz	"Bool"
	.size	.L.str.67, 5

	.type	.L.str.68,@object               # @.str.68
.L.str.68:
	.asciz	"Continuation"
	.size	.L.str.68, 13

	.type	.L.str.69,@object               # @.str.69
.L.str.69:
	.asciz	"Closure"
	.size	.L.str.69, 8

	.type	.L.str.70,@object               # @.str.70
.L.str.70:
	.asciz	"Cons"
	.size	.L.str.70, 5

	.type	.L.str.71,@object               # @.str.71
.L.str.71:
	.asciz	"Int"
	.size	.L.str.71, 4

	.type	.L.str.72,@object               # @.str.72
.L.str.72:
	.asciz	"String"
	.size	.L.str.72, 7

	.type	.L.str.73,@object               # @.str.73
.L.str.73:
	.asciz	"Symbol"
	.size	.L.str.73, 7

	.type	.L.str.74,@object               # @.str.74
.L.str.74:
	.asciz	"Vector"
	.size	.L.str.74, 7

	.type	.L.str.75,@object               # @.str.75
.L.str.75:
	.asciz	"Hash"
	.size	.L.str.75, 5

	.type	.L.str.76,@object               # @.str.76
.L.str.76:
	.asciz	"Set"
	.size	.L.str.76, 4

	.type	.L.str.77,@object               # @.str.77
.L.str.77:
	.asciz	"Other"
	.size	.L.str.77, 6

	.type	.L.str.78,@object               # @.str.78
.L.str.78:
	.asciz	"applyprim_halt"
	.size	.L.str.78, 15

	.type	.L.str.79,@object               # @.str.79
.L.str.79:
	.asciz	"get_vector_length"
	.size	.L.str.79, 18

	.type	.L.str.80,@object               # @.str.80
.L.str.80:
	.asciz	"Vectors larger than 256 elements are unimplemented. Sorry!"
	.size	.L.str.80, 59

	.type	.L.str.81,@object               # @.str.81
.L.str.81:
	.asciz	"applyprim_vector_63"
	.size	.L.str.81, 20

	.type	.L.str.82,@object               # @.str.82
.L.str.82:
	.asciz	"applyprim_vector_45length"
	.size	.L.str.82, 26

	.type	.L.str.83,@object               # @.str.83
.L.str.83:
	.asciz	"prim_vector_45length"
	.size	.L.str.83, 21

	.type	.L.str.84,@object               # @.str.84
.L.str.84:
	.asciz	"bounds check"
	.size	.L.str.84, 13

	.type	.L.str.85,@object               # @.str.85
.L.str.85:
	.asciz	"Bounds check fail, wanted pos %ld, only %ld elements"
	.size	.L.str.85, 53

	.type	.L.str.86,@object               # @.str.86
.L.str.86:
	.asciz	"applyprim_vector_45ref"
	.size	.L.str.86, 23

	.type	.L.str.87,@object               # @.str.87
.L.str.87:
	.asciz	"vector-ref"
	.size	.L.str.87, 11

	.type	.L.str.88,@object               # @.str.88
.L.str.88:
	.asciz	"vector-ref index_pos"
	.size	.L.str.88, 21

	.type	.L.str.89,@object               # @.str.89
.L.str.89:
	.asciz	"applyprim_vector_45set_33"
	.size	.L.str.89, 26

	.type	.L.str.90,@object               # @.str.90
.L.str.90:
	.asciz	"Function '%s' expected 3 arguments but got 1."
	.size	.L.str.90, 46

	.type	.L.str.91,@object               # @.str.91
.L.str.91:
	.asciz	"Function '%s' expected 3 arguments but got 2."
	.size	.L.str.91, 46

	.type	.L.str.92,@object               # @.str.92
.L.str.92:
	.asciz	"Function '%s' only takes 3 arguments."
	.size	.L.str.92, 38

	.type	.L.str.93,@object               # @.str.93
.L.str.93:
	.asciz	"vector-set!"
	.size	.L.str.93, 12

	.type	.L.str.94,@object               # @.str.94
.L.str.94:
	.asciz	"apply vector-set! index_pos"
	.size	.L.str.94, 28

	.type	.L.str.95,@object               # @.str.95
.L.str.95:
	.asciz	"Expected an empty list but got something else for function '%s'"
	.size	.L.str.95, 64

	.type	.L.str.96,@object               # @.str.96
.L.str.96:
	.asciz	"applyprim_void"
	.size	.L.str.96, 15

	.type	.L.str.97,@object               # @.str.97
.L.str.97:
	.asciz	"vec_eq_helper avec"
	.size	.L.str.97, 19

	.type	.L.str.98,@object               # @.str.98
.L.str.98:
	.asciz	"vec_eq_helper bvec"
	.size	.L.str.98, 19

	.type	.L.str.99,@object               # @.str.99
.L.str.99:
	.asciz	"vec_eq_helper alen"
	.size	.L.str.99, 19

	.type	.L.str.100,@object              # @.str.100
.L.str.100:
	.asciz	"vec_eq_helper blen"
	.size	.L.str.100, 19

	.type	.L.str.101,@object              # @.str.101
.L.str.101:
	.asciz	"prim_eq_63 Bool a"
	.size	.L.str.101, 18

	.type	.L.str.102,@object              # @.str.102
.L.str.102:
	.asciz	"prim_eq_63 Bool b"
	.size	.L.str.102, 18

	.type	.L.str.103,@object              # @.str.103
.L.str.103:
	.asciz	"prim_eq_63 Int a"
	.size	.L.str.103, 17

	.type	.L.str.104,@object              # @.str.104
.L.str.104:
	.asciz	"prim_eq_63 Int b"
	.size	.L.str.104, 17

	.type	.L.str.105,@object              # @.str.105
.L.str.105:
	.asciz	"prim_eq_63 Str a"
	.size	.L.str.105, 17

	.type	.L.str.106,@object              # @.str.106
.L.str.106:
	.asciz	"prim_eq_63 Str b"
	.size	.L.str.106, 17

	.type	.L.str.107,@object              # @.str.107
.L.str.107:
	.asciz	"prim_eq_63 Sym a"
	.size	.L.str.107, 17

	.type	.L.str.108,@object              # @.str.108
.L.str.108:
	.asciz	"prim_eq_63 Sym b"
	.size	.L.str.108, 17

	.type	.L.str.109,@object              # @.str.109
.L.str.109:
	.asciz	"Hash eq? not supported currently."
	.size	.L.str.109, 34

	.type	.L.str.110,@object              # @.str.110
.L.str.110:
	.asciz	"Set eq? not supported currently."
	.size	.L.str.110, 33

	.type	.L.str.111,@object              # @.str.111
.L.str.111:
	.asciz	"I do not know what youre trying to eq?!!."
	.size	.L.str.111, 42

	.type	.L.str.112,@object              # @.str.112
.L.str.112:
	.asciz	"applyprim_eq_63"
	.size	.L.str.112, 16

	.type	.L.str.113,@object              # @.str.113
.L.str.113:
	.asciz	"applyprim_eqv_63"
	.size	.L.str.113, 17

	.type	.L.str.114,@object              # @.str.114
.L.str.114:
	.asciz	"applyprim_equal_63"
	.size	.L.str.114, 19

	.type	.L.str.115,@object              # @.str.115
.L.str.115:
	.asciz	"applyprim_number_63"
	.size	.L.str.115, 20

	.type	.L.str.116,@object              # @.str.116
.L.str.116:
	.asciz	"applyprim_integer_63"
	.size	.L.str.116, 21

	.type	.L.str.117,@object              # @.str.117
.L.str.117:
	.asciz	"applyprim_boolean_63"
	.size	.L.str.117, 21

	.type	.L.str.118,@object              # @.str.118
.L.str.118:
	.asciz	"applyprim_void_63"
	.size	.L.str.118, 18

	.type	.L.str.119,@object              # @.str.119
.L.str.119:
	.asciz	"applyprim_procedure_63"
	.size	.L.str.119, 23

	.type	.L.str.120,@object              # @.str.120
.L.str.120:
	.asciz	"applyprim_null_63"
	.size	.L.str.120, 18

	.type	.L.str.121,@object              # @.str.121
.L.str.121:
	.asciz	"applyprim_cons_63"
	.size	.L.str.121, 18

	.type	.L.str.122,@object              # @.str.122
.L.str.122:
	.asciz	"applyprim_cons"
	.size	.L.str.122, 15

	.type	.L.str.123,@object              # @.str.123
.L.str.123:
	.asciz	"applyprim_cdr"
	.size	.L.str.123, 14

	.type	.L.str.124,@object              # @.str.124
.L.str.124:
	.asciz	"cdr"
	.size	.L.str.124, 4

	.type	.L.str.125,@object              # @.str.125
.L.str.125:
	.asciz	"- a"
	.size	.L.str.125, 4

	.type	.L.str.126,@object              # @.str.126
.L.str.126:
	.asciz	"- b"
	.size	.L.str.126, 4

	.type	.L.str.127,@object              # @.str.127
.L.str.127:
	.asciz	"apply -"
	.size	.L.str.127, 8

	.type	.L.str.128,@object              # @.str.128
.L.str.128:
	.asciz	"apply - carval"
	.size	.L.str.128, 15

	.type	.L.str.129,@object              # @.str.129
.L.str.129:
	.asciz	"applyprim__45 final"
	.size	.L.str.129, 20

	.type	.L.str.130,@object              # @.str.130
.L.str.130:
	.asciz	"* a"
	.size	.L.str.130, 4

	.type	.L.str.131,@object              # @.str.131
.L.str.131:
	.asciz	"* b"
	.size	.L.str.131, 4

	.type	.L.str.132,@object              # @.str.132
.L.str.132:
	.asciz	"apply * cons_obj"
	.size	.L.str.132, 17

	.type	.L.str.133,@object              # @.str.133
.L.str.133:
	.asciz	"apply * carval"
	.size	.L.str.133, 15

	.type	.L.str.134,@object              # @.str.134
.L.str.134:
	.asciz	"apply * cdrval"
	.size	.L.str.134, 15

	.type	.L.str.135,@object              # @.str.135
.L.str.135:
	.asciz	"apply * taking a non-list argument!"
	.size	.L.str.135, 36

	.type	.L.str.136,@object              # @.str.136
.L.str.136:
	.asciz	"/ a"
	.size	.L.str.136, 4

	.type	.L.str.137,@object              # @.str.137
.L.str.137:
	.asciz	"/ b"
	.size	.L.str.137, 4

	.type	.L.str.138,@object              # @.str.138
.L.str.138:
	.asciz	"applyprim__61"
	.size	.L.str.138, 14

	.type	.L.str.139,@object              # @.str.139
.L.str.139:
	.asciz	"= a"
	.size	.L.str.139, 4

	.type	.L.str.140,@object              # @.str.140
.L.str.140:
	.asciz	"= b"
	.size	.L.str.140, 4

	.type	.L.str.141,@object              # @.str.141
.L.str.141:
	.asciz	"< a"
	.size	.L.str.141, 4

	.type	.L.str.142,@object              # @.str.142
.L.str.142:
	.asciz	"< b"
	.size	.L.str.142, 4

	.type	.L.str.143,@object              # @.str.143
.L.str.143:
	.asciz	"applyprim__60_61"
	.size	.L.str.143, 17

	.type	.L.str.144,@object              # @.str.144
.L.str.144:
	.asciz	"<= a"
	.size	.L.str.144, 5

	.type	.L.str.145,@object              # @.str.145
.L.str.145:
	.asciz	"<= b"
	.size	.L.str.145, 5

	.type	.L.str.146,@object              # @.str.146
.L.str.146:
	.asciz	"applyprim_not"
	.size	.L.str.146, 14

	.type	.L.str.147,@object              # @.str.147
.L.str.147:
	.asciz	"not"
	.size	.L.str.147, 4

	.type	.L.str.148,@object              # @.str.148
.L.str.148:
	.asciz	"applyprim_hash_45has_45key_63"
	.size	.L.str.148, 30

	.type	.L.str.149,@object              # @.str.149
.L.str.149:
	.asciz	"hash-has-key?"
	.size	.L.str.149, 14

	.type	.L.str.150,@object              # @.str.150
.L.str.150:
	.asciz	"Key not provided value in (hash)"
	.size	.L.str.150, 33

	.type	.L.str.151,@object              # @.str.151
.L.str.151:
	.asciz	"applyprim_hash_45keys"
	.size	.L.str.151, 22

	.type	.L.str.152,@object              # @.str.152
.L.str.152:
	.asciz	"hash-keys"
	.size	.L.str.152, 10

	.type	.L.str.153,@object              # @.str.153
.L.str.153:
	.asciz	"Too many args given in hash-ref"
	.size	.L.str.153, 32

	.type	.L.str.154,@object              # @.str.154
.L.str.154:
	.asciz	"Bad Types Somewhere in hash-ref... good luck!"
	.size	.L.str.154, 46

	.type	.L.str.155,@object              # @.str.155
.L.str.155:
	.asciz	"hash-ref"
	.size	.L.str.155, 9

	.type	.L.str.156,@object              # @.str.156
.L.str.156:
	.asciz	"Map did not have requested key"
	.size	.L.str.156, 31

	.type	.L.str.157,@object              # @.str.157
.L.str.157:
	.asciz	"applyprim_hash_45set"
	.size	.L.str.157, 21

	.type	.L.str.158,@object              # @.str.158
.L.str.158:
	.asciz	"hash-set"
	.size	.L.str.158, 9

	.type	.L.str.159,@object              # @.str.159
.L.str.159:
	.asciz	"applyprim_hash_63"
	.size	.L.str.159, 18

	.type	.L.str.160,@object              # @.str.160
.L.str.160:
	.asciz	"applyprim_hash_45count"
	.size	.L.str.160, 23

	.type	.L.str.161,@object              # @.str.161
.L.str.161:
	.asciz	"hash-count"
	.size	.L.str.161, 11

	.type	srr,@object                     # @srr
	.bss
	.globl	srr
	.p2align	3
srr:
	.quad	0
	.size	srr, 8

	.type	fpr,@object                     # @fpr
	.globl	fpr
	.p2align	3
fpr:
	.quad	0
	.size	fpr, 8

	.type	retr,@object                    # @retr
	.globl	retr
	.p2align	3
retr:
	.quad	0
	.size	retr, 8

	.type	spr,@object                     # @spr
	.globl	spr
	.p2align	3
spr:
	.quad	0
	.size	spr, 8

	.ident	"Ubuntu clang version 14.0.5-++20220603124341+2f0a69c32a4c-1~exp1~20220603124352.149"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym insert_update
	.addrsig_sym malloc
	.addrsig_sym eq_helper
	.addrsig_sym insert_copy
	.addrsig_sym map_insert
	.addrsig_sym map_keys
	.addrsig_sym const_init_null
	.addrsig_sym prim_cons
	.addrsig_sym map_get
	.addrsig_sym map_has_key
	.addrsig_sym map_count
	.addrsig_sym printf
	.addrsig_sym prim_display
	.addrsig_sym alloc
	.addrsig_sym alloc_atomic
	.addrsig_sym _get_both
	.addrsig_sym unwrap_cons
	.addrsig_sym const_init_int
	.addrsig_sym const_init_void
	.addrsig_sym const_init_true
	.addrsig_sym const_init_false
	.addrsig_sym make_record
	.addrsig_sym calloc
	.addrsig_sym closure_alloc
	.addrsig_sym prim_make_45vector
	.addrsig_sym unwrap_clo
	.addrsig_sym prim_vector_45set_33
	.addrsig_sym closure_get_env_part
	.addrsig_sym closure_get_fn_part
	.addrsig_sym check_for_overflow
	.addrsig_sym handle_overflow
	.addrsig_sym __assert_fail
	.addrsig_sym handle_underflow
	.addrsig_sym handle_continuation_function
	.addrsig_sym prim_car
	.addrsig_sym start_runtime
	.addrsig_sym exit
	.addrsig_sym prim__43
	.addrsig_sym unwrap_int
	.addrsig_sym get_type_name
	.addrsig_sym prim_vector_45ref
	.addrsig_sym unwrap_hash
	.addrsig_sym unwrap_vector
	.addrsig_sym unwrap_bool
	.addrsig_sym unwrap_str
	.addrsig_sym unwrap_sym
	.addrsig_sym prim_print
	.addrsig_sym print_cons
	.addrsig_sym prim_print_aux
	.addrsig_sym prim_println
	.addrsig_sym print_vector
	.addrsig_sym print_hash
	.addrsig_sym _get_vector_length
	.addrsig_sym prim_halt
	.addrsig_sym prim_vector_63
	.addrsig_sym prim_vector_45length
	.addrsig_sym bounds_check
	.addrsig_sym prim_void
	.addrsig_sym cons_eq_helper
	.addrsig_sym vec_eq_helper
	.addrsig_sym strcmp
	.addrsig_sym prim_eq_63
	.addrsig_sym make_predicate
	.addrsig_sym prim_eqv_63
	.addrsig_sym prim_equal_63
	.addrsig_sym prim_number_63
	.addrsig_sym prim_integer_63
	.addrsig_sym prim_boolean_63
	.addrsig_sym prim_void_63
	.addrsig_sym prim_procedure_63
	.addrsig_sym prim_null_63
	.addrsig_sym prim_cons_63
	.addrsig_sym prim_cdr
	.addrsig_sym applyprim__42
	.addrsig_sym prim__61
	.addrsig_sym prim__60_61
	.addrsig_sym prim_not
	.addrsig_sym map_to_sin
	.addrsig_sym prim_hash_45has_45key_63
	.addrsig_sym prim_hash_45keys
	.addrsig_sym hash_ref_impl
	.addrsig_sym prim_hash_45set
	.addrsig_sym prim_hash_63
	.addrsig_sym prim_hash_45count
	.addrsig_sym __underflow_handler
	.addrsig_sym __program_finished
	.addrsig_sym gotosub9824
	.addrsig_sym gotosub9823
	.addrsig_sym __main
	.addrsig_sym gotosub9825
	.addrsig_sym callee
	.addrsig_sym helper
	.addrsig_sym srr
	.addrsig_sym fpr
	.addrsig_sym retr
	.addrsig_sym spr
