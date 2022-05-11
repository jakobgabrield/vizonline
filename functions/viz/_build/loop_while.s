	.text
	.file	"Viz"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movl	$0, -44(%rbp)
	leaq	.Lfmt(%rip), %rdi
	leaq	.Lstr(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	leaq	.Lfmt.2(%rip), %rdi
	leaq	.Lstr.1(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	leaq	.Lfmt.3(%rip), %rbx
	cmpl	$9, -44(%rbp)
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %while_body
                                        # =>This Inner Loop Header: Depth=1
	movl	-44(%rbp), %esi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	incl	-44(%rbp)
	cmpl	$9, -44(%rbp)
	jle	.LBB0_2
.LBB0_3:                                # %while_end
	leaq	.Lfmt.5(%rip), %rdi
	leaq	.Lstr.4(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	leaq	.Lfmt.7(%rip), %rdi
	leaq	.Lstr.6(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	leaq	.Lfmt.8(%rip), %rbx
	cmpl	$0, -44(%rbp)
	jle	.LBB0_6
	.p2align	4, 0x90
.LBB0_5:                                # %while_body12
                                        # =>This Inner Loop Header: Depth=1
	movl	-44(%rbp), %esi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	decl	-44(%rbp)
	cmpl	$0, -44(%rbp)
	jg	.LBB0_5
.LBB0_6:                                # %while_end17
	leaq	.Lfmt.10(%rip), %rdi
	leaq	.Lstr.9(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	leaq	.Lfmt.12(%rip), %rdi
	leaq	.Lstr.11(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	movl	$10, -44(%rbp)
	movq	%rsp, %rax
	leaq	-16(%rax), %rbx
	movq	%rbx, %rsp
	movl	$10, -16(%rax)
	leaq	.Lfmt.13(%rip), %r14
	leaq	.Lfmt.15(%rip), %r15
	leaq	.Lstr.14(%rip), %r12
	leaq	.Lfmt.16(%rip), %r13
	jmp	.LBB0_7
	.p2align	4, 0x90
.LBB0_11:                               # %while_end35
                                        #   in Loop: Header=BB0_7 Depth=1
	decl	-44(%rbp)
.LBB0_7:                                # %while20
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_10 Depth 2
	cmpl	$0, -44(%rbp)
	jle	.LBB0_12
# %bb.8:                                # %while_body23
                                        #   in Loop: Header=BB0_7 Depth=1
	movl	-44(%rbp), %esi
	movq	%r14, %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	cmpl	$0, (%rbx)
	jle	.LBB0_11
	.p2align	4, 0x90
.LBB0_10:                               # %while_body29
                                        #   Parent Loop BB0_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	movl	(%rbx), %esi
	movq	%r13, %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	decl	(%rbx)
	cmpl	$0, (%rbx)
	jg	.LBB0_10
	jmp	.LBB0_11
.LBB0_12:                               # %while_end38
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lstr,@object                   # @str
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstr:
	.asciz	"=========================="
	.size	.Lstr, 27

	.type	.Lfmt,@object                   # @fmt
.Lfmt:
	.asciz	"%s\n"
	.size	.Lfmt, 4

	.type	.Lstr.1,@object                 # @str.1
.Lstr.1:
	.asciz	"loop counting up"
	.size	.Lstr.1, 17

	.type	.Lfmt.2,@object                 # @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4

	.type	.Lfmt.3,@object                 # @fmt.3
.Lfmt.3:
	.asciz	"%d\n"
	.size	.Lfmt.3, 4

	.type	.Lstr.4,@object                 # @str.4
.Lstr.4:
	.asciz	"=========================="
	.size	.Lstr.4, 27

	.type	.Lfmt.5,@object                 # @fmt.5
.Lfmt.5:
	.asciz	"%s\n"
	.size	.Lfmt.5, 4

	.type	.Lstr.6,@object                 # @str.6
.Lstr.6:
	.asciz	"loop counting down"
	.size	.Lstr.6, 19

	.type	.Lfmt.7,@object                 # @fmt.7
.Lfmt.7:
	.asciz	"%s\n"
	.size	.Lfmt.7, 4

	.type	.Lfmt.8,@object                 # @fmt.8
.Lfmt.8:
	.asciz	"%d\n"
	.size	.Lfmt.8, 4

	.type	.Lstr.9,@object                 # @str.9
.Lstr.9:
	.asciz	"=========================="
	.size	.Lstr.9, 27

	.type	.Lfmt.10,@object                # @fmt.10
.Lfmt.10:
	.asciz	"%s\n"
	.size	.Lfmt.10, 4

	.type	.Lstr.11,@object                # @str.11
.Lstr.11:
	.asciz	"testing nested while loop"
	.size	.Lstr.11, 26

	.type	.Lfmt.12,@object                # @fmt.12
.Lfmt.12:
	.asciz	"%s\n"
	.size	.Lfmt.12, 4

	.type	.Lfmt.13,@object                # @fmt.13
.Lfmt.13:
	.asciz	"%d\n"
	.size	.Lfmt.13, 4

	.type	.Lstr.14,@object                # @str.14
.Lstr.14:
	.asciz	"nested loop"
	.size	.Lstr.14, 12

	.type	.Lfmt.15,@object                # @fmt.15
.Lfmt.15:
	.asciz	"%s\n"
	.size	.Lfmt.15, 4

	.type	.Lfmt.16,@object                # @fmt.16
.Lfmt.16:
	.asciz	"%d\n"
	.size	.Lfmt.16, 4

	.section	".note.GNU-stack","",@progbits
