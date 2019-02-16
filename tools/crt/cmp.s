// *signed* compare of ints

	.globl	@cmp
	.text
9:	csm
?cmp:	scr	x3,067	// save AAR (left operand)
	scr	x4,070	// save BAR (right operand)
	lca	0(x3),x3
	lca	0(x4),x4
	ba	off,x3
	ba	off,x4
	c	x3,x4
	b	9b

	.data
@cmp:	.word	?cmp
off:	.bin	040000000#4	// offset for signed
