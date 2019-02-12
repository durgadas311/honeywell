// implement << (shift left) on 'int', result in x5

// this is a CSM call, but stack (x1) should still be usable.
	.globl	@one,@zero
	.globl	@sla
	.text
9:	csm	// return, but keep CSR pointing here
?sla:	scr	x3,067	// save AAR (ptr to num shifts)
	scr	x4,070	// save BAR (ptr to value)
	lca	0(x4),x5
	lca	0(x3),x3
2:	c	@zero,x3
	bct	9b,042
	ba	x5
	bs	@one,x3
	b	2b
	.data
@sla:	.word	?sla
