// multiply two 'int' values

// returns result in x5
	.globl	@mul
	.text
9:	csm	// return but keep CSR pointing here
?mul:	scr	x3,067	// save AAR (ptr to p1)
	scr	x4,070	// save BAR (ptr to p2)
	lca	0(x3),x5
	bim	0(x4),x5
	b	9b

	.data
@mul:	.word	?mul
