// bitwise OR, NOT, NEG

	.globl	@one,@none,@zero
	.globl	@or,@not,@neg
	.text
9:	csm
?or:	scr	x3,067	// save AAR (left operand)
	scr	x4,070	// save BAR (right operand)
	lca	0(x3),x3
	lca	0(x4),x5
	ha	@none,x3
	ha	@none,x5
	ext	x3,x5
	ha	@none,x5
	b	9b

8:	csm
?not:	scr	x3,067	// save AAR (only operand)
	lca	0(x3),x5
	ha	@none,x5
	b	8b

7:	csm
?neg:	scr	x3,067	// save AAR (only operand)
	lca	@zero,x5
	bs	0(x3),x5
	b	7b

	.data
@or:	.word	?or
@not:	.wnotd	?not
@neg:	.wnegd	?neg
