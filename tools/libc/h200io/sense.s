// Return true if SENSE switche is ON
// int sense(int sense)

	.globl	@zero,@one
	.globl	_sense
_sense:
	scr	0(x1),070
	sst	4(x1),2f+5,037	// set cond
2:	bct	1f,000	// cond set previously
	lca	@zero,x5
	lcr	0(x1),077

1:	lca	@one,x5
	lcr	0(x1),077
