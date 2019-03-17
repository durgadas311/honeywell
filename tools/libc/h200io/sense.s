// Return true if SENSE switche is ON
// int sense(int sense)

	.globl	@P0,@P1
	.globl	_sense
_sense:
	scr	0(x1),070
	sst	4(x1),2f+5,037	// set cond
2:	bct	1f,000	// cond set previously
	lca	@P0,x5
	lcr	0(x1),077

1:	lca	@P1,x5
	lcr	0(x1),077
