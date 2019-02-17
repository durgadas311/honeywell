// Set card reader/punch option
// int copt(int op)
// returns 0 on success

	.globl	@zero,@none
	.globl	_copt
_copt:
	scr	0(x1),070
	exm	4(x1),3f+6,001	// set option code
3:	pcb	1f,041,000	// option set above
	lca	@zero,x5
	lcr	0(x1),077

1:	lca	@none,x5
	lcr	0(x1),077
