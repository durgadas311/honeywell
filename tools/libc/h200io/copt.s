// Set card reader/punch option
// int copt(int op)
// returns 0 on success

	.globl	@P0,@N1
	.globl	_copt
_copt:
	scr	0(x1),070
	exm	4(x1),3f+7,001	// set option code
3:	pcb	1f,013,041,000	// option set above
	lca	@P0,x5
	lcr	0(x1),077

1:	lca	@N1,x5
	lcr	0(x1),077
