// Return true if current tape unit position is EOT
// int isbot(int unit)

	.globl	@P0,@P1
	.globl	_iseot
_iseot:
	scr	0(x1),070
	sst	4(x1),2f+7,007	// set unit number
2:	pcb	1f,014,000,060	// unit set above
	lca	@P0,x5
	lcr	0(x1),077

1:	lca	@P1,x5
	lcr	0(x1),077
