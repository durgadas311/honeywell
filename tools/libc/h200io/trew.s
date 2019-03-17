// Rewind mag tape. Uses I/O channel 14
// int trew(int unit)
// returns 0 on success, -1 on error

	.globl	@P0
	.globl	_trew
_trew:
	scr	0(x1),070
	sst	4(x1),2f+7,007	// set unit number
2:	pcb	.,014,000,020	// unit set above
	lca	@P0,x5		// never any error?
	lcr	0(x1),077
