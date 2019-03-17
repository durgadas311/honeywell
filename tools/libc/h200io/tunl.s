// Rewind and unload mag tape. Uses I/O channel 14
// int tunl(int unit)
// returns 0 on success, -1 on error

	.globl	@P0
	.globl	_tunl
_tunl:
	scr	0(x1),070
	sst	4(x1),2f+7,007	// set unit number
2:	pcb	.,014,040,020	// unit set above
	lca	@P0,x5		// no errors?
	lcr	0(x1),077
