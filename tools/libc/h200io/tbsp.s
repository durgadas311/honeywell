// Backspace tape unit one block
// int tbsp(int unit)
// returns 0 on success, -1 on error

	.globl	@P0
	.globl	_tbsp
_tbsp:
	scr	0(x1),070
	sst	4(x1),2f+7,007	// set unit number in PDT
	sst	4(x1),3f+7,007	// set unit number in busy PCB

2:	pdt	0(x5),014,040,000,0	// unit set above
3:	pcb	.,014,040,00		// unit set above
	lca	@P0,x5			// never any error?
	lcr	0(x1),077
