// Skip forword one block on tape unit. (to next block gap)
// int tfwd(int unit)
// returns 0 on success, -1 on error

	.globl	@zero,@none
	.globl	_tfwd
_tfwd:
	scr	0(x1),070
	sst	4(x1),2f+7,007	// set unit number
	sst	4(x1),3f+7,007	// set unit number
2:	pdt	0(x5),014,040,040,0	// unit set above
3:	pcb	.,014,040,000		// unit set above
	lca	@zero,x5	// no errors?
	lcr	0(x1),077
