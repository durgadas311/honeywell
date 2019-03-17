// Write a block to mag tape. Uses I/O channel 14
// int twrite(int unit, void *buf)
// returns num chars on success, -1 on error

	.globl	@N1
	.globl	_twrite
_twrite:
	scr	0(x1),070
	lca	8(x1),x5	// buf
	sst	4(x1),2f+7,007	// set unit number in PDT
	sst	4(x1),3f+7,007	// set unit number in busy PCB
	sst	4(x1),4f+7,007	// set unit number in error PCB

	// Write a RM-terminated block to tape, fwd direction.
2:	pdt	0(x5),014,000,020,0	// unit set above
3:	pcb	.,014,000,000		// unit set above
4:	pcb	1f,014,040,040		// unit set above
	scr	x5,004	// we know x5 has WM
	scr	x6,014
	bs	x6,x5	// num chars transferred
	lcr	0(x1),077

1:	lca	@N1,x5
	lcr	0(x1),077
