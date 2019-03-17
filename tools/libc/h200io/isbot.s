// Return true if current tape unit position is BOT
// int isbot(int unit)

	.globl	@P0,@P1
	.globl	_isbot
_isbot:
	scr	0(x1),070
	sst	4(x1),2f+7,007	// set unit number
2:	pcb	1f,014,040,060	// unit set above
	lca	@P0,x5
	lcr	0(x1),077

1:	lca	@P1,x5
	lcr	0(x1),077
