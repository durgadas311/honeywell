// Read a punch card. Assumes only one card reader. Uses I/O channel 13
// int cread(void *buf)
// returns 0 on success
// Reader does not report "hopper empty", caller must detect EOF card.
// user must ensure EOF card is present.
// If buffer is smaller than 80 chars, must have RM.

	.globl	@zero,@none
	.globl	_cread
_cread:
	scr	0(x1),070
	lca	4(x1),x5
	pdt	0(x5),013,041
	pcb	.,013,041,010
	pcb	1f,013,041,041
	lca	@zero,x5
	lcr	0(x1),077

1:	lca	@none,x5
	lcr	0(x1),077
