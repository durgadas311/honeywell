// void *memset(void *d, int c, int n)
// neither requires nor affects punctuation

	.globl	@zero,@one
	.globl	_memset
	.text
_memset:
	scr	0(x1),070
	lca	4(x1),x5	// dest
	exm	8(x1),x6,001	// fill char
	lca	12(x1),x3	// count
1:	c	@zero,x3
	bct	9f,042
	exm	x6,0(x5),001
	ba	@one,x5
	bs	@one,x3
	b	1b

9:	lca	4(x1),x5
	lcr	0(x1),077
