// void *memcpy(void *d, void *s, int n)
// neither requires nor affects punctuation

	.globl	@P0,@P1
	.globl	_memcpy
	.text
_memcpy:
	scr	0(x1),070
	lca	4(x1),x5	// dest
	lca	8(x1),x6	// source
	lca	12(x1),x3	// count
1:	c	@P0,x3
	bct	9f,042
	exm	0(x5),0(x6),001
	ba	@P1,x5
	ba	@P1,x6
	bs	@P1,x3
	b	1b

9:	lca	4(x1),x5
	lcr	0(x1),077
