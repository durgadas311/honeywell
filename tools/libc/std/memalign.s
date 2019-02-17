// memalign(int align, int size);

	.globl	@zero,@one,@heap,@mtop
	.globl	_memalign
	.text
_memalign:
	scr	0(x1),070
	lca	4(x1),x5
	c	@zero,x5 // just ignore this...
	bct	1f,042
	bs	@one,x5	// adr mask
	lca	x5,x6
	ext	@heap,x5
	c	@zero,x5
	bct	1f,042	// no alignment needed
	ha	x6,x5
	ba	@one,x5	// num pad needed
1:	ba	@heap,x5
	lca	x5,x6		// X5 is ret value
	ba	8(x1),x6	// X6 is new @heap
	c	@mtop,x6
	bct	2f,046	// no more memory
	lca	x6,@heap
	lcr	0(x1),077

2:	lca	@zero,x5
	lcr	0(x1),077
