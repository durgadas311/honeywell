// memalign(int align, int size);

	.globl	@P0,@P1
	.globl	@heap,@mtop
	.globl	_memalign
	.text
_memalign:
	scr	0(x1),070
	lca	4(x1),x5
	c	@P0,x5 // just ignore this...
	bct	1f,042
	bs	@P1,x5	// adr mask
	lca	x5,x6
	ext	@heap,x5
	c	@P0,x5
	bct	1f,042	// no alignment needed
	ha	x6,x5
	ba	@P1,x5	// num pad needed
1:	ba	@heap,x5
	lca	x5,x6		// X5 is ret value
	ba	8(x1),x6	// X6 is new @heap
	c	@mtop,x6
	bct	2f,046	// no more memory
	lca	x6,@heap
	lcr	0(x1),077

2:	lca	@P0,x5
	lcr	0(x1),077

	.heap	2048	// some default heap space
