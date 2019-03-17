// int memcmp(void *s1, void *s2, int n)
// neither requires nor affects punctuation
// return effectively (*s1 - *s2)

	.globl	@P0,@P1
	.globl	_memcmp
	.text
_memcmp:
	scr	0(x1),070
	lca	4(x1),x4	// s1
	lca	8(x1),x6	// s2
	lca	12(x1),x3	// count
	lca	@P0,x5
	lca	@P0,x7
1:	c	@P0,x3
	bct	9f,042	// while count-- > 0
	exm	0(x4),x5,001
	exm	0(x6),x7,001
	bs	x7,x5
	ba	@P1,x4
	ba	@P1,x6
	bs	@P1,x3
	c	@P0,x5
	bct	1b,042	// while *s1++ == *s2++
9:	lcr	0(x1),077	// X5 contains diff of last chars
