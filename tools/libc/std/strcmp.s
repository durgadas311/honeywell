// strcmp(char *s1, char *s2)
// strings must have a RM.

	.globl	@zero,@one
	.globl	_strcmp
	.text
_strcmp:
	scr	0(x1),070
	lca	4(x1),x3
	lca	8(x1),x4
	lca	@zero,x5
	lca	@zero,x6
4:	bcc	1f,0(x3),030	// RM?
	bcc	2f,0(x4),030	// RM?
	exm	0(x3),x5,001
	exm	0(x4),x6,001
	bs	x6,x5
	c	@zero,x5
	bct	3f,045	// if *s1++ != *s2++
	ba	@one,x3
	ba	@one,x4
	b	4b
1:	// s1 end, check s2...
	bcc	3f,0(x4),030	// RM? X5=0 so just ret
	bs	@one,x5		// s1 < s2, so ret -
	b	3f
2:	// s2 end, s1 not...
	ba	@one,x5		// s1 > s2, so ret +
3:	// differ, X5 contains +/-
	lcr	0(x1),077
