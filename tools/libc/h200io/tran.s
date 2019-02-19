// void tran(char *d, char *s, void *tr)
// both strings must have a RM.

	.globl	@two
	.globl	_tran
	.text
_tran:
	scr	0(x1),070
	lca	4(x1),x6
	mos	0(x6),0(x6),010	// stops at WM, not RM...
	scr	x6,067
	bs	@two,x6	// X6 = end of dest
	lca	8(x1),x5
	mos	0(x5),0(x5),010	// stops at WM, not RM...
	scr	x5,067
	bs	@two,x5	// X5 = end of src
	// now set WM at start of each...
	lca	4(x1),x3
	lca	8(x1),x4
	sw	0(x3),0(x4)
	mcw	12(x1),1f+12	// set trans tbl
1:	mat	0(x5),0(x6),0	// filled in above
	// clear prev set WM...
	cw	0(x3),0(x4)
	lcr	0(x1),077
