// strcat
// string must have a RM.

// remember: C prepends a '_' to names.
// args: string, return: length (x5)

	.globl	_strcat
	.text
_strcat:
	scr	0(x1),070
	lca	4(x1),x5
	lca	8(x1),x6
	mos	0(x5),0(x5),010	// stops at WM, not RM...
	scr	x5,067
	cw	-1(x5)	// erase WM (RM)
	ci	-1(x5)	// erase IM (RM)
	mos	0(x6),-1(x5),017	// stops at WM, not RM...
	lca	4(x1),x5
	lcr	0(x1),077
