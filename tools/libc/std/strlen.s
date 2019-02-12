// strlen
// string must have a RM.

// remember: C prepends a '_' to names.
// args: string, return: length (x5)

	.globl	@one
	.globl	_strlen
	.text
_strlen:
	scr	0(x1),070
	lca	4(x1),x5
	// we don't actually copy, but we do read both AAR and BAR
	mos	0(x5),0(x5),010	// stops at WM, not RM...
	scr	x5,067
	bs	4(x1),x5
	bs	@one,x5
	lcr	0(x1),077
