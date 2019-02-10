// strlen
// string must have a RM.

// remember: C prepends a '_' to names.
// args: string, return: length (x5)

	.globl	_strlen
	.text
_strlen:
	scr	0(x1),070
	lca	4(x1),in
	// we don't actually copy, but we do read both AAR and BAR
	mos	(inp),(inp),010	// stops at WM, not RM...
	scr	x5,067
	bs	in,x5
	lcr	0(x1),077

	.data
inp:
in:	.word	0
