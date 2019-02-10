// strcpy
// string must have a RM.

// remember: C prepends a '_' to names.
// args: string, return: length (x5)

	.globl	_strcpy
	.text
_strcpy:
	scr	0(x1),070
	lca	4(x1),i1
	lca	8(x1),i2
	mos	(p2),(p1),017	// stops at WM, not RM...
	lca	i1,x5
	lcr	0(x1),077

	.data
p1:
i1:	.word	0
p2:
i2:	.word	0
