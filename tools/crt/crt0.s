// For now, we can't determine "top of memory".
// So, use built-in stack.

	.globl	_main
start:
	lca	stk,x1
	lca	stk,x2
	lca	argv,0(x1)	// param2
	lca	argc,-4(x1)	// param1
//	lca	zz,-8(x1)	// return value
	bs	fz,x1
	b	_main
	h	4(x1)	// point to return value

	.data
fz:	.bin	12

// a fake argv...
argv:	.word	av

av:	.word	pgm
	.word	0

argc:	.bin	1#4

pgm::	.string	f:"foobar_"

stk:	.word	stack-4

	.bss
	.space	128
stack:	.space	0
