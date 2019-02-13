// divide two numbers, and return both quotient and remainder.
// param1 = numerator, param2 = denominator, param3 = (div_t *)
// 'div_t' is essentially an array of two ints, so the pointer
// points to:
//	+--------+
// p -> |  quot  |
//	+--------+
// +4   |  rem   |
//	+--------+
// which means it points to the *right* end of element [0].

	.globl	@div
	.globl	_div
	.text
_div:	
	scr	0(x1),070
	lcr	@div,064
	csm	8(x1),4(x1)
	lca	12(x1),x3
	lca	x5,0(x3)
	lca	x6,4(x3)
	lcr	0(x1),077
