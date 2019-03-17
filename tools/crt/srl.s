// implement >>> (shift right) on 'int', result in x5

	.globl	@P0
	.globl	@srl
	.text
9:	csm	// return, but keep CSR pointing here
?srl:	scr	x3,067	// save AAR (ptr to num shifts)
	scr	x4,070	// save BAR (ptr to value)
	lca	0(x3),x3
	c	@P0,x3
	bct	1f,042
	lca	0(x4),num
	bs	dum		// must clear out garbage
	exm	x3,2f+2,001	// num bits into place
	fma	fpn,00,02
2:	bms	05,00	// v2 replaced at runtime
	fma	fpn,00,00
	lca	num,x5
	b	9b

1:	lca	0(x4),x5	// trivial case
	b	9b

	.data
@srl:	.word	?srl

// Logical shift, separate data from sign bit.
dum:	.bin	0#2	// top end of mantissa
num:	.bin	0#4	// where to splice into
fpn:	.bin	0#2	// exponent
