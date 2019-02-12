// implement divide on 'int', quotient in x5, remainder in x6
// B / A => X5 r X6

// this is a CSM call, but stack (x1) should still be usable.
	.globl	@one,@zero
	.globl	@div
	.text
9:	csm	// return, but keep CSR pointing here
?div:	scr	x3,067	// save AAR (ptr to num shifts)
	scr	x4,070	// save BAR (ptr to value)
	bs	wk1
	sst	-3(x4),sb,040	// B sign (DE)
	sst	-3(x3),sa,040	// A sign (HL)
	ha	sb,sa		// sa = signs differ
	bbe	5f,sa,040	// signs differ?
	ba	0(x4),wk1
1:	bbe	6f,sb,040	// divisor negative?
	ba	0(x3),wk2h
2:
	// TODO: optimize by skipping chars that are 0
	bs	ct
	ba	bc,ct

6:	ba	wk1	// <<= 1
	c	wk2,wk1	//
	bct	3f,041
	bs	wk2,wk1
	ba	@one,wk1
3:	bs	@one,ct
	c	@zero,ct
	bct	6b,045
	bbe	6f,sa,040	// signs differ?
	lca	wk1h,x6		// remainder
	mcw	wk1,wk2h
	lca	wk2h,x5		// quotient
	b	9b

4:	bs	0(x3),wk2h	// negate divisor
	b	2b

5:	bs	0(x4),wk1	// negate dividend
	bs	wk1h
	b	1b

6:	bs	wk2h
	bs	wk1,wk2h	// -DE
	lca	wk2h,x5		// quotient
	bs	wk2h
	bs	wk1h,wk2h	// -HL
	lca	wk2h,x6		// remainder
	b	9b

	.data
@div:	.word	?div

// double-recision register space
wk1h:	.word	0	// HL:DE
wk1:	.word	n:0

wk2h:	.word	0	// BC:00
wk2:	.word	n:0

sb:	.bin	0#1
sa:	.bin	0#1

ct:	.bin	0#1
bc:	.bin	24#1
