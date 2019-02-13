// implement divide on 'int', quotient in x5, remainder in x6
// B / A => X5 r X6

// this is a CSM call, but stack (x1) should still be usable.
	.globl	@one,@zero
	.globl	@div
	.text
9:	csm	// return, but keep CSR pointing here
?div:	scr	x3,067	// save AAR (ptr to denom, HL)
	scr	x4,070	// save BAR (ptr to num, DE)
	bs	num
	bs	den
	sst	-3(x4),sn,040	// B sign (DE)
	sst	-3(x3),sd,040	// A sign (HL)
	ha	sn,sd		// sd = signs differ
	bbe	5f,sd,040	// signs differ?
	ba	0(x4),num
1:	bbe	6f,sn,040	// divisor negative?
	ba	0(x3),denh
2:
	// TODO: optimize by skipping chars that are 0
	bs	ct
	ba	bc,ct

6:	ba	num	// <<= 1
	c	den,num	//
	bct	3f,041
	bs	den,num
	ba	@one,num
3:	bs	@one,ct
	c	@zero,ct
	bct	6b,045
	bbe	6f,sd,040	// signs differ?
	lca	numh,x6		// remainder
	mcw	num,denh
	lca	denh,x5		// quotient
	b	9b

4:	bs	0(x3),denh	// negate divisor
	b	2b

5:	bs	0(x4),num	// negate dividend
	bs	numh
	b	1b

6:	bs	denh
	bs	num,denh	// -DE
	lca	denh,x5		// quotient
	bs	denh
	bs	numh,denh	// -HL
	lca	denh,x6		// remainder
	b	9b

	.data
@div:	.word	?div

// double-recision register space
numh:	.word	0	// HL:DE
num:	.word	n:0

denh:	.word	0	// BC:00
den:	.word	n:0

sn:	.bin	0#1
sd:	.bin	0#1

ct:	.bin	0#1
bc:	.bin	24#1
