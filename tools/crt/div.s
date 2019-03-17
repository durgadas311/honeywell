// implement divide on 'int', quotient in x5, remainder in x6
// B / A => X5 r X6

// this is a CSM call, but stack (x1) should still be usable.
	.globl	@P0,@P1,@P6
	.globl	@div,@udiv
	.text
9:	csm	// return, but keep CSR pointing here
?udiv:	scr	x3,067	// save AAR (ptr to denom, HL)
	scr	x4,070	// save BAR (ptr to num, DE)
	bs	sn
	bs	sd
	b	8f

9:	csm	// return, but keep CSR pointing here
?div:	scr	x3,067	// save AAR (ptr to denom, HL)
	scr	x4,070	// save BAR (ptr to num, DE)
	sst	-3(x4),sn,040	// numerator sign (DE)
	sst	-3(x3),sd,040	// denominator sign (HL)
	ha	sn,sd		// sd = signs differ
8:	bs	num
	bs	den
	bbe	5f,sd,040	// signs differ?
	ba	0(x4),num
1:	bbe	6f,sn,040	// divisor negative?
	ba	0(x3),denh
2:
	bs	ct
	ba	bc,ct	// ct = 24

// If this were not so expensive, we could
// optimize the division. main problem is "num <<= 6".
.if 0	// while (ct > 6 && den > (num << 6)) {
3:	c	ct,@P6
	bct	6f,046
	c	den-1,num
	bct	6f,046
	bs	@P6,ct	// ct -= 6
	ba	num	// num <<= 6
	ba	num
	ba	num
	ba	num
	ba	num
	ba	num
	b	3b
.endif	// }

			// do {
6:	ba	num	// num <<= 1
	c	den,num	//
	bct	3f,041	// if (den <= num) {
	bs	den,num	//	num -= den
	ba	@P1,num //	num += 1
3:	bs	@P1,ct
	c	@P0,ct
	bct	6b,045	// } while (--ct > 0)

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
@udiv:	.word	?udiv

// double-precision register space
numh:	.word	0	// HL:DE
num:	.word	n:0

denh:	.word	0	// BC:00
den:	.word	n:0

sn:	.bin	0#1
sd:	.bin	0#1

ct:	.bin	0#1
bc:	.bin	24#1
