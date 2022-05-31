// Re-creation of H200/2000 version of MACHIN w/o extreme optimizations.
// From "The Pi Factory - RoB S 2013"
// http://www.honeypi.org.uk/ and specifically
// http://www.honeypi.org.uk/bigimage/PiFACTORY.CPP
//
numdp	=	775	// target num dec places

// How the arccot1[][] array is represented using index registers
// arccot1[][] layout w.r.t. x2 (x1)
// arccot1[m]		x2
// sizeof(arccot1[m])	30
// arccot1[m][0]	0(x2)
// arccot1[m][1]	-10(x2)
// arccot1[m][2]	-20(x2)
// arccot1[m+1][0]	-30(x2)
// arccot1[m+1][1]	-40(x2)
// arccot1[m+1][2]	-50(x2)
// WM[m]:		-9(x2),
//			-19(x2),
//			-29(x2)
// WM[m+1]:		-39(x2),
//			-49(x2),
//			-59(x2)
	.admode	3
	.text

	cam	040
	lca	topp,x1	// arccot1[1][...] - init WM
	lca	x1,x2
	sw	-9(x1),-19(x1)
	sw	-29(x1)		// WMs for arccot1[1][*]
	s	pi_dig	// zero
	// initial pass uses init constants
	bs	_term1
	bs	_term2
	bs	_term2a
	bs	_n	// n = 0;
	za	d80,divd1	// dividend1 = 80;
	za	d956,divd2	// dividend2 = 956;
	b	subr1
// main loop "for (n = 0; n < DECIMALPLACES + 10; n++) {"
// actually, "for (n = 1;..." since pass 0 done above.
1:
	c	ndp,_n
	bct	done,046
	ba	c1,_n	// n++
	// TODO: break loop when out of space...
	bs	mulbuf	// zero buffer
	za	pi_dig,mulmul
	m	d10,mulbuf
	mcw	mulbuf,pi_dig	// shift left, i.e. (10 * pi_dig) % 10^10
	bs	mulbuf	// zero buffer
	za	-10(x1),mulmul
	m	d10,mulbuf
	mcw	mulbuf,divd1	// dividend1 = 10 * arccot1[1][1];
	bs	mulbuf	// zero buffer
	za	-20(x1),mulmul
	m	d10,mulbuf
	mcw	mulbuf,divd2	// dividend2 = 10 * arccot1[1][2];
	b	subr1
	// first 9 digits are '0'
	c	c9,_n
	b	1b,041	// n < 9
	mcw	pi_dig0,_ch
	pdt	_ch,011,002,000
	pcb	.,011,002,010
	b	1b,045	// n != 9
	mcw	dp,_ch
	pdt	_ch,011,002,000
	pcb	.,011,002,010
	b	1b
done:
	pdt	_ch+1,011,002,001
	pcb	.,011,002,010
	b	(139)
	.data
_n:	.bin	0#6
_ch:	.bin	0#1	// digit to print
	.bin	f:' '	// RM for PDT
	.text

// subr1 - main body of computation
subr1:	scr	9f,070
	bs	_m
	lca	topp,x2	// &arccot1[m] - init WM
	ba	c30,x2	// there is no [0] element
	zs	d1,divs2n	// 2 * m - 1 for m=0 is -1
	// divs2n = {1,3,5,7,...}
	// for (m = 1; m <= terms1 || dividend1 > 0; m++ ) {
1:
	c	botp,x2
	bct	2f,041	// stop if out of sapce...
	ba	c1,_m	// m++ (m = 1)
	a	d2,divs2n	// 1,3,5,7,...
	bs	c30,x2	// &arccot1[m]
	sw	-39(x2),-49(x2)
	sw	-59(x2)		// WMs for arccot1[m+1][*]
	c	_m,_term1
	bct	5f,046	// stay in loop if m <= terms1
	// compare of signed decimal does not work!
	bcc	2f,divd1,006	// exit if negative
	c	d0,divd1
	bct	2f,042	// exit loop if dividend1 == 0
5:
	// quotient1 = dividend1 / 25;
	// arccot1[m][1] = dividend1 % 25;
	za	divd1,divrem	// zero rest of field
	d	d25,divbuf
	mcw	divquo,quot1	// quotient
	mcw	divrem,-10(x2)	// remainder
	// dividend1 = 10 * arccot1[m + 1][1] + quotient1;
	za	-40(x2),mulmul
	m	d10,mulbuf
	mcw	mulbuf,divd1	// 10 * arccot1[m + 1][1]
	a	quot1,divd1	// + quotient1
	// if (m <= terms2 || dividend2 > 0) {
	c	_m,_term2
	bct	6f,046
	// compare of signed decimal does not work!
	bcc	4f,divd2,006	// skip if negative
	c	d0,divd2
	bct	4f,042	// skip if == 0
6:
	// quotient2 = dividend2 / 57121;
	// arccot1[m][2] = dividend2 % 57121;
	za	divd2,divrem
	d	d57121,divbuf
	mcw	divquo,quot2	// quotient
	mcw	divrem,-20(x2)	// remainder
	// dividend2 = 10 * arccot1[m + 1][2] + quotient2;
	bs	mulbuf
	za	-50(x2),mulmul
	m	d10,mulbuf
	mcw	mulbuf,divd2	// 10 * arccot1[m + 1][2]
	a	quot2,divd2	// + quotient2
	bs	_term2a
	ba	_m,_term2a	// terms2a = m;
	s	quot2,quot1	// quotient1 -= quotient2;
4:	// }
	// divisor2n = 2 * m - 1; - loop variable, 1,3,5,7,...
	// dividend2n = 10 * arccot1[m][0] + quotient1 + 9 * divisor2n;
	bs	mulbuf	// zero buffer
	za	divs2n,mulmul
	m	d9,mulbuf
	mcw	mulbuf,divd2n	// 9 * divisor2n
	bs	mulbuf	// zero buffer
	za	0(x2),mulmul
	m	d10,mulbuf
	a	mulbuf,divd2n	// + 10 * arccot1[m][0]
	a	quot1,divd2n	// + quotient1
	// quotient2n = dividend2n / divisor2n - 9;
	// arccot1[m][0] = dividend2n % divisor2n;
	za	divd2n,divrem
	d	divs2n,divbuf
	mcw	divquo,quot2n	// quotient
	s	d9,quot2n
	mcw	divrem,0(x2)	// remainder
	// if (m % 2 == 0)
	bbe	3f,_m,001	// if (m % 2 != 0)
	s	quot2n,pi_dig	// piDigit[9] -= quotient2n;
	b	1b
3:	a	quot2n,pi_dig	// piDigit[9] += quotient2n;
	b	1b
	// }
2:	bs	_term1
	ba	_m,_term1
	bs	c1,_term1	// terms1 = m - 1;
	bs	_term2
	ba	_term2a,_term2	// terms2 = terms2a;
9::	b	0	// return from subr1
	.data	// local vars for subr1
_m:	.bin	0#6
_term1:	.bin	0#6
_term2:	.bin	0#6
_term2a: .bin	0#6
	.text

// global variables
	.data
pi_dig0:	// the MSD
pi_dig:	.dec	0#10

divd1:	.dec	0#10
divd2:	.dec	0#10
quot1:	.dec	0#10
quot2:	.dec	0#10
divs2n:	.dec	0#10
divd2n:	.dec	0#10
quot2n:	.dec	0#10
// space for division
divquo:	.dec	0#10
	.dec	n:0#1
divbuf:
divrem:	.dec	n:0#10
// space for multiplication
mulmul:	.dec	0#10
	.dec	n:0#1
mulbuf:	.dec	n:0#10

// constants
dp:	.bin	'.'#1
zadr:	.word	0
topp:	.word	top-1
botp:	.word	bottom+100	// this +100 needs tuning

c0:	.bin	0
c1:	.bin	1
c9:	.bin	9
c30:	.bin	30
ndp:	.bin	numdp+10	// loop count to acheive numdp

d0:	.dec	0#10
d1:	.dec	1#10
d2:	.dec	2#10
d9:	.dec	9#10
d10:	.dec	10#10
d25:	.dec	25#10
d80:	.dec	80#10
d956:	.dec	956#10
d57121:	.dec	57121#10

// Space for arccot1[][] array, cleared by loader.
// Each "row" of the array takes 30 chars.
// There is a padding needed to avoid trashing variables.
	.bss
bottom:
top:	.space	16800	// not finely tuned, but gets to Feynman Pt
