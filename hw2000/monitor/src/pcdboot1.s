// Primary bootstrap code, part 2, for card decks.
ncards	=	6	// number of *additional* cards (BOOTSTRAP read 1)
	.admode	2
	.globl	boot
	.globl	one,zero,c80	// for use by pcdboot2
	.globl	pcdboot1
	// this will be preceeded by SW/SI instructions
pcdboot1:
1:	pdt	boot+80,011,041	//
	pcb	.,011,041,010
	ba	c80,1b+^+1	// OK if no overflow
	// above must all fit before boot+80
	bs	one,cnt
	c	zero,cnt
	bct	1b,045
	b	2f	// fall-through to pcdboot2 SW/SI

// Because these are not .data, must reverse symbol anchors
c80::	.bin	80#2
one:	.bin	1#1
zero:	.bin	0#1
cnt:	.bin	ncards#1

2:
