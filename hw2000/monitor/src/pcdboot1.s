// primary bootstrap code for card decks.
// bootstrap this into 01620 (octal) and RUN
ncards	=	5	// number of *additional* cards (BOOTSTRAP read 1)
//	.org	01620	// done by linker
	.admode	3
	.globl	boot
	.globl	one,zero,c80	// for use by pcdboot2
	.globl	pcdboot1
	// this will be preceeded by SW/SI instructions
pcdboot1:
1:	pdt	(ptr-^),011,041	//
	pcb	.,011,041,010
	ba	c80,ptr
	bs	one,cnt
	c	zero,cnt
	bct	1b,045
	b	2f	// fall-through to pcdboot2 SW/SI

// Because these are not .data, must reverse symbol anchors
c80::	.bin	80#2
one:	.bin	1#1
zero:	.bin	0#1
cnt:	.bin	ncards#1
ptr::	.word	boot+80

2:
