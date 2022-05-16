// secondary bootstrap code for card decks.
// linked with pcdboot1 after prefixing with SW/SI module
load	=	0224	// load address for monitor code
ncards	=	6
//
	.admode	3
	.globl	pcdboot2
pcdboot2:
1:	pdt	(ptr-^),011,041	// 
	pcb	.,011,041,010
	ba	c80,ptr
	bs	one,cnt
	c	zero,cnt
	bct	1b,045
	b	2f	// fall-through to loader SW/SI

// Because these are not .data, must reverse symbol anchors
c80::	.bin	80#2
cnt:	.bin	6#1	// TODO: auto-count how many cards
one:	.bin	1#1
zero:	.bin	0#1
ptr::	.word	load

2:
