// secondary bootstrap code for card decks.
// linked with pcdboot1 after prefixing with SW/SI module
load	=	0234	// load address for monitor code
ncards	=	6	// TODO: auto-count how many cards
//
	.admode	2
	.globl	one,zero,c80
	.globl	pcdboot2
pcdboot2:
1:	pdt	load,011,041
	pcb	.,011,041,010
	ba	c80,1b+^+1	// OK if no overflow
	bs	one,cnt
	c	zero,cnt
	bct	1b,045
	b	2f	// fall-through to loader SW/SI

// Because these are not .data, must reverse symbol anchors
cnt:	.bin	ncards#1

2:
