// bootstrap code for card decks.
// linked with brdldr (prefixed by SW/SI module)
	.admode	3
	.globl	boot	// bootstrap address, base address
	.globl	pcdboot2
pcdboot2:
1:	pdt	(ptr-^),011,041	// 
	pcb	.,011,041,010
	ba	eighty,ptr
	bs	one,cnt
	c	zero,cnt
	bct	1b,045
	b	2f

// Because these are not .data, must reverse symbol anchors
eighty::	.bin	80#2
cnt:	.bin	10#1	// TODO: auto-count how many cards
one:	.bin	1#1
zero:	.bin	0#1
ptr::	.word	boot+160

2:	// must be more SW/SI instr...
