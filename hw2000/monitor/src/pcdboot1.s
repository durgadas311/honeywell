// bootstrap code for card decks.
// linked with brdldr (prefixed by SW/SI module)
	.admode	3
	.globl	boot	// bootstrap addres, base address
	.globl	pcdboot1
pcdboot1:
	pdt	boot+80,011,041	// get another card (2)
	pcb	.,011,041,010
	nop	// need to ensure WM terminates PCB
	// continue on with pcdboot2... SW/SI
