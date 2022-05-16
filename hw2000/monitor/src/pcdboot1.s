// primary bootstrap code for card decks.
// bootstrap this into 01620 (octal) and RUN
//	.org	01620	// done by linker
	.admode	3
	.globl	boot
	.globl	pcdboot1
pcdboot1:
	pdt	boot+80,011,041		// get rest of bootstrap (2)
	pcb	.,011,041,010
	pdt	boot+160,011,041	// get next monitor card (3)
	pcb	.,011,041,010
	pdt	boot+240,011,041	// get next monitor card (4)
	pcb	.,011,041,010
	pdt	boot+320,011,041	// get next monitor card (5)
	pcb	.,011,041,010
	pdt	boot+400,011,041	// get next monitor card (6)
	pcb	.,011,041,010
	pdt	boot+480,011,041	// get next monitor card (7)
	pcb	.,011,041,010
// continued in pcdboot2...
