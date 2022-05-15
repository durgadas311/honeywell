// primary bootstrap code for card decks.
// bootstrap this into 01620 (octal) and RUN
//	.org	01620	// done by linker
	.admode	3
	.globl	boot
	.globl	pcdboot1
pcdboot1:
	pdt	boot+80,011,041	// get rest of bootstrap
	pcb	.,011,041,010
//	pdt	boot+160,011,041	// get next monitor card (3)
//	pcb	.,011,041,010
//	pdt	boot+240,011,041	// get next monitor card (4)
//	pcb	.,011,041,010
	nop	// need to ensure WM terminates last instr
// continued in pcdboot2...
