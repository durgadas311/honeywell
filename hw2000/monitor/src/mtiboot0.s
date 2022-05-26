// bootstrap loader for BRF data on tape
	.admode	2
// by the time we reach here, we are
// well above index register storage.
// (SW/SI instructions)
header	=	01000	// a safe place from LCAs

	.globl	mtiboot0,nextr,header
	.text
mtiboot0:
	// clear punc/data at header (512 chars).
	// 'header' must be 01000.
1:	exm	.+1,header,017	// any char w/o punc...
	scr	1b+1+@+^,070	// next addr
	bbe	1b,1b+1+@,010	// loop for 01000-01777
	nop	0,0	// TODO: what goes here
	nop	0	// TODO: what goes here

// load segment from tape
// todo: allow selection of drive unit
// expected to be at 0061... according to Tape Monitor instructions
// may require hand-tuning.
nextr:	pdt	header,011,040,060	// load header data (0065: 040)
	pcb	.,011,040,000		// (0073: 040)
	pcb	.,011,040,060		// (0101: 040) branch if BOT? EOT?

