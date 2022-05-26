// bootstrap loader for BRF data on tape
// The bootstrap code is in "BRF subset" format, banner=042
	.admode	2
// preceeded by SW instructions
header	=	00400	// a safe place for buffer

	.globl	mtiboot0,nextr,header,skip
	.globl	mtiboot1,start1,eot	// handler for EOT
	.text
mtiboot0:
	// clear punc/data at header (512 chars).
	// 'header' must be 01000.
1:	exm	.+1,header,017	// any char w/o punc...
	scr	1b+1+@+^,070	// next addr
	bbe	1b,1b+1+@,004	// loop while addr 00400-00777
	sst	.+1,2f+@+2,040	// force output device for EOT detect
	nop		// TODO: what goes here
	.byte	0

// load segment from tape
// todo: allow selection of drive unit
// expected to be at 0061... according to Tape Monitor instructions
// may require hand-tuning.
nextr:	pdt	header,011,040,060	// load header data (0065: 040)
	pcb	.,011,040,000		// (0073: 040)
2:	pcb	eot,011,040,060		// (0101: 040) branch if EOT
skip:	nop	start1	// changed to B to skip SWs
