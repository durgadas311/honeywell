// bootstrap loader for BRF data on tape
	.admode	2
// preceeded by SW instructions
loader	=	1340	// a likely place to load code
			// must match ORG used in link

	.globl	mtiboot1,start1,eot
	.globl	nextr,header,skip
	.text
mtiboot1:
// assert: banner == 042
// assert: every record has valid termination control
//
// load program data from header
// parse next brt command
// assert: ctlc == 000-017,021-037, or 077 or 061
	sst	b,skip,077
start1:
	cw	header+8	// NOP, but AAR=header+7
	scr	1f+1+^,067	// reset AAR for new record
next:
1:	exm	header+7,ctlc,011
	scr	2f+1+^,067	// carry fwd next addr (data)
	scr	3f+1+^,067	// addr of first char in string
	scr	4f+1+^,067	// addr of first char in string
	bce	loader,ctlc,061	// end load
	bce	nextr,ctlc,077	// end of record
	bbe	setw,ctlc,020	// WM requested
nextc:
2:	exm	header+8,loader,017
	scr	1b+1+^,067	// carry fwd next A addr
	scr	2b+1+^,067	// carry fwd next A addr
	scr	2b+1+@+^,070	// carry fwd next B addr
	bs	one,ctlc
	bbe	nextc,ctlc,017	// loop until count == 0
4:	cw	loader		// repair punctuation in buffer
	b	next		// now get next control char
	// TODO: need punc
setw:
3:	sw	loader		// force punctuation before copy
	b	nextc

eot:	h	nextr	// TODO: handle EOT error
//
	.data
one:	.bin	1#1
b:	.byte	065	// B(ranch) opcode
//
ctlc:	.bin	0#1	// control char
//
