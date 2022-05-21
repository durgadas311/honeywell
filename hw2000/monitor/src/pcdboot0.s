// Primary bootstrap code, part 1, for card decks.
// This module clears punctuation for all subsequent cards.
// It clears from boot+80 to 04000, which must be enough to
// cover all subsequent boot cards (not BRF cards).
// Bootstrap this into 01620 (octal) and RUN
	.admode	2
	.globl	boot
	.globl	pcdboot0
	// this will be preceeded by SW/SI instructions
pcdboot0:
2:	exm	2b+1,boot+80,016	// any char w/o punc
	scr	2b+1+@+^,070	// modify EXM B-addr
	bbe	1f,2b+1+@,040	// hardcoded to stop at 04000
	b	2b
1:
