// implement << (shift left)

// this is a CSM call, but stack (x1) should still be usable.
// Arguments must not be actual constants!
// We don't know the lengths of either arg...
// This routine only works for INT (or smaller).
	.text
@sla:
	scr	x3,067	// save AAR (ptr to num shifts)
	scr	x4,070	// save BAR (ptr to value)
	bs	sh
	ba	0(x3),sh // set zero-balance
2:	bct	1f,060
	ba	0(x4)
	bs	c1,sh
	b	2b
1:	// result was shifted in-place
	csm
	.data
c1:	.bin	1#1
sh:	.bin	0#1	// value must be reasonable...
vv:	.bin	0#4	// never more than a int
