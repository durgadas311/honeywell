// isrm - check if RM at location

// args: ptr, returns != 0 if RM

	.globl	_isrm
	.text
_isrm:
	scr	0(x1),070
	lca	4(x1),x5
	bcc	1f,0(x5),030
	bs	x5
	lcr	0(x1),077
1:	// x5 can't be zero?
	lcr	0(x1),077
