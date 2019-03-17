// void tran(char *d, char *s, void *tr, int op)
// string 's' must have a RM.

	.globl	@P1
	.globl	_tran
	.text
_tran:
	scr	0(x1),070
	lca	4(x1),x6	// dest
	lca	8(x1),x5	// src
	mcw	12(x1),1f+12	// set trans tbl
	exm	16(x1),1f+13,01	// set variant
	lca	@err,064	// setup CSR in case
1:	mit	0(x5),0(x6),0,0	// filled in above
	scr	x5,070	// BAR = end of dest, RM+1
	bs	@P1,x5
	si	0(x5)		// set RM
	sw	0(x5)		// set RM
2:	bs	4(x1),x5	// length of translation
	lcr	0(x1),077

err:	scr	x5,070	// BAR = end of tran (nxt chr)
	b	2b

	.data
@err:	.word	err
