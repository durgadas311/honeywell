// print - send a string to the H200 console.
// string must have a RM.

// remember: C prepends a '_' to names.
// args: string, flags
PR_NL=001
PR_SPB=002
PR_SPA=004

	.globl	_print
	.text
_print:
	scr	0(x1),070
	lca	4(x1),x5
	bbe	4f,8(x1),PR_SPB	// test space before flag
1:	bbe	5f,8(x1),PR_SPA	// test space after flags
	bbe	3f,8(x1),PR_NL	// test NL flags
5:	pdt	0(x5),012,07,0
	b	2f
3:	pdt	0(x5),012,07,1
2:	pcb	.,012,07,010
	bbe	6f,8(x1),PR_SPA	// test space after flags
5:	lcr	0(x1),077

4:	pdt	sp,012,07,0
	pcb	.,012,07,010
	b	1b

6:	pdt	sp,012,07,1
	pcb	.,012,07,010
	b	5b

	.data
sp::	.string	f:"  "
