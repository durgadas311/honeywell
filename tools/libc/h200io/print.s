// print - send a string to the H200 console.
// string must have a RM.

// remember: C prepends a '_' to names.
// args: string, flags
PR_NL=001
PR_SPB=002
PR_SPA=004

	.text
_print:
	scr	0(x1),070
	bbe	1f,8(x1),PR_SPB	// test space before flag
	pdt	sp,012,07,0
	pcb	.,012,07,010
	// TODO: implement "space after"
1:	bbe	3f,8(x1),PR_NL	// test NL flags
	pdt	4(x1),012,07,0
	b	2f
3:	pdt	4(x1),012,07,1
2:	pcb	.,012,07,010
	lcr	0(x1),077

	.data
sp::	.string	f:"  "
