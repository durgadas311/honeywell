// gets - read characters from the H200 console keyboard.

// args: char *, returns num chars
// Does not force newline, but if CR is pressed then newline happens

	.globl	_gets
	.text
_gets:
	scr	0(x1),070
	lca	4(x1),x5
	pdt	0(x5),012,047,0
	pcb	.,012,047,010
	scr	x5,002	// we know X5 has WM...
	scr	x6,012
	bs	x6,x5
	lcr	0(x1),077
