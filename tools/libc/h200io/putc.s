// putc - send a character to the H200 console.
// NOTE: cannot send \n or any other "std" control characters.

// remember: C prepends a '_' to names.
// args: char (as int)

	.globl	_putc
	.text
_putc:
	scr	0(x1),070
	exm	4(x1),ch,001
	pdt	ch,012,07,0
	pcb	.,012,07,010
	lcr	0(x1),077

	.data
ch::	.string	f:"  "
