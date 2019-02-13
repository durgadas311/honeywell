// lputc - send a character to the H200 line printer.
// NOTE: cannot send \n or any other "std" control characters.

// remember: C prepends a '_' to names.
// args: char (as int)

	.globl	_lputc
	.text
_lputc:
	scr	0(x1),070
	exm	4(x1),ch,001
	pdt	ch,011,02,0
	pcb	.,011,02,010
	lcr	0(x1),077

	.data
ch::	.string	f:"  "
