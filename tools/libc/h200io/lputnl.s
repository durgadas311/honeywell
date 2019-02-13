// lputnl - force a newline on the H200 line printer.

// remember: C prepends a '_' to names.
// args: none

	.globl	_lputnl
	.text
_lputnl:
	scr	0(x1),070
	pdt	nn,011,02,01
	pcb	.,011,02,010
	lcr	0(x1),077

	.data
nn::	.string	c:" "
