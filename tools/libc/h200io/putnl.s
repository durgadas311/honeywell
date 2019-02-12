// putnl - force a newline on the H200 console.

// remember: C prepends a '_' to names.
// args: none

	.globl	_putnl
	.text
_putnl:
	scr	0(x1),070
	pdt	nn,012,07,01
	pcb	.,012,07,010
	lcr	0(x1),077

	.data
nn::	.string	c:" "
