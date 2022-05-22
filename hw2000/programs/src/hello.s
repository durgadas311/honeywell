// Simple "Hello World" program that also displays
// the program name and segment from the communications
// area.
	.admode	3

	cam	040
	mcw	75,sg
	mcw	73,pg
	pdt	hello,011,07,01
	pcb	.,011,07,010
	b	(139)
	nop

	.data
hello::	.string	"HELLO WORLD, I AM "
pg:	.string	"      "
	.string	"/"
sg:	.string "  "
	.string	f:" "
