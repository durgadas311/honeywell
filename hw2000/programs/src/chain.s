// Program to get input from console and call
// monitor with search parameters.
	.admode	3
	cam	040
loop:
	pdt	prompt,011,007,000
	pcb	.,011,007,010
	// get input
	pdt	buffer,011,047,001
	pcb	.,011,047,010
	bce	loop,buffer,' '
	lca	buffer+5,73
	bce	noseg,buffer+6,' '
	lca	buffer+7,75
noseg:
	lca	search,111
	b	130
	nop

	.data
search:	.bin	020	// search mode
prompt:: .string	f:"ENTER PROGRAM:  "
buffer:
	.string	"      "
	.string	"  "
	.string	f:" "
