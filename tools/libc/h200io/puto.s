// puto - print int as octal string (leading 0 supression)

// args: int

	.globl	_puto
	.text
_puto:
	scr	0(x1),070
	lca	4(x1),in
	bs	ocx
	lca	oop,op
1:	sst	in,(opi),07
	// ugly "x >> 3" by doing ((x >> 6) << 3)
	mcw	in,ins	// >> 6 (1 char)
	ba	ins	// << 1
	ba	ins	// << 1
	ba	ins	// << 1
	bct	2f,060	// if 0, we're done
	ba	one,op
	b	1b

2:	// 'op' points to first non-zer digit
	pdt	oc,012,07,0
	pcb	.,012,07,010
	lcr	0(x1),077

	.data
in:	.bin	0#4
ins:	.bin	n:0#1	// space to over-shift

oc:
ocx:	.string "12345678"
	.string	f:"_"	// term for PDT
opi:	// for indirect ref to "op"
op:	.word	0
oop:	.word	ocx
one:	.bin	1#1
