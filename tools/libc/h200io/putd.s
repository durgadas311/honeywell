// putd - print int as decimal string to console (leading 0 supression)
// lputd - print int as decimal string to line printer

// args: int

	.globl	@zero,@one,@div
	.globl	_putd,_lputd,_putwd,_lputwd
	.text
_lputd:
	scr	0(x1),070
	b	itoa
	// 'op' points to first non-zero digit
	pdt	(opi),011,02,0
	pcb	.,011,02,010
	lcr	0(x1),077

_lputwd:
	scr	0(x1),070
	b	itoa
	pdt	oc,011,02,0
	pcb	.,011,02,010
	lcr	0(x1),077

_putd:
	scr	0(x1),070
	b	itoa
	// 'op' points to first non-zero digit
	pdt	(opi),012,07,0
	pcb	.,012,07,010
	lcr	0(x1),077

_putwd:
	scr	0(x1),070
	b	itoa
	pdt	oc,012,07,0
	pcb	.,012,07,010
	lcr	0(x1),077

itoa:	scr	itoax,070
	lca	4(x1),in
	lcr	@div,064
	bs	ocx
	lca	oop,op
1:	csm	ten,in		// in / 10
	exm	x6,(opi),001	// *opi = in % 10
	lca	x5,in		// in /= 10;
	c	@zero,in
	bct	2f,042		// if 0, we're done
	bcc	2f,(opi),010	// or WM - end of field
	bs	@one,op
	b	1b
2:
itoax::	b	0	// return


	.data
in:	.bin	0#4

oc:
ocx:	.string "12345678"
	.string	f:"_"	// term for PDT
opi:	// for indirect ref to "op"
op:	.word	0
oop:	.word	ocx
ten:	.bin	012#4	// 10, as int
