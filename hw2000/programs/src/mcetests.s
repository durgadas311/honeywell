// program to run mce examples from manual
//	org   1340
	.admode	3
	.text
start:	cam	40
//
loop:	lca	(tabptr-^),fmtptr
	bs	wlen,tabptr
	lca	(tabptr-^),inpptr
	bs	wlen,tabptr
	lca	(tabptr-^),resptr
	bs	wlen,tabptr
	b	run1
	pdt	col1,011,002,000
	pcb	.,011,002,000
	pdt	(resptr-^),011,002,000
	pcb	.,011,002,000
	pdt	expect,011,002,002
	pcb	.,011,002,000
	s	one,tabctr
	bct	done,060
	b	loop
//
done:	b	(139)
//
run1:	scr	1f,070
	lca	(fmtptr-^),format
	scr	prtptr,070
	ba	one,prtptr
	lca	(inpptr-^),numfmt
	scr	numptr,070
	ba	one,numptr
	pdt	col1,011,002,000
	pcb	.,011,002,000
	pdt	(prtptr-^),011,002,000
	pcb	.,011,002,000
	pdt	col3,011,002,000
	pcb	.,011,002,000
	pdt	(numptr-^),011,002,000
	pcb	.,011,002,000
	pdt	col1,011,002,001
	pcb	.,011,002,000
	mce	numfmt,format	// test the format
	pdt	col1,011,002,000
	pcb	.,011,002,000
	pdt	(prtptr-^),011,002,000
	pcb	.,011,002,000
	pdt	col1,011,002,001
	pcb	.,011,002,000
1::	b	0
	nop		// terminate last instruction

	.data
//
format:	.space	32
	.bin	f:' '#1
numfmt:	.space	16
	.bin	f:' '#1
prtptr:	.word	0
numptr:	.word	0
fmtptr:	.word	0
inpptr:	.word	0
resptr:	.word	0
col3::	.string	f:": :x"
col1	=	.-2
expect::	.string	f:": expectedx"
tabptr:	.word	table
tabctr:	.dec	010
//
one:	.bin	1#1
wlen:	.bin	@#1
//
ex1num:		.dec	-0000099
ex1fmt:		.string	m:"   ,  0.  ??-x"
ex1res::	.string	f:"       .99  -x"
ex1nua:		.dec	+0000099
ex1rea::	.string	f:"       .99   x"
//
ex2num:		.dec	+25454986
ex2fmt:		.string	m:"   ?  ?   x"
ex2res::	.string	f:"254 54 986x"
//
ex3num:		.dec	+000450
ex3fmt:		.string	m:"$ ,  0.  ?~*x"
ex3res::	.string	f:"$    4.50  *x"
ex3nua:		.dec	-000450
ex3rea::	.string	f:"$    4.50 ~*x"
//
ex4num:		.dec	+00897445
ex4fmt:		.string	m:"    , $0.  x"
ex4res::	.string	f:"  $8,974.45x"
//
ex5num:		.dec	+0010450
ex5fmt:		.string	m:"  , *0.  x"
ex5res::	.string	f:"***104.50x"
//
ex6num:		.dec	+343
ex6fmt:		.string	m:"  .  ~x"
ex6res::	.string	f:" 3.43 x"
//
ex7num:		.string	" 0001343"
ex7fmt:		.string	m:"     $0 .  ?~x"
ex7res::	.string	f:"     $13.43  x"
ex7nua:		.string " 0000043"
ex7rea::	.string	f:"      $0.43  x"
//
	.word	ex7rea
	.word	ex7nua
	.word	ex7fmt-1
//
	.word	ex7res
	.word	ex7num
	.word	ex7fmt-1
//
	.word	ex6res
	.word	ex6num
	.word	ex6fmt-1
//
	.word	ex5res
	.word	ex5num
	.word	ex5fmt-1
//
	.word	ex4res
	.word	ex4num
	.word	ex4fmt-1
//
	.word	ex3rea
	.word	ex3nua
	.word	ex3fmt-1
//
	.word	ex3res
	.word	ex3num
	.word	ex3fmt-1
//
	.word	ex2res
	.word	ex2num
	.word	ex2fmt-1
//
	.word	ex1rea
	.word	ex1nua
	.word	ex1fmt-1
//
	.word	ex1res
	.word	ex1num
table:	.word	ex1fmt-1
