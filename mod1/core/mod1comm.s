// MOD1 communications area template (defaults).

	.admode	4
	.globl	_setcom
	.text

// The communications block is both .text and .data,
// but must choose one, so keep in .text segment.
base	=	075
	// .org 075
comm:
	.space	1	// *** reserved ***
	.bin	0#1	// relo bank ind
	.bin	0#1	// boot pcu
	.bin	0#1	// EX dev
	.string	"000"	// REV
	.string	"PROGNM" // PROG name
	.string	"SG"	// SEG
	.space	1	// *** reserved ***
	.string	"        "	// halt name
	.space	1	// *** reserved ***
	mc	// emerg exit - enter supervisor
	.byte	0100	// prog exit code
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	.bin	0#4	// exit to own-code - TODO:
	.space	1	// *** reserved ***
	.bin	0#3	// relo augment
	.bin	' '#1	// *** reserved ***
	.bin	020#1	// search mode
	.bin	'N'#1	// start mode
	.string	"-00000"	// visibility mask
	.bin	0#3	// spcl start loc
	.bin	0#4	// own-code ret before dist - TODO:
	.bin	0#4	// own-code ret after dist - TODO:
	.admode	3
	scr	2f+3-comm+base,070 // 3-chr seg load entry
	b	1f-comm+base	//
	.word	3f-comm+base	// 3-chr exit (indir)
	.admode	4
	.bin	0#5	// current date
	.bin	0#1	// trapping mode
	.space	7	// *** reserved ***
	.bin	0#1	// oper ctl file dev
	.space	8	// *** reserved ***
	.word	3f-comm+base	// 4-chr exit (indir)
	.word	4f-comm+base	// 4-chr seg load (indir)
	.space	15	// *** reserved ***
	.bin	0#3	// memory limit
//	--- end of "visible" comm area...

// TODO: require/use stack for return addr?
// 3-chr segment load entry
1:	mc	// enter supervisor
	.byte	0101	// segment load code
	b	(2f+1)	// in 3-chr mode now...
2:	.word	0

// 3-chr or 4-chr prog exit
3:	mc	// enter supervisor
	.byte	0100	// exit code
	// does not return

// 4-chr segment load entry
4:	scr	2b+3,070
	mc	// enter supervisor
	.byte	0101	// segment load code
	b	(2b)

//	--- end of comm template...
	.bin	b:0#1	// termination for EXM

// setcom(void *base)
// Copy MOD1 comm area template into user program area.
// 'base' is base addr of prog segment,
// as used to set BRR.
_setcom:
	scr	0(x1),070
	lca	4(x1),x5
	ba	commb,x5
	exm	comm,0(x5),077 // must force all punc
	lcr	0(x1),077

	.data
commb:	.word	base	// start of comm area
