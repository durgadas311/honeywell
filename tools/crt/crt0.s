// We assume our "parent" setup X1 to "top of memory",
// or at least top of _our_ memory. Must have set WM.

// loc  65-67  = revision (3 chars) (WM)
// loc  68-75  = prog/seg (8 chars) (WM,WM)
// loc 113-118 = visibility (6 chars) (WM)
// loc 186-189 = end of memory address (WM)

	.globl	_main
	.globl	@P4,@P8
	.globl	@mtop
start:
	bs	@P4,x1	// retreat inside our region
	lca	x1,x2
	lca	x1,@heap	// heap starts here
	lca	189,@mtop	// max usable memory
	// copy 8 chars from MOD1 PROG/SEQ location.
	// string is not punctuated, so must manually copy.
	lca	av,x5	// a.k.a. '&pgm'
	// prog/seg, loc 68-73
	exm	75,7(x5),021	// move SEG
	exm			// move PRG
	lca	av+4,x5	// a.k.a. '&rev'
	exm	67,2(x5),021	// move REV
	// revision, loc 65-67
	lca	av+8,x5	// a.k.a. '&vis'
	// visibility, loc 113-118
	exm	118,5(x5),021	// move VIS
	lca	argv,0(x1)	// param2 = argv
	lca	argc,-4(x1)	// param1 = argc
	bs	@P8,x1
	b	_main
	h	.	// don't allow continue

	.data
@heap:	.word	0
@mtop:	.word	0

// a fake argv...
argv:	.word	av

av:	.word	pgm
	.word	rev
	.word	vis
	// TODO: are program args possible?
	.word	0

argc:	.bin	3#4

// TODO: this should come from the "OS".
pgm::	.string	f:"foobaroo_"	// 8 char buffer, with RM
vis::	.string	f:"-00000_"	// 6 char buffer, with RM
rev::	.string	f:"000_"	// 3 char buffer, with RM
