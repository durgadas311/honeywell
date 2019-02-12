// We assume our "parent" setup X1 to "top of memory",
// or at least top of _our_ memory. Must have set WM.

	.globl	_main
	.globl	@zero,@one,@two,@four,@eight,@twlv
start:
	bs	@four,x1	// retreat inside our region
	lca	x1,x2
	// copy 8 chars from MOD1 PROG/SEQ location.
	// string is not punctuated, so must manually copy.
	lca	av,x5	// a.k.a. 'pgm'
	exm	68,0(x5),011	// 1 char, data bits only, ++
	exm			// next char, ...
	exm			//
	exm			//
	exm			//
	exm			//
	exm			//
	exm			// eighth char.
	lca	argv,0(x1)	// param2 = argv
	lca	argc,-4(x1)	// param1 = argc
	bs	@eight,x1
	b	_main
	h	.	// don't allow continue

	.data
// Public constants - make these "clean" ints
// However, LCA when dest is short/char is problematic
@zero:	.bin	0#4
@one:	.bin	01#4
@two:	.bin	02#4
@four:	.bin	04#4
@eight:	.bin	010#4
@twlv:	.bin	014#4

// a fake argv...
argv:	.word	av

av:	.word	pgm
	// TODO: are program args possible?
	.word	0

argc:	.bin	1#4

// TODO: this should come from the "OS".
pgm::	.string	f:"foobaroo_"	// 8 char buffer, with RM
