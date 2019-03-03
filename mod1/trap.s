// Interrupt (trap) entry into OS.
// includes MC (syscall).

// X1 = stack (kernel)
// X2 = frame (used?)	- trashed by C
// X3 = task base	- trashed by C

// mirrors struct task:
flags	=	3	// int (r)
sr	=	7	// int (r)
eiaar	=	11	// int (r)
eibar	=	15	// int (r)
iiaar	=	19	// int (r)
iibar	=	23	// int (r)
eivar	=	24	// char[5] (l)
iivar	=	29	// char[5] (l)
brr	=	34	// char[2] (l)
ibr	=	36	// char[2] (l)
time	=	47	// "char[10]" (10-chr int) (r)

	.globl	_syscal,_task,_endtsk,_runtsk
	.globl	@zero,@one,@two,@four,@eight,@twlv,@sxtn
	.globl	_tick

// EI handler, incl. MC (syscall)
eires:	lcr	eibar(x5),070
	lcr	eiaar(x5),067
	rvi	eivar(x5),035	// addr mode changes
	rnm
ei:	svi	075
eiind:	.byte	0,0,0,0,0100
	cam	060
	scr	aar,067
	scr	bar,070
	lca	^task,x5
	lca	aar,eiaar(x5)
	lca	bar,eibar(x5)
	exm	eiind,eivar(x5),031
	bbe	sc,eiind+4,020
	bcc	tick,eiind+4,020
//	...
	b	eires	// resume program

iires:	lcr	iibar(x5),070
	lcr	iiaar(x5),067
	rvi	iivar(x5),033
	rnm
ii:	svi	073
iiind:	.byte	0,0,0,0,0100
	cam	060
	scr	aar,067
	scr	bar,070
	lca	^task,x5
	lca	aar,iiaar(x5)
	lca	bar,iibar(x5)
	exm	iiind,iivar(x5),031
//	...
	b	iires	// resume program

// TODO: context switches require more...
// ^task is already setup...
sc:
	scr	eisr,076	// user's SR
	lca	eisr,sr(x5)
	ba	brr(x5),eisr-2
	lca	
	lca	@zero,0(x1)
	exm	(sr-3),0(x1),001
	ba	@one,sr
	lcr	sr,076	// point past code
	bs	@four,x1
	b	_syscal
	ba	@four,x1
	b	eires

tick:
	scr	atr,054		// save ATR
	ba	atrov,time(x5)
	ba	atr,time(x5)
	// TODO: possible context switch...
	b	_tick
	lcr	@zero,054	// zero ATR
	b	eires

// runtsk(struct task *task)
// does not return?
_runtsk:
	scr	0(x1),070	// used?
	lca	4(x1),^task
	lca	^task,x5
	lca	@one,flags(x5)	// runnable
	lcr	eiadr,076
//	...
	b	eires

_endtsk:
	scr	0(x1),070
	scr	atr,054		// save ATR
	lca	^task,x5
	ba	atr,time(x5)
	lca	@zero,flags(x5)	// done
	// TODO: any other cleanup?
	lcr	0(x1),077

	.data
atr:	.word	0	// temp ATR storage
atrov:	.bin	0100000000#5	// 2^24
aar:	.word	0
bar:	.word	0
eisr:	.word	0

eiadr:	.word	ei
iiadr:	.word	ii

@zero:	.word	0
@one:	.word	1
@two:	.word	2
@four:	.word	4
@eight:	.word	8
@twlv:	.word	12
@sxtn:	.word	16

// C-linkage for "struct task *task"
_task:	.word	^task
^task:	.word	0
