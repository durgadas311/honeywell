// Interrupt (trap) entry into OS.
// includes MC (syscall).
// Also includes "cold boot".

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

	.globl	_task,^task,_memtop,^memtop
	.globl	_endtsk,_runtsk
	.globl	@zero,@one,@two,@four,@eight,@twlv,@sxtn
	.globl	_tick,_syscal,_start
	.text

?start:	// also top of memory, etc.
	cam	060
	lca	@start,x1	// set stack pointer
	bs	@four,x1	// wiggle room
	lca	@start,_memtop
	bs	@4k,_memtop	// space for stack
	sst	@zero,_memtop,077	// force
	sst	@zero,_memtop-1,077	// 4k boundary
	lcr	eiadr,066
	lcr	iiadr,076
	// TODO: safety net for CSM?
	b	_start

// EI handler, incl. MC (syscall)
// Note thatan ATR overflow could happen
// while entering here for other reasons,
// so we need to check other bits after
// servicing an ATR overflow, and check
// ATR first.
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
	bcc	tick,eiind+4,020
	// must service all (other) sources before
	// syscall, as syscall might cause dispatch.
//	...
	bbe	sc,eiind+4,020
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
// ^task is already setup in x5...
sc:
	scr	eisr,076	// user's SR
	lca	eisr,sr(x5)
	ba	brr(x5),eisr-2	// relocate SR
	lca	@zero,0(x1)
	exm	(eisr-3),0(x1),001
	ba	@one,sr(x5)
	lcr	sr(x5),076	// point past func code
	bs	@four,x1
	b	_syscal
	ba	@four,x1
	// might be on new task now... X5 must get set
	lca	^task,x5
	b	eires	// TODO: return rather than branch?

tick:
	scr	0(x1),070
	scr	atr,054		// save ATR
	ba	atrov,time(x5)	// count overflow
	ba	atr,time(x5)	// count residual
	// can't callout to _tick here, might be
	// a syscall so can't dispatch(?)
	lcr	@zero,054	// zero ATR
	lcr	0(x1),077

// runtsk(struct task *task)
// does not return? only if error...
// program may re-enter monitor/supervisor
// through standard points.
_runtsk:
	scr	0(x1),070
	lca	4(x1),x5
	c	@zero,flags(x5)
	bct	1f,042	// task has exited?
	lca	x5,^task
	lca	@one,flags(x5)	// runnable
	// TODO: must enter interrupt mode?
	lib	ibr+1(x5),brr+1(x5),006
	lcr	sr(x5),066
//	...
	b	eires
1:	lcr	0(x1),077

_endtsk:
	scr	0(x1),070
	// ATR should be stopped, but
	// overflowed should have been handled already?
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
@4k:	.word	4096

@start:	.word	?start


// C-linkage for "struct task *task"
_task:	.word	^task
^task:	.word	0
// ditto for "void *memtop"
_memtop: .word	^memtop
^memtop: .word	0
