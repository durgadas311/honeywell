// Interrupt (trap) entry into OS.
// includes MC (syscall).
// Also includes "cold boot".

// ATR handling dilemma:
// Do we count time spent in the monitor? Syscalls make some
// sense, but interrupt less because they may be for other task's.
// We also have a race condition, where we take an EI for
// (e.g.) a syscall but then ATR overflows after the SVI.
// We must turn off clock (LIB), then SVI again, then check
// overflow. Additionally, SVI must be called while still in
// EI mode in order to return valid status, so we can't
// resume "normal" mode to perform the syscall. Although,
// if we perform the syscall in normal mode, then ATR overflow
// should get handled.

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
time	=	45	// "char[8]" (10-chr int) (r)
id	=	46	// char
ret	=	47	// char

	// public/exports
	.globl	_memtop,^memtop
	.globl	_endtsk,_runtsk,_initsk,_inimon
	.globl	_scret,_scarg,_scptr
	.globl	@zero,@one,@two,@four,@eight,@twlv,@sxtn
	// externals
	.globl	_tick,_syscal,_start,_panic
	.globl	_montsk,^task
	.text

?start:	// also top of memory, etc.
	cam	060
	lca	@start,x1	// set stack pointer
	bs	@four,x1	// wiggle room
	lca	@start,_memtop
	bs	@4k,_memtop	// space for stack
	sst	@zero,_memtop,077	// force
	sst	@zero,_memtop-1,077	// 4k boundary
	lcr	eiadr,066	// setup EIR
	lcr	iiadr,076	// setup IIR
	// TODO: safety net for CSM?
	b	_start

// EI handler, incl. MC (syscall).
// Note thatan ATR overflow could happen
// while entering here for other reasons,
// so we need to check other bits after
// servicing an ATR overflow, and check
// ATR first.
// EI in II cannot incl. syscall? and so
// cannot change tasks? Must return to II
// in that case!
eires:
	// might be on new task now... X5 must get set
	lca	^task,x5
	lcr	sr(x5),066	// set EIR for RNM
	lcr	eibar(x5),070	// restore BAR
	lcr	eiaar(x5),067	// restore AAR
	rvi	eivar(x5),035	// addr mode changes
	rnm
ei:	svi	075
eiind:	.byte	0,0,0,0,0	// no WM allowed!
	cam	060
	scr	aar,067
	scr	bar,070
	// TODO:
	// ATR is still counting! and might overflow!
	// do we turn off here (LIB), and not account
	// for "system time"? Or run up until dispatch
	// ?
	//	lib	@zero,@zero,0
	// TODO: how long/when do we stay in intr mode?
	lca	^task,x5
	lca	aar,eiaar(x5)
	lca	bar,eibar(x5)
	exm	eiind,eivar(x5),031
	scr	eisr,066	// user's SR from EIR
	lca	eisr,sr(x5)
	bcc	tick,eiind+4,020
	// must service all (other) sources before
	// syscall, as syscall might cause dispatch.
	bbe	eivio,eiind+4,040	// adr vio in II
	// bbe	eicp,eiind+4,010	// cons/panel intr
	// bbe	eipi,eiind+4,004	// periph intr
	bbe	sc,eiind+4,020
	b	eires	// resume program

// II - FPE, adr/opcode violation, instr timeout.
// These are all fatal to the task.
iires:	lca	^task,x5
	lcr	sr(x5),076	// set IIR for RNM
	lcr	iibar(x5),070
	lcr	iiaar(x5),067
	rvi	iivar(x5),033
	rnm
ii:	svi	073
iiind:	.byte	0,0,0,0,0	// no WM allowed!
	cam	060
	scr	aar,067
	scr	bar,070
	lca	^task,x5
	lca	aar,iiaar(x5)
	lca	bar,iibar(x5)
	exm	iiind,iivar(x5),031
	bcc	1f,iiind+4,020
2:	exm	iivar+4(x5),ret(x5),001	// result/exit code
	b	_endtsk
	b	_sched	// find something new to run
	b	iires	// resume something else
1:	// FPE, odd case (item mark)
	sst	@none,iivar+4(x5),004 // move IM bit to data
	b	2b

// Nothing to do but panic here...
eivio:	lca	eiava,0(x1)
	bs	@four,x1
	b	_panic	// does not return,
	h	.	// but just in case...

// TODO: context switches require more...
// ^task is already setup in x5...
sc:
	ba	brr(x5),eisr-2	// relocate SR
	lca	@zero,0(x1)
	exm	(eisr-3),0(x1),001 // arg is sc num
	ba	@one,sr(x5)	// point past func code
	bs	@four,x1
	b	_syscal
	ba	@four,x1
	b	eires

// Utility routines for syscall implementations

// void scret(int val) - set a return value for a syscall
_scret:	scr	0(x1),070
	lca	^task,x5
	lca	@zero,x3
	ba	brr(x5),x3-2	// task base adr
	lca	4(x1),x5(x3)	// caller's X5 reg
	lcr	0(x1),077

// int scarg(int off) - get an arg for a syscall
// arg is not relocated (if it was a pointer).
_scarg:	scr	0(x1),070
	lca	^task,x5
	lca	@zero,x3
	ba	brr(x5),x3-2	// task base adr
	lca	x2(x3),x4	// caller's bp (X2)
	ba	4(x1),x4	// bp+off
	lca	0(x4),x5	// val into ret
	lcr	0(x1),077

// void *scptr(int off) - get a ptr arg for a syscall
// arg is assumed to be ptr and is relocated.
_scptr:	scr	0(x1),070
	lca	^task,x5
	lca	@zero,x3
	ba	brr(x5),x3-2	// task base adr
	lca	x2(x3),x4	// caller's bp (X2)
	ba	4(x1),x4	// bp+off
	lca	0(x4),x5	// val into ret
	ba	x3,x5		// relocate it
	lcr	0(x1),077

/////////////////////////////////////////////////////////

tick:	// X5 has task struct ptr
	scr	0(x1),070
	ba	atrov,time(x5)	// count overflow
	lcr	0(x1),077

// sustsk() - suspend current task.
// get ready to run a different task.
// must still be in EI mode!
_sustsk:
	scr	0(x1),070
	lib	@zero,@zero,0	// stop clock
	svi	040		// save only EI
eis:	.byte	0
	scr	atr,054		// save ATR
	lcr	@zero,054	// zero ATR
	lca	4(x1),x5
	bcc	tick,eis,020
	ba	atr,time(x5)	// count residual
	lcr	0(x1),077

// runtsk(struct task *task)
// does not return? only if error...
// program may re-enter monitor/supervisor
// through standard points.
// TODO: when is this used?
_runtsk:
	scr	0(x1),070
	lca	4(x1),x5
	bce	1f,flags(x5),000	// task has exited?
	lca	x5,^task
	sst	@one,flags(x5),077	// runnable - bit or char?
	// TODO: must enter interrupt mode?!
	lib	ibr+1(x5),brr+1(x5),006	// LCR and ATR on
	lcr	sr(x5),066	// EIR
//	...
	b	eires
1:	lcr	0(x1),077

// endtsk() - end current task
_endtsk:
	scr	0(x1),070
	// ATR should be stopped, but is it
	// already zeroed?
	scr	atr,054		// save ATR
	lca	^task,x5
	c	_montsk,x5
	bct	1f,042		// superv cannot exit
	ba	atr,time(x5)
	sst	@zero,flags(x5),077	// done
	// TODO: any other cleanup?
	lcr	0(x1),077
1:	lca	monxa,0(x1)
	bs	@four,x1
	b	_panic	// does not return,
	h	.	// but just in case...

// initsk(struct task *task) - init task struct
// Since initial run will only be via EI resume,
// no need to mess with II fields.
// Defaults to "user" task, caller must change
// if desired.
// Caller must setup IBR/BRR, id, as well as comm area.
// TODO: need to worry about latent punctuation?
_initsk:
	scr	0(x1),070
	lca	4(x1),x5	// task
	// TODO: memset? or CLEAR?
	lca	@zero,flags(x5)
	sst	@zero,id(x5),077
	sst	@zero,ret(x5),077
	lca	@zero,eiaar(x5)
	lca	@zero,eibar(x5)
	lca	@zero,time(x5)	// zero time...
	lca	@zero,time-3(x5) // ...
	lca	@zero,time-6(x5) // ... and set WM
	sst	@zero,ibr(x5),077 // clear ibr LO
	sst	// clear ibr HI
	sst	// clear brr LO
	sst	// clear brr HI	// end of @zero...
	exm	user,eivar(x5),031
	lcr	0(x1),077

// Special, addtn, setup for supervisor
	.globl	_superv
_inimon:
	scr	0(x1),070
	lca	4(x1),x5	// struct task *montsk
	exm	@zero,eivar+3(x5),001	// force sys mode
	bct	_superv,040	// branch never, set AAR
	scr	sr(x5),067	// run supervisor code entry
	exm	@zero,id(x5),001	// special task id
	lca	@one,flags(x5)	// always runnable
	lcr	0(x1),077

	.data
atr:	.word	0	// temp ATR storage
atrov:	.bin	02000000#5	// 2^19
aar:	.word	0
bar:	.word	0
eisr:	.word	0

eiadr:	.word	ei
iiadr:	.word	ii

// USER mode for LVI (RNM)
user:	.byte	000,060,000,062,0100	// 4-chr-adr, PROT + TIMEOUT + RELOC
//sys:	.byte	000,060,000,000,0100	// 4-chr-adr

// global constants
@zero:	.word	0
@one:	.word	1
@none:	.word	-1
@two:	.word	2
@four:	.word	4
@eight:	.word	8
@twlv:	.word	12
@sxtn:	.word	16
@4k:	.word	4096

@start:	.word	?start

eiava:	.word	eiav
eiav:	.string	f:"EI adr vio_"
monxa:	.word	monx
monx:	.string	f:"Monitor killed_"

// C-linkage for "void *memtop"
_memtop: .word	^memtop
^memtop: .word	0
