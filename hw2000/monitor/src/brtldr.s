// bootstrap loader for BRF data on mag tape
// This is the AAAMON Tape monitor
	.admode	3

// The communications area
@comm	=	0100		// start of communications area
@calm	=	0100		// call method, 1=manual, 0=call card
@rev	=	0101		// REV of last unit loaded
@prg	=	0104		// program name
@seg	=	0112		// segment
@drv	=	0114		// tape drive LUN
@halt	=	0115		// halt name (name+seg)
@star	=	0125		// call card col 18
@fxst0	=	0126		// fixed start 0 (general return)
@fxst1	=	0132		// fixed start 1 ()
@fxst2	=	0136		// fixed start 2 ()
@fxst3	=	0142		// fixed start 3 ()
@xtoc	=	0146		// exit to own-code
@sdir	=	0152		// search direction, 22=fwd, 23=rev
@relau	=	0153		// relocation augment
@relpos	=	0156		// relative position
@semd	=	0157		// search mode
@stmd	=	0160		// start mode
@vis	=	0161		// visibility mask
@spst	=	0167		// special start location
@ocrt1	=	0172		// own-code return 1
@ocrt2	=	0176		// own-code return 2
@nret	=	0202		// return for normal call
@gret	=	0213		// general return address
@date	=	0216		// current date, YYDDD
@trap	=	0223		// trapping mode
@aret	=	0224		// alternate return address (read call card)
@ecd	=	0227		// ECD field
@cona	=	0233		// console typewriter availability (!IM)
@comme	=	0234		// end (+1) of communications area
@buf	=	02100		// buffer (tape and card)
@user	=	02474		// user program space

// by the time we reach here, we are
// well above index register storage.
	.globl	brtldr
	.text
brtldr:
	cam	040	// 3-char mode
	// fill is alread 0 from load
	lca	hptr,x3
	lca	hptre,x4
	b	cleer
	lca	cptr,x3
	lca	cptre,x4
	b	cleer
	// set RM in buffer
	sw	@buf+250
	si	@buf+250
	// patch PDT/PCB instructions PCU from bootstrap
	sst	0065,p1+@+2,077
	sst	0073,p2+@+2,077
	sst	0101,p3+@+2,077	// this is the output device
	sst	0065,p4+@+2,077
	sst	0073,p5+@+2,077
	// TODO: clear index registers?
	// setup WMs in comm area
	sw	@rev,@prg
	sw	@seg,@gret
	sw	@drv,@halt
	bs	@drv	// initial tape drive LUN = 0
	// constants in comm area
	lca	eptr,@gret+^	// (sets WM)
	lca	nret		// BAR already set (sets WM)
	lca			// one more instruction (sets WM)
	sw	@fxst0,@fxst1	// setup fixed start 0 (general return)
	mcw	eptr
	mcw	branch
enter:
	lca	norm,@stmd	// default to 'N' start mode
	lca	one,@relpos
	lca	neg1,@semd
	lca	fwd,@sdir
	// setup/re-init communications area
	h	0,017002	// "halt 3" - ready to load
normal:
	// TODO: set search mode...? resets?
	bbe	manu,@calm,001
	// process a call card
1:	pdt	@buf,011,041
	pcb	.,011,041,010
	pcb	nofo2,011,041,041
	bce	cc,@buf+17,054	// TODO: needs copy to comm area
	b	1b
cc:	// move data to comm area (must have WMs)
	mcw	@buf+17,@star	// '*' and halt name
	mcw			// tape drive
	mcw			// segment
	mcw			// prog name
manu:	sst	@drv,p1+@+3,007
	sst	@drv,p2+@+3,007
	sst	@drv,p3+@+3,007
	sst	@drv,p4+@+3,007
	sst	@drv,p5+@+3,007
	// find and load segment from tape
	sst	neg1,nfnd,01	// reset found flag
	lca	zeroa,x5	// init dist ptr
nextc:
p1:	pdt	@buf,011,040,060	// load header data
p2:	pcb	.,011,040,010
p3:	pcb	nofo2,011,040,060	// end of tape (error)
// load program data from header
// works for card and tape records?
	lca	hptr,x6		// starting ptr
	mcw	0(x6),banr	// get banner char
	bce	nofo,banr,'1'	// assume '1' is "1EOF "...
	bbe	segm,banr,010	// is this a segment header card?
	// TODO: needs backspace handling
	bbe	nextc,nfnd,01	// keep searching until found
	lca	hptre,rend	// compute end of rec
	// any more data from hdr before changing x6?
	ba	6(x6),x6	// ptr to prog data
// parse next brt command
next:	bcc	ctlc,0(x6),03	// check for 11xxxx
	// 4 lsb bits have string len
	sst	0(x6),slen,017	// len of string
// must not set punc until after move (destroys punc)
	lca	x6,x4
	ba	slen,x4
	ba	one,x4		// points one past last
	sw	0(x4)		// mark end of move
	exm	1(x6),0(x5),037	// incl one extra+wm
	cw	0(x4)		// clear end of move
	bcc	stw1,0(x6),01	// needs wm
	bcc	sti1,0(x6),02	// needs im
move:	ba	slen,x5
	cw	0(x5)		// cleanup stray wm
	ba	slen,x6
nexte:	ba	one,x6
nextf:	c	x6,rend
	bct	next,044		// cont if x6 < rend
nextr:	// done loading a record
	bbe	ldone,banr,004	// last record of segment - should not see this
	b	nextc
	// TODO: set 020 in @semd...
	// TODO: check halt name...
ldone:
	lca	fwd,@sdir	// reset @sdir to fwd
	bce	0(x5),@stmd,'N'
	bce	(@aret),@stmd,'R'
//	bce	(@spst),@stmd,'S'
	h	0(x5)	// error condition
//
// exit is via ctlc on case of 61 char
// next rec is via ctlc on case of 77 char
//
stw1:	sw	0(x5)
	b	move
//
sti1:	si	0(x5)
	b	move
//
ctlc:	bce	ldst,0(x6),060	// set dist ptr
	bce	term,0(x6),061	// term load
	bce	cler,0(x6),062	// clear area
	bce	setw,0(x6),063	// set word mark
	bce	seti,0(x6),064	// set item mark
// we should only get 77 if banner is 50,41
	bce	nextr,0(x6),077	// end of record
//	error... invalid control
	h	.
//
ldst:	mcw	3(x6),x5
	ba	four,x6
	b	nextf
//
cler:	mcw	3(x6),x3
	mcw	6(x6),x4
	exm	7(x6),fill,07	// clears punc
	ba	eight,x6
	b	cleer
	b	nextf
//
setw:	sw	-1(x5)
	b	nexte
//
seti:	si	-1(x5)
	b	nexte
//
// todo: can load resume? is this guaranteed last rec?
term:	mcw	3(x6),x5	// set go adr
	b	ldone		// todo: also indicate goto
//
// clear/fill memory - must clear punc too
// caller sets x3, x4, fill (w/punc)
cleer:	scr	1f,070		// set return address
2:	exm	fill,0(x3),07
	c	x3,x4
	ba	one,x3
	bct	2b,044		// cont if x3 .lt. x4
1::	b	0

// segment header card, copy data to comm area
segm:	scr	1f,070		// set return address
	// TODO: check @sdir, need backup...
	bce	2f,@semd,077
	bce	rel,@semd,001
//	bcc	xxx,@semd,001	// 060 or 020 search
//				// else 040/000 search
	c	@buf+17,@seg+1
	bct	3f,045		// return if !=
	c	@buf+15,@prg+5
	bct	3f,045		// return if !=

	// found - load this program unit.
	// copy prog,segm,rev to communications area
	// these all terminate on B word mark!
2:	mcw	@buf+17,@seg+1	// segment
	mcw			// prog name
	mcw			// revision
	sst	zeroa,nfnd,01	// start loading now...
4:
1::	b	0

3:	bbe	revers,@sdir,001
	b	4b
	
// seeks previous segment header...
// this also works if current rec is non-header (041, 044)
revers:
	mcw	@buf+5,bsc
	c	zeroa,bsc
	bct	nofo2,042	// TODO: proper behavior - set fwd dir...
p4:	pdt	@buf,011,040,000
p5:	pcb	.,011,040,010
	bs	one,bsc
	c	zeroa,bsc
	bct	p4,045
	b	4b

rel:	bce	2b,@relpos,01	// stop when counter reaches 1
	bs	one,@relpos
	b	3b

nofo:	// TODO: switch to backward search...
	//	bbe	nofo2,@sdir,001
	//	sst	neg1,@sdir,001	// set bwd bit
	//	Need to backup to previous segment header... how?
	//	can backup 2 and read end-seg (non-hdr) record,
	//	then use seq to backup to seg hdr. But, it might be
	//	a seg-hdr, so need to check that.
	// but don't know where that is...
	// could read last record, us it's seq to find it's segment...
	// messy...
	// should never hit "1HDR " going backward (stops at segment seq=0)
	//
nofo2:	// manual intervention required...
	h	0,014011	// "halt 8", or possibly "halt 9"
	b	nextc		// Pressing RUN means "ready to load"

// template code for RETURN FOR NORMAL CALL
	scr	@aret+^,077	// return to program, if desired
branch:	// a B instruction opcode - any one will do
nret::	b	normal

	.data
// constants
neg1:	.bin	077#1
one:	.bin	1#1
four:	.bin	4#1
eight:	.bin	8#1
norm:	.bin	'N'#1
fwd:	.bin	022#1
zeroa:	.bin	0#3	// init for dist - must be 3 char

hptr:	.word	@buf
hptre:	.word	@buf+250
cptr:	.word	@comm
cptre:	.word	@comme-1
eptr:	.word	enter
//
banr:	.bin	0#1	// banner char
slen:	.bin	0#1	// string len
rend:	.word	0	// record ptr
fill:	.byte	0	// clear fill char
nfnd:	.bin	1#1	// 0 if the seg-hdr found (if loading)
bsc:	.bin	0#2	// counter for backspacing tape
