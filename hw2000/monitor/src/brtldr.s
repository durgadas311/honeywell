// bootstrap loader for BRF data on mag tape
// This is the AAAMON Tape monitor
	.admode	3

// The communications area
@comm	=	0100		// start of communications area
@calm	=	0100		// call method, 0=manual, 1=call card
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
	sst	0101,p3+@+2,077
	// TODO: clear index registers?
	// setup WMs in comm area
	sw	@rev,@prg
	sw	@seg,@gret
	sw	@drv
	// setup WMs in card buffer
	sw	@buf+6		// hdrlen
	// constants in comm area
	lca	eptr,@gret+^	// (sets WM)
	lca	nret		// BAR already set (sets WM)
	lca			// one more instruction (sets WM)
	sw	@fxst0,@fxst1	// setup fixed start 0 (general return)
	mcw	eptr
	mcw	branch
	bs	@drv	// initial tape drive LUN = 0
enter:
	lca	norm,@stmd	// default to 'N' start mode
	lca	one,@relpos
	lca	neg1,@semd
	// setup/re-init communications area
	h	0,017002	// "halt 3" - ready to load
normal:
	sst	@drv,p1+@+3,007
	sst	@drv,p2+@+3,007
	sst	@drv,p3+@+3,007
	// load segment from tape
	sst	neg1,found,01	// reset found flag
	lca	zeroa,x5	// init dist ptr
nextc:
p1:	pdt	@buf,011,040,060	// load header data
p2:	pcb	.,011,040,010
p3:	pcb	nofo2,011,040,060	// end of tape (error)
// load program data from header
// returns zero-balance if not last record
// works for card and tape records?
	lca	hptr,x6		// starting ptr
	mcw	0(x6),banr	// get banner char
	bce	nofo,banr,'1'	// assume '1' is "1EOF "...
	bbe	segm,banr,010	// is this a segment header card?
	bbe	nextc,found,01	// keep searching until found
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
	bbe	done,banr,004	// last record of segment?
	b	nextc
	// TODO: set 020 in @semd...
done:	bce	0(x5),@stmd,'N'
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
	sst	neg1,banr,04	// force end-of-load cond
	b	nextr		// todo: also indicate goto
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
	// TODO: check search mode - compare prog/segm...
	bce	2f,@semd,077
	bce	rel,@semd,001
//	bcc	xxx,@semd,001	// 060 or 020 search
//				// else 040/000 search
	c	@buf+17,@seg+1
	bct	3f,045		// return if !=
	c	@buf+15,@prg+5
	bct	3f,045		// return if !=

	// copy prog,segm,rev to communications area
	// these all terminate on B word mark!
2:	mcw	@buf+17,@seg+1	// segment
	mcw			// prog name
	mcw			// revision
	sst	eight,found,01	// start loading now... (anything with bit 01 clear)
3:
1::	b	0

rel:	bce	2b,@relpos,01	// stop when counter reaches 1
	bs	one,@relpos
	b	3b

nofo:	// TODO: switch to backward search...
	// should never hit "1HDR " going backward (stops at segment seq=0)
	//
nofo2:	// manual intervention required...
	h	0,014011	// "halt 8", or possibly "halt 9"
	b	nextc		// Pressing RUN means "more cards added"

// template code for RETURN FOR NORMAL CALL
	scr	@aret+^,077	// return to program, if desired
branch:	// a B instruction opcode - any one will do
nret::	b	normal

	.data
neg1:	.bin	077#1
one:	.bin	1#1
four:	.bin	4#1
eight:	.bin	8#1
hptr:	.word	@buf
hptre:	.word	@buf+250
cptr:	.word	@comm
cptre:	.word	@comme-1
eptr:	.word	enter
//
banr:	.bin	0#1	// banner char
slen:	.bin	0#1	// string len
rend:	.word	0	// record ptr
zeroa:	.bin	0#3	// init for dist - must be 3 char
fill:	.byte	0	// clear fill char
norm:	.bin	'N'#1
found:	.bin	1#1
