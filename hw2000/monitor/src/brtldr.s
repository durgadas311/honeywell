// bootstrap loader for BRF data on mag tape
// This is the AAAMON Tape monitor
	.admode	3
	.globl	brtldr
	.text

// The communications area - always org 0100
// NOTE: this is .text, so labels assigned to left char
@comm:				// start of communications area
@calm:	.bin	0#1		// call method, 1=manual, 0=call card
@rev:	.string	"   "		// REV of last unit loaded
@prg:	.string	"      "	// program name
@seg:	.string	"  "		// segment
@drv:	.bin	0#1		// tape drive LUN
@halt:	.string	"        "	// halt name (name+seg)
@star:	.bin	0#1		// call card col 18
@fxst0:	.bin	0#4		// fixed start 0 (general return)
@fxst1:	.bin	0#4		// fixed start 1 ()
@fxst2:	.bin	0#4		// fixed start 2 ()
@fxst3:	.bin	0#4		// fixed start 3 ()
@xtoc:	.bin	0#4		// exit to own-code
@sdir:	.bin	022#1		// search direction, 22=fwd, 23=rev
@relau:	.bin	0#3		// relocation augment
@relpos: .bin	1#1		// relative position
@semd:	.bin	020#1		// search mode
@stmd:	.bin	'N'#1		// start mode
@vis:	.bin	0400000000000#6	// visibility mask
@spst:	.word	0		// special start location
@ocrt1:	.bin	0#4		// own-code return 1
@ocrt2:	.bin	0#4		// own-code return 2
@nret:				// return for normal call
	scr	@aret+^,077	// ... return to program, if desired
	b	normal		// ... resume monitor
@gret:	.word	enter		// general return address
@date:	.string	"     "		// current date, YYDDD
@trap:	.bin	0#1		// trapping mode
@aret:	.word	0		// alternate return address (read call card)
@ecd:	.string	"JJ0#"		// ECD field
@cona:	.byte	0		// console typewriter availability (!IM)
@comme	=	0234		// end (+1) of communications area
@user	=	02474		// user program space

brtldr:
	cam	040	// 3-char mode
	// @buf cleared by loader
	// set RM in buffer
	sw	@buf+250,@buf+6
	si	@buf+250
	// patch PDT/PCB instructions PCU from bootstrap
	sst	0065,p1+@+2,077
	sst	0073,p2+@+2,077
	sst	0101,p3+@+2,077	// this is the output device
	sst	0065,p4+@+2,077
	sst	0073,p5+@+2,077
	// TODO: clear index registers?
enter:
	lca	norm,@stmd	// default to 'N' start mode
	lca	one,@relpos
	lca	defs,@semd
	lca	fwd,@sdir
	h	0,017002	// "halt 3" - ready to load
normal:	// entry for automated loading
	// TODO: set search mode...? resets?
	bbe	manu,@calm,001
	// read  and process a call card
1:	pdt	@buf,011,041
	pcb	.,011,041,010
	pcb	nofo2,011,041,041
	bce	cc,@buf+17,'*'
	b	1b
cc:	// move data to comm area
	mcw	@buf+17,@star	// '*'
	mcw			// halt name
	mcw			// tape drive
	mcw			// segment
	mcw			// prog name
manu:	sst	@drv,p1+@+3,007
	sst	@drv,p2+@+3,007
	sst	@drv,p3+@+3,007
	sst	@drv,p4+@+3,007
	sst	@drv,p5+@+3,007
	// find and load segment from tape
	sst	one,nfnd,01	// reset found flag
	lca	zeroa,x5	// init dist ptr
nextc:
	b	read
nextl:
	lca	hptr,x6		// starting ptr
	mcw	0(x6),banr	// get banner char
	bce	nofo,banr,'1'	// assume '1' is "1EOF "...
	bbe	segm,banr,010	// is this a segment header card?
	// backspace done by segm routine, as needed
	bbe	nextc,nfnd,01	// keep searching until found
	// got segment header we want, start loading...
	// TODO: setup halt name flag.
	lca	hptre,rend	// compute end of rec
	// any more data from hdr before changing x6?
	ba	6(x6),x6	// ptr to prog data (req WM at @buf+6)
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

// segment header card, copy data to comm area if match.
// If no match and searching backward, position tape accordingly.
segm:	scr	1f,070		// set return address
	// TODO: check @sdir, need backup...
//	bce	2f,@semd,077	// simple case, any segment matches - not for BRT?
	bce	rel,@semd,001	// count down @relpos to 01
	// segment always checked (00,20,40,60)
	c	@buf+17,@seg+1
	bct	not1,045	// return if !=
	bcc	5f,@semd,001	// check prog (20,60)
3:	bcc	6f,@semd,002	// check vis (40,60)
	// found - load this program unit.
	// copy prog,segm,rev to communications area
	// these all terminate on B word mark!
2:	mcw	@buf+17,@seg+1	// segment
	mcw			// prog name
	mcw			// revision
	sst	zero,nfnd,01	// start loading now...
4:
1::	b	0		// return to main

// test program name
5:	c	@buf+15,@prg+5
	bct	not1,045	// return if !=
	b	3b
// test visibility mask
6:	ext	@vis+5,@buf+23	// extract visibility bits
	c	@buf+23,zvis	// check for 0 (fail)
	bct	not1,042	// if no bits left, no match
	b	2b

// TODO: check vis, too - but what does that mean if fail?
rel:	bce	2b,@relpos,01	// stop when counter reaches 1
	bs	one,@relpos
	// now get next segment header...
not1:	bbe	revers,@sdir,001
	b	4b		// goto return

// seeks previous segment header...
// this also works if current rec is non-header (041, 044)
revers:
	mcw	@buf+5,bsc
	c	zero2,bsc
	bct	nofo2,042
	b	bksp		// backspace tape 'bsc' records
	b	4b		// goto return

read:	scr	1f,070
p1:	pdt	@buf,011,040,060	// load header data
p2:	pcb	.,011,040,010
p3:	pcb	nofo2,011,040,060	// end of tape (error)
1::	b	0

// backspace tape 'bsc' records
bksp:	scr	1f,070
p4:	pdt	@buf,011,040,000
p5:	pcb	.,011,040,010
	bs	one,bsc
	c	zero2,bsc
	bct	p4,045
1::	b	0

// hit "1EOF " record (at least banner 01) in main loop
nofo:	// switch to backward search...
	bbe	nofo2,@sdir,001	// if already searching bkwd, done (fail)
	sst	one,@sdir,001	// set bkwd bit
	// backup 2 to locate a valid record...
	mcw	two,bsc
	b	bksp
	b	read	// EOT jumps to nofo2 (not possible here?)
	bbe	nextl,@buf,010	// if segment header, resume search
	mcw	@buf+5,bsc	// cannot be zero?
	b	bksp
	// now must be at segment header(?)
	b	nextc		// resume search
	// should never hit "1HDR " going backward (stops at segment seq=0)
	//
nofo2:	// manual intervention required...
	sst	zero,@sdir,001	// clear bkwd bit
	h	0,014011	// "halt 8", or possibly "halt 9"
	b	nextc		// Pressing RUN means "ready to load"

	.data
// constants
one:	.bin	1#1
two:	.bin	2#2
four:	.bin	4#1
eight:	.bin	8#1
norm:	.bin	'N'#1
fwd:	.bin	022#1
defs:	.bin	020#1
zero:	.bin	0#1
zero2:	.byte	0
zeroa:	.byte	0	// init for dist - must be 3 char
zvis:	.byte	0,0,0	// visibility zero when combined w/...zero

hptr:	.word	@buf
hptre:	.word	@buf+250
//
banr:	.bin	0#1	// banner char
slen:	.bin	0#1	// string len
rend:	.word	0	// record ptr
fill:	.byte	0	// clear fill char
nfnd:	.bin	1#1	// 0 if the seg-hdr found (if loading)
bsc:	.bin	0#2	// counter for backspacing tape

	.bss	// this will be cleared by loader
@buf:	.space	250
