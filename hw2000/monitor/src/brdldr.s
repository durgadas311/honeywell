// bootstrap loader for BRF data on punch cards
	.admode	3
	.globl	brdldr
	.text

// The communications area - always org 0100
// NOTE: this is .text, so labels assigned to left char
@comm:				// start of communications area
	.bin	0#1		// reserved
@rev:	.string	"   "		// REV of last unit loaded
@prg:	.string	"      "	// program name
@seg:	.string	"  "		// segment
	.bin	0#1		// reserved
@halt:	.string	"        "	// halt name (name+seg)
	.bin	0#1		// reserved
@fxst0:	b	enter		// fixed start 0 (general return)
@fxst1:	.bin	0#4		// fixed start 1 ()
@fxst2:	.bin	0#4		// fixed start 2 ()
@fxst3:	.bin	0#4		// fixed start 3 ()
@xtoc:	.bin	0#4		// exit to own-code
	.bin	0#1		// reserved
@relau:	.bin	0#3		// relocation augment
@relpos: .bin	1#1		// relative position
@semd:	.bin	020#1		// search mode
@stmd:	.bin	'N'#1		// start mode
	.bin	0#6		// reserved
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
@cbuf	=	01620		// card buffer
@user	=	01750		// user program space

brdldr:
	cam	040	// 3-char mode
	// fill is alread 0 from load
	lca	hptr,x3
	lca	hptre,x4
	b	cleer
	// setup WMs in card buffer
	sw	@cbuf+6		// allow BA on hdrlen
enter:
	lca	norm,@stmd	// default to 'N' start mode
	lca	one,@relpos
	lca	neg1,@semd
// setup/re-init communications area
	h	0,017002	// "halt 3" - ready to load
normal:
// load segment from cards
	sst	neg1,found,01	// reset found flag
	lca	zeroa,x5	// init dist ptr
nextc:	pdt	@cbuf,011,041	// load header data
	pcb	.,011,041,010
	pcb	done,011,041,041	// end of deck (error)
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
nextr:	ext	emsk,banr	// zero if not last
	a	banr,banr	// get zero-balance status
// done loading a card
	bct	nextc,060	// loop if zero-balance (077)
	// TODO: set 020 in @semd...
	bce	0(x5),@stmd,'N'
	bce	(@aret),@stmd,'R'
//	bce	(@spst),@stmd,'S'
done:	h	0(x5)	// error condition
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
term:	mcw	3(x6),x5	//set go adr
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
	c	@cbuf+17,@seg+1
	bct	3f,045		// return if !=
	c	@cbuf+15,@prg+5
	bct	3f,045		// return if !=

	// copy prog,segm,rev to communications area
	// these all terminate on B word mark!
2:	mcw	@cbuf+17,@seg+1	// segment
	mcw			// prog name
	mcw			// revision
	sst	eight,found,01	// start loading now... (anything with bit 01 clear)
3:
1::	b	0

rel:	bce	2b,@relpos,01	// stop when counter reaches 1
	bs	one,@relpos
	b	3b

nofo:	h	0,014011	// "halt 8", or possibly "halt 9"
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
emsk:	.bin	04#1	// banner mask for last record
norm:	.bin	'N'#1
zeroa:	.bin	0#3	// init for dist - must be 3 char
hptr:	.word	@cbuf
hptre:	.word	@cbuf+80
//
banr:	.bin	0#1	// banner char
slen:	.bin	0#1	// string len
rend:	.word	0	// record ptr
fill:	.byte	0	// clear fill char
found:	.bin	1#1
