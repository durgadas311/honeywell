// bootstrap loader for BRF data on mag tape
// ORG at 1340 (01750 octal)
@vis	=	0124	// doc says 0124-0129 (invalid octal)
	.admode	3

	.globl	mtibldr
	.text
mtibldr:
	cam	040	// 3-char mode
	sst	0065,1f+@+2,077	// copy PCU from boot PDT
	sst	0073,2f+@+2,077	// copy PCU from boot PCB #1
	sst	0101,3f+@+2,077	// copy PCU from boot PCB #2
	// fill is alread 0 from load
	lca	hptr,x3
	lca	hptre,x4
	b	cleer	// clear tape buffer
	sw	0(x4)	//
	si	0(x4)	// RM at end of buffer
	lca	monvi,@vis+5	// default visibility
enter:
	h	@vis,017001	// "halt 1" - ready to load
	sst	one,found,01	// reset found flag
	lca	zeroa,x5	// init dist ptr
nextt:
1:	pdt	tbuf,011,040,060	// load header data
2:	pcb	.,011,040,000
3:	pcb	nofo,011,040,060	// EOT
// load program data from header
// returns zero-balance if not last record
// works for card and tape records?
	lca	hptr,x6		// starting ptr
	mcw	0(x6),banr	// get banner char
	bce	nofo,banr,'1'	// assume '1' is "1EOF "...
	bbe	segm,banr,010	// is this a segment header card?
	bbe	nextt,found,01	// keep searching until found
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
//
	bcc	stw1,0(x6),01	// needs wm
	bcc	sti1,0(x6),02	// needs im
move:	ba	slen,x5
	cw	0(x5)		// cleanup stray wm
	ba	slen,x6
nexte:	ba	one,x6
nextf:	c	x6,rend
	bct	next,044		// cont if x6 < rend
	// done with record
nextr:	bbe	0(x5),banr,004	// last record of segment, start code
	b	nextt

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
	bce	term,0(x6),061	// term load (only banner 54,44)
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
	b	0(x5)		// start monitor
//
// clear/fill memory - must clear punc too
// caller sets x3, x4, fill (w/punc)
cleer:	scr	1f,070		// set return address
2:	exm	fill,0(x3),07
	c	x3,x4
	ba	one,x3
	bct	2b,044		// cont if x3 .lt. x4
1::	b	0

// segment header card, check for match
// x6 = record buffer
segm:	scr	1f,070		// set return address
	c	17(x6),monsg
	bct	3f,045		// if !=
	c	15(x6),monnm
	bct	3f,045		// if !=
	ext	@vis+5,23(x6)
	c	23(x6),zervi
	bct	3f,042		// if 0, no match

	sst	eight,found,01	// start loading now... (anything with bit 01 clear)
3:
1::	b	0

nofo:	h	0,014011	// "halt 8", or possibly "halt 9"
	b	nextt		// Pressing RUN means "(new) tape rewound"

	.data
one:	.bin	1#1
four:	.bin	4#1
eight:	.bin	8#1
emsk:	.bin	04#1	// banner mask for last record
hptr:	.word	tbuf
hptre:	.word	tbuf+250
//
monnm:	.string	"AAAMON"
monsg:	.string	"S1"
monvi:	.bin	0400000000000#6
zervi:	.bin	0#6
zeroa:	.bin	0#3	// init for dist - must be 3 char
//
banr:	.bin	0#1	// banner char
slen:	.bin	0#1	// string len
rend:	.word	0	// record ptr
fill:	.byte	0	// clear fill char
found:	.bin	1#1	// 0 = found

tbuf	=	.
