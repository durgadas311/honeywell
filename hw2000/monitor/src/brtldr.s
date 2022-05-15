// bootstrap loader for BRF data on tape
	.admode	3
// by the time we reach here, we are
// well above index register storage.
	.globl	brtldr
	.text
brtldr:
	lca	hptr,strt
	lca	hptre,endc
	exm	fzero,fill,07	// clears punc
	b	cleer
//
// load segment from tape
// todo: allow selection of drive unit
	lca	zeroa,x5	// init dist ptr
nextr:	pdt	header,011,040,060	// load header data
	pcb	.,011,040,000
//	check errors? eot? eof? search?
//
// load program data from header
// returns zero-balance if not last record
// works for card and tape records?
	lca	hptr,x6		// starting ptr
	mcw	0(x6),banr	// get banner char
	lca	hptr,rend	// compute end of rec
	ba	3(x6),rend	// rec len
	mcw	6(x6),clen	// get rec ctl len
//	any more data from hdr?
	ba	clen,x6		// ptr to prog data
// parse next brt command
next:	bcc	ctlc,0(x6),03	// check for 11xxxx
//	4 lsb bits have string len
	sst	0(x6),slen,017	// len of string
// must not set punc until after move (destroys punc)
	lca	x6,endc
	ba	slen,endc
	ba	one,endc	// points one past last
	sw	(endc-^)	// mark end of move
	exm	1(x6),0(x5),037	// incl one extra+wm
	cw	(endc-^)	// clear end of move
	bcc	stw1,0(x6),01	// needs wm
	bcc	sti1,0(x6),02	// needs im
move:	ba	slen,x5
	cw	0(x5)		// cleanup stray wm
	ba	slen,x6
nexte:	ba	one,x6
nextf:	c	x6,rend
	bct	next,044		// cont if x6 < rend
endr:	ext	emsk,banr	// zero if not last
	a	banr,banr	// get zero-balance status
// done loading a record
	bct	nextr,060	// loop if zero-balance (077)
done:	h	(x5)		// or b (x5)?
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
	bce	endr,0(x6),077	// end of record
//	error...
	h	.
//
ldst:	mcw	3(x6),x5
	ba	four,x6
	b	nextf
//
cler:	mcw	3(x6),strt
	mcw	6(x6),endc
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
	b	endr		// todo: also indicate goto
//
// clear/fill memory - must clear punc too
// caller sets strt, endc, fill (w/punc)
cleer:	scr	1f,070		// set return address
2:	exm	fill,(strt-^),07
	c	strt,endc
	ba	one,strt
	bct	2b,044		// cont if strt .lt. endc
1::	b	0

	.data
neg1:	.bin	077#1
one:	.bin	1#1
four:	.bin	4#1
eight:	.bin	8#1
emsk:	.bin	04#1	// mask for last record
hptr:	.word	header
hptre:	.word	header+256
//
fzero:	.byte	0
banr:	.bin	0#1	// banner char
slen:	.bin	0#1	// string len
clen:	.bin	0#1	// rec ctl len (hdr)
rend:	.word	0	// record ptr
zeroa:	.bin	0#3	// init for dist - must be 3 char
strt:	.word	0	// clear start addr
endc:	.word	0	// clear end addr
fill:	.byte	0	// clear fill char
//
header	=	.
