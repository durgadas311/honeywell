// Read a record (sector) from disk. Uses I/O channel 15
//
// void dset(struct dsk_adr *adr)
// int dread(void *buf)
// int dwrite(void *buf)
// returns num chars on success, -1 on error
//
// struct dsk_adr {
//	char lun;
//	int cyl;
//	int trk;
//	int rec;
//	int sw;	- dtell only
//	int dl;	- dtell only
// };

	.globl	@zero,@none,@one

	.globl	_dset
_dset:	lca	4(x1),x5	// adr reg buf
	exm	0(x5),lun,001
	bs	cyl
	ba	4(x5),cyl
	bs	trk
	ba	8(x5),trk
	bs	rec
	ba	12(x5),rec
	// TODO: more?
	pcb	.,015,004,010	// wait ctrl idle?
	pdt	adr,015,004,004
	// no need to wait?
	pcb	.,015,004,010	// wait ctrl idle
	exm	@zero,nxt,001
	lcr	0(x1),077

	.globl	_dread
_dread:
	scr	0(x1),070
	bbe	4f,nxt,040	// no dset?
	lca	4(x1),x5	// buf
	sst	lun,1f+7,007	// set unit number...
	sst	nxt,2f+7,001	// search first/next bit
	sst	lun,3f+7,007	// set unit number...

	pcb	.,015,004,010	// wait ctrl idle
1:	pcb	.,015,044,000,0	// wait disk idle
2:	pdt	0(x5),015,044,002,0
3:	pcb	.,015,044,000,0	// wait disk idle
	pcb	4f,015,004,050	// general exception
	scr	x5,005	// we know x5 has WM
	scr	x6,015
	bs	x6,x5	// num chars transferred
	exm	@one,nxt,001
	lcr	0(x1),077

4:	lca	@none,x5
	lcr	0(x1),077

	.globl	_dwrite
_dwrite:
	scr	0(x1),070
	bbe	4b,nxt,040	// no dset?
	lca	4(x1),x5	// buf
	sst	lun,1f+7,007	// set unit number...
	sst	nxt,2f+7,001	// search first/next bit
	sst	lun,3b+7,007	// set unit number...

	pcb	.,015,004,010	// wait ctrl idle
1:	pcb	.,015,044,000,0	// wait disk idle
2:	pdt	0(x5),015,004,002,0
	b	3b	// rest is same as read

	.data
nxt:	.bin	040#1

adr:
lun:	.bin	0#1
pck:	.bin	0#1
cyl:	.bin	0#2
trk:	.bin	0#2
rec:	.bin	0#2
	.bin	c:0#1
