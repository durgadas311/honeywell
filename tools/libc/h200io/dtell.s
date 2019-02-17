// Read the disk controller address register
//
// void dtell(struct dsk_adr *adr)
//
// struct dsk_adr {
//	char lun;
//	int cyl;
//	int trk;
//	int rec;
//	int sw;	- dtell only
//	int dl;	- dtell only
// };

	.globl	@zero

	.globl	_dtell
_dtell:	lca	4(x1),x5	// adr reg buf
	pcb	.,015,004,010	// wait ctrl idle?
	pdt	adr,015,044,004
	pcb	.,015,004,010	// wait ctrl idle
	exm	lun,0(x5),001
	lca	@zero,4(x5)
	ba	cyl,4(x5)
	lca	@zero,8(x5)
	ba	trk,8(x5)
	lca	@zero,12(x5)
	ba	rec,12(x5)
	lca	@zero,16(x5)
	ba	sw,16(x5)
	lca	@zero,20(x5)
	ba	dl,20(x5)
	lcr	0(x1),077

	.data
adr:
lun:	.bin	0#1
pck:	.bin	0#1
cyl:	.bin	0#2
trk:	.bin	0#2
rec:	.bin	0#2
sw:	.bin	0#2
dl:	.bin	0#2
	.bin	c:0#1
