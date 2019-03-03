// void *tlu(void *tab, char *src, int op)
// void *tlu(void *tab, int src, int op)
// void *tadds(void *tab, char *fld, int val)
// void *taddi(void *tab, int fld, int val)

	.globl	@zero,@two,@eight
	.globl	_tlus,_tadds
	.globl	_tlui,_taddi
	.text
_tlus:
	scr	0(x1),070
	mcw	4(x1),1f+8	// table
	lca	8(x1),x3	// search arg (char *)
	exm	12(x1),1f+9,001	// comparison op
	sst	12(x1),2f+5,007	//
	mos	0(x3),0(x3),010	// find end of str
	scr	x5,067
	sw	0(x3)	// what if empty string?
	bs	@two,x5
	mcw	x5,1f+4
1:	tlu	0,0,0
	scr	x5,070
	cw	0(x3)	// what if empty string?
2:	bct	3f,040	// if found
	lca	@zero,x5
3:	lcr	0(x1),077

_tlui:
	scr	0(x1),070
	lca	4(x1),1f+8	// table
	lca	x1,x3
	ba	@eight,x3	// 8(x1) = &search arg
	mcw	x3,1f+4		// search arg
	exm	12(x1),1f+9,001	// comparison op
	sst	12(x1),2f+5,007	//
	lca	@zero,x5	// set WM
1:	tlu	0,0,0
	scr	x5,070
2:	bct	3f,040	// if found
	lca	@zero,x5
3:	lcr	0(x1),077

// Caller must advance return addr +1 before calling again
_tadds:
	scr	0(x1),070
	lca	4(x1),x5	// table
	lca	12(x1),3(x5)	// value - address/int - sets WM
	lca	8(x1),x3	// field - string
	exm	0(x3),4(x5),031	// BAR = RM+1, no punc moved
	scr	x5,070		// RM+1, end of tbl is -2
	bs	@two,x5		// X5 has WM from prev LCA
	lcr	0(x1),077

// Caller must advance return addr +1 before calling again
_taddi:
	scr	0(x1),070
	lca	4(x1),x5	// table
	lca	12(x1),3(x5)	// value - address/int - sets WM
	exm	8(x1),7(x5),021	// field - address/int, no punc
	ba	sevn,x5
	lcr	0(x1),077

	.data
sevn:	.bin	07#1
