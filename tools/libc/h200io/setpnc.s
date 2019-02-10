// setpnc - set punctiation of a specific character

// args: ptr, punc
SP_WM=001
SP_IM=002
SP_CLR=004
// SP_RM=(SP_WM|SP_IM)

	.globl	_setpnc
	.text
_setpnc:
	scr	0(x1),070
	lca	4(x1),x5
	bbe	4f,8(x1),SP_CLR
1:	bbe	5f,8(x1),SP_WM
2:	bbe	6f,8(x1),SP_IM
3:	lcr	0(x1),077

4:	cw	0(x5)
	ci	0(x5)
	b	1b
5:	sw	0(x5)
	b	2b
6:	si	0(x5)
	b	3b
