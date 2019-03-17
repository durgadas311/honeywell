// Physically seek to cylinder 0 (restore).
//
// void drest(int lun)
// returns -1 on error
//

	.globl	@P0
	.globl	_drest
_drest:
	exm	4(x1),1f+7,001	// lun
1:	pcb	.,015,044,030	// wait idle, restore
	// TODO: wait for completion?
	lca	@P0,x5
	lcr	0(x1),077
