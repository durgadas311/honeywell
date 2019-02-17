// Physically seek to cylinder.
//
// void dseek(int lun, int cyl)
// returns -1 on error
//

	.globl	@zero,@none

	.globl	_dseek
_dseek:
	exm	4(x1),1f+7,001	// lun
	exm	7(x1),1f+9,001	// cyl
	exm	8(x1),1f+10,001	// cyl
1:	pcb	.,015,004,020,000,000,000	// wait idle, seek cyl
	// TODO: wait for completion?
	lca	@zero,x5
	lcr	0(x1),077
