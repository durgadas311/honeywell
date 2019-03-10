#include "task.h"
#include <h200io.h>

extern void endtsk();
extern void sched();

extern void scret();	/* (int ret) */
extern int scarg();	/* (int off) */
extern void *scptr();	/* (int off) */

void syscal(sc)
int sc;
{
	switch (sc) {
	case 0:	/* normal program exit */
		/* task->ret should be 0 */
		endtsk();
		break;
	case 1:	/* segment load */
		/* either an overlay or chained prog */
		/* load(...) */
		break;
	case 2:	/* yield */
		/* just continue to sched() */
		scret(0);
		break;
	default:
		print("BAD SYSCALL", PR_NL);
		task->ret = TR_ENOSYS;
		endtsk();
		break;
	}
	/* TODO: ever not sched()? */
	sched();
}
