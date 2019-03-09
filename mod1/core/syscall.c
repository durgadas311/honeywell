#include "task.h"
#include <h200io.h>

extern void endtsk();
extern void sched();

void syscal(sc)
int sc;
{
	switch (sc) {
	case 0:	/* program exit */
		endtsk();
		break;
	case 1:	/* segment load */
		/* either an overlay or chained prog */
		/* load(...) */
		break;
	default:
		print("BAD SYSCALL", PR_NL);
		endtsk();
		break;
	}
	/* TODO: ever not sched()? */
	sched();
}
