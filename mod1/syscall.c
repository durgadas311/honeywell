#include "task.h"
#include <h200io.h>

extern void endtsk();

void syscal(sc)
int sc;
{
	switch (sc) {
	case 0:	/* program exit */
		endtsk();
		break;
	case 1:	/* segment load */
		/* load(...) */
		break;
	default:
		print("BAD SYSCALL", PR_NL);
		endtsk();
		break;
	}
}
