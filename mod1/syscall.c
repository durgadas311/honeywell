#include "task.h"

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
	}
}
