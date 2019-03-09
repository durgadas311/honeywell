#include "task.h"

extern void sched();

/* supervisor - a.k.a. monitor */

void superv() {
	for (;;) {
		/* TODO: implement console interact */
		sched();
	}
}
