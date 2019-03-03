#include "task.h"

extern void *memtop;

void start() {
	/* TODO: initialization... */
	/* superv(); // supervisor loop */
	/* must never return from here... */
	asm(" h .");
}
