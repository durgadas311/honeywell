#include "task.h"

extern void *memtop;
extern void superv();

static struct task montsk;

static struct task tasks[4];

static void *monfnc = superv;

void start() {
	unsigned x;

	initsk(&montsk);
	montsk.eivar[3] = 0;
	montsk.sr = monfnc;
	montsk.id = '0';
	for (x = 0; x < 4; ++x) {
		initsk(&tasks[x]);
	}
	/* TODO: more initialization... */
	montsk.flags = 1;
	runtsk(&montsk);	/* should not return */

	/* must never return from here... */
	asm(" h .");
}
