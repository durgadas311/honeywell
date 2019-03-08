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

void sched() {
	struct task *t, *t1, *tn;
	if (task == &montsk) {
		t = &tasks[0];
	} else {
		t = task;
	}
	tn = 0;
	for (t1 = t + 1; t1 != t; ++t1) {
		if ((t1->flags & 2) != 0) {
			tn = t1;
			/* t1->flags = t1->flags & ~2; */
			t1->flags &= ~2;
			break;
		}
		if (tn == 0 && t1->flags & 1) {
			tn = t1;
		}
	}
	if (tn == 0) {
		return;
	}
	/* TODO: any shutdown required? */
	task = tn;
}
