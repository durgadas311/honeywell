#include "task.h"

#define NTASK	4

extern void *memtop;
extern void superv();

static struct task montsk;

static struct task tasks[NTASK];

void start() {
	unsigned x;

	initsk(&montsk);
	inimon(&montsk);
	for (x = 0; x < NTASK; ++x) {
		initsk(&tasks[x]);
	}
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
	t1 = t;
	do {
		++t1;
		if (t1 == &tasks[NTASK]) {
			t1 = &tasks[0];
		}
		if ((t1->flags & 2) != 0) {
			tn = t1;
			t1->flags &= ~2;
			break;
		}
		if (tn == 0 && t1->flags & 1) {
			tn = t1;
		}
	} while (t1 != t);
	if (tn == 0) {
		task = &montsk;
		return;
	}
	/* TODO: any shutdown required? */
	task = tn;
}
