/*
 * This runs in user mode, cannot call kernel directly.
 * however, we can perform I/O and other priviledged ops.
 */

#include "task.h"
#include "syscall.h"
#include <h200io.h>

extern int sgets();	/* (char *buf) */

/* supervisor - a.k.a. monitor */

static char buf[65];

void superv() {
	unsigned n;
	setpnc(buf + 64, SP_WM | SP_IM);
	for (;;) {
		n = sgets(buf);
		/* TODO: implement something useful */
		print(buf, PR_NL);
	}
}
