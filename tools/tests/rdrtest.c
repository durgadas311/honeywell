#include <h200io.h>
#include <stdlib.h>
/* Program to test reading punchcards */

static char buf[81];

/* translation table for IBM026-H punch codes (FORTRAN) */
static char fortran[] = {
	"0123456789'=: =&+ABCDEFGHI;.)(]?"
	"-JKLMNOPQR#$*)\\-</STUVWXYZ@,(~[^"
};

int main(argc, argv)
int argc;
char **argv;
{
	int x;
	char *tr;
	tr = memalign(64, 64+1);
	strcpy(tr, fortran);
	copt(PC_HOL);
	if (sense(SW_2)) {
		copt(PC_SPC);
		print("SPC", PR_NL);
	}
	setpnc(buf + 80, SP_WM | SP_IM);
	for (;;) {
		x = cread(buf);
		if (x) break;
		if (sense(SW_3) && memcmp(buf, "1EOF ", 5) == 0) break;
		if (sense(SW_1)) {
			tran(buf, buf, tr, TR_6TO6);
		}
		lprint(buf, PR_NL);
	}
	print("EOF", PR_NL);
	return 0;
}
