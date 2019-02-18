#include <h200io.h>
/* Program to test reading punchcards */

static char buf[81];

int main(argc, argv)
int argc;
char **argv;
{
	int x;
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
		lprint(buf, PR_NL);
	}
	print("EOF", PR_NL);
	return 0;
}
