#include <h200io.h>

static char buf[100];

int main(argc, argv)
int argc;
char **argv;
{
	int n;
	setpnc(buf + 40, SP_IM | SP_WM);
	print("what? ", 0);
	n = gets(buf);
	print("you said: ", 0);
	putd(n);
	putc(' ');
	print(buf, PR_NL);
	return 0;
}
