#include <h200io.h>
#include <stdlib.h>

static int ary[] = {
	100, 5,
	100, 3,
	250, 4
};

static int dv[2];

int main(argc, argv)
int argc;
char **argv;
{
	int x;
	for (x = 0; x < 6; x += 2) {
		puto(ary[x]);
		putc('/');
		puto(ary[x+1]);
		putc('=');
		div(ary[x], ary[x+1], dv);
		puto(dv[0]);
		putc('r');
		puto(dv[1]);
		putnl();
	}
	return 0;
}
