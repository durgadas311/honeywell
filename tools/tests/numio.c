#include <h200io.h>

int main(argc, argv)
int argc;
char **argv;
{
	int x;
	for (x = 0; x < 100; x += 10) {
		puto(x);
		putc(' ');
		putx(x);
		putc(' ');
		putd(x);
		putnl();
	}
	return 0;
}
