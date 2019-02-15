#include <h200io.h>

int main(argc, argv)
int argc;
char **argv;
{
	int x, y;
	unsigned ux;
	ux = x = 077000077;

	y = x >> 6;
	puto(x);
	print(" >>6 ", 0);
	puto(y);
	putnl();

	y = ux >> 6;
	puto(x);
	print(" >>>6 ", 0);
	puto(y);
	putnl();

	y = x << 3;
	puto(x);
	print(" <<3 ", 0);
	puto(y);
	putnl();

	return 0;
}
