#include <h200io.h>

int main(argc, argv)
int argc;
char **argv;
{
	int x, y, z;
	for (x = 0; x < 8; ++x) {
		y = x * 5 / 3;
		z = y % 13;
		y = x * 12;
/*
		puto(y);
		putnl();
		print(argv[x], PR_SPB | PR_NL);
*/
	}
	return 0;
}
