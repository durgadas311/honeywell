#include <h200io.h>

int ary[] = {
	100, 5,
	100, 3,
	250, 4
};

int main(argc, argv)
int argc;
char **argv;
{
	int x;
	for (x = 0; x < 6; ++x) {
		puto(ary[x]);
	}
	return 0;
}
