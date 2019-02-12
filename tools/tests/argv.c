#include <h200io.h>

int main(argc, argv)
int argc;
char **argv;
{
	int x;
	for (x = 0; x < argc; ++x) {
		puto(x);
		print(argv[x], PR_SPB | PR_NL);
	}
	return 0;
}
