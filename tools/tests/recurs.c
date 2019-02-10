#include <h200io.h>

void recurs(lev)
int lev;
{
	int l;
	l = lev + 1;
	print("recurse level ", 0);
	puto(l);
	print(" enter", PR_NL);
	if (l < 5) {
		recurs(l);
	}
	print("recurse level ", 0);
	puto(l);
	print(" exit", PR_NL);
}

int main(argc, argv)
int argc;
char **argv;
{
	print("main begin", PR_NL);
	recurs(0);
	print("main end", PR_NL);
	return 0;
}
