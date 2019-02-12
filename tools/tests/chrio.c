#include <h200io.h>

char str[] = { "Hello, world!" };

int main(argc, argv)
int argc;
char **argv;
{
	char *p;
	p = str;
	while (!isrm(p)) {
		putc(*p++);
	}
	putnl();
	return 0;
}
