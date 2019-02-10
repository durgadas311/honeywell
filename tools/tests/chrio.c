#include <h200io.h>

char str[] = { "Hello, world!" };

char *str2 = "Goodbye!";

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
