#include <h200io.h>

static char buf[100];

int main(argc, argv)
int argc;
char **argv;
{
	int n;
	strcpy(buf, "this");
	strcat(buf, " and that: ");
	n = strlen(buf);
	print(buf, 0);
	puto(n);
	putnl();
	return 0;
}
