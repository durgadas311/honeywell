#include <h200io.h>

void panic(str)
char *str;
{
	print(str, PR_NL);
	asm("	h	.");
}
