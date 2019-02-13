/*
 * This is not executable code!
 *
 * It is just for compiling with -S and
 * examining the resulting assembly.
 */

#include <h200io.h>
#include <stdlib.h>

static int ary[] = {
	100, 5,
	100, 3,
	250, 4
};

static int dv[2];

char str[] = { "here we are" };

char *str2 = "there we go";

int main(argc, argv)
int argc;
char **argv;
{
	asm("//--ary[0]--");
	ary[0];
	asm("//--ary--");
	ary;
	asm("//--dv[0]--");
	dv[0];
	asm("//--dv--");
	dv;
	asm("//--str[0]--");
	str[0];
	asm("//--str2[0]--");
	str2[0];
	asm("//--*argv = str2--");
	*argv = str2;
	asm("//----");
	return 0;
}
