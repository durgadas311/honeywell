#include <stdio.h>

#if defined(IBM026)
#include "myfont026.h"
#elif defined(IBM029)
#include "myfont029.h"
#else
#include "myfont.h"
#endif

// values are 10x
#define DOT_WIDTH 1152

#define DOT_SPACING 1280

#define CHAR_WIDTH (((DOT_SPACING * 6) + 5) / 10)
#define CHAR_HEIGHT (DOT_SPACING * 8)
#define CHAR_BASELINE (DOT_SPACING * 1)

void do_row_dots(int i, int b) {
	int x, y;
	int xt, yt;
	int xxt, yyt;

	x = 0;
	// there is nothing below the baseline...
	y = i * DOT_SPACING;
	while (b) {
		if ((b & 0x10) != 0) {
			xt = (x + 5) / 10;
			yt = (y + 5) / 10;
			yyt = (y + DOT_WIDTH + 5) / 10;
			xxt = (x + DOT_WIDTH + 5) / 10;
			printf("%d %d m 0\n", xt, yt);
			printf(" %d %d l 1\n", xt, yyt);
			printf(" %d %d l 1\n", xxt, yyt);
			printf(" %d %d l 1\n", xxt, yt);
			printf(" %d %d l 1\n", xt, yt);
		}
		x += DOT_SPACING;
		b = (b << 1) & 0x1f;
	}
}

void do_char(int c) {
	int i;
	int b;
	int cc;

	i = c * 8;
	if (i >= sizeof(fontTable)) {
		return;
	}
	unsigned char *cp = &fontTable[i];
	if (cp[0] == 0) {
		return;
	}
	printf("StartChar: uni%04X\n", cp[0]);
	printf("Encoding: %d %d %d\n", cp[0], cp[0], c); // what is 3rd number?
	printf("Width: %d\n", CHAR_WIDTH);
	printf("VWidth: 0\n");
	printf("Flags: HW\n");
	printf("LayerCount: 2\n");
	printf("Fore\n");
	printf("SplineSet\n");
	for (i = 1; i <= 7; ++i) {
		b = cp[i];
		do_row_dots((7 - i), b);
	}
	printf("EndSplineSet\n");
	printf("EndChar\n");
}

int main(int argc, char **argv) {

	int c;

	printf("## Ascent: %d Descent: %d\n", CHAR_HEIGHT, CHAR_BASELINE);
	for (c = 0; c < 256; ++c) {
		do_char(c);
	}
	return 0;
}
