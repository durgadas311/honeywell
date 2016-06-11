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
#define DOT_DOT_WIDTH	(DOT_SPACING - 80)

#define CHAR_WIDTH (((DOT_SPACING * 6) + 5) / 10)
#define CHAR_HEIGHT (DOT_SPACING * 8)
#define CHAR_BASELINE (DOT_SPACING * 1)

// tan(22.8) ~= 0.421 ...
static int dd;
static int ds;
static int dt;

void do_row_dots(int i, int b) {

	int x, y;
	int xt, yt;
	int xddt, xdtt, xdddst, xdddsdtt, xdd_dst, xdwt;
	int yddt, ydst, ydsdtt, y_dst, y_ddt, y_dsdtt;
/*
	int xddt, xdddst, xdd_dst, xdwt;
	int yddt, ydst, y_dst, y_ddt;
*/

	x = 0;
	// there is nothing below the baseline...
	y = (i * DOT_SPACING) + (DOT_DOT_WIDTH / 2);
	while (b) {
		if ((b & 0x10) != 0) {
			xt = (x + 5) / 10;
			yt = (y + 5) / 10;
			xdtt = (x + dt + 5) / 10;
			xddt = (x + dd + 5) / 10;
			xdd_dst = (x + dd - ds + 5) / 10;
			xdddst = (x + dd + ds + 5) / 10;
			xdddsdtt = (x + dd + ds + dt + 5) / 10;
			xdwt = (x + DOT_DOT_WIDTH + 5) / 10;
			ydst = (y + ds + 5) / 10;
			yddt = (y + dd + 5) / 10;
			ydsdtt = (y + ds + dt + 5) / 10;
			y_dst = (y - ds + 5) / 10;
			y_dsdtt = (y - ds - dt + 5) / 10;
			y_ddt = (y - dd + 5) / 10;

			printf("%d %d m 0\n", xt, yt);
			printf(" %d %d %d %d %d %d c 0\n",
				xt, ydst,
				xt, ydst,
				xdtt, ydsdtt);
			printf(" %d %d %d %d %d %d c 0\n",
				xdd_dst, yddt,
				xdd_dst, yddt,
				xddt, yddt);
			printf(" %d %d %d %d %d %d c 0\n",
				xdddst, yddt,
				xdddst, yddt,
				xdddsdtt, ydsdtt);
			printf(" %d %d %d %d %d %d c 0\n",
				xdwt, ydst,
				xdwt, ydst,
				xdwt, yt);
			printf(" %d %d %d %d %d %d c 0\n",
				xdwt, y_dst,
				xdwt, y_dst,
				xdddsdtt, y_dsdtt);
			printf(" %d %d %d %d %d %d c 0\n",
				xdddst, y_ddt,
				xdddst, y_ddt,
				xddt, y_ddt);
			printf(" %d %d %d %d %d %d c 0\n",
				xdd_dst, y_ddt,
				xdd_dst, y_ddt,
				xdtt, y_dsdtt);
			printf(" %d %d %d %d %d %d c 0\n",
				xt, y_dst,
				xt, y_dst,
				xt, yt);
		}
		x += DOT_SPACING;
		b = (b << 1) & 0x1f;
	}
}

void do_row_squares(int i, int b) {
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
#ifdef DOTS
		do_row_dots((7 - i), b);
#else
		do_row_squares((7 - i), b);
#endif
	}
	printf("EndSplineSet\n");
	printf("EndChar\n");
}

int main(int argc, char **argv) {

	int c;

	dd = (DOT_DOT_WIDTH / 2);
	ds = (((dd) * 421) + 500) / 1000;
	dt = ((dd - ds) / 2);

	printf("## Ascent: %d Descent: %d\n", CHAR_HEIGHT, CHAR_BASELINE);
	for (c = 0; c < 256; ++c) {
		do_char(c);
	}
	return 0;
}
