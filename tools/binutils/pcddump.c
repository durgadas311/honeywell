/*
 * Program to dump card deck data.
 * Converts "Hollerith-based punch codes" to equivalent ASCII
 * text based on Honeywell 200/2000 assignments.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <strings.h>

// This defines how punch codes (condensed) are printed by H200/2000.
static char xlate_pun[128] = {
[0x00] = ' ',	// no punch
[0x09] = '9',	// [9]
[0x08] = '8',	// [8]
[0x07] = '7',	// [7]
[0x06] = '6',	// [6]
[0x05] = '5',	// [5]
[0x04] = '4',	// [4]
[0x03] = '3',	// [3]
[0x02] = '2',	// [2]
[0x01] = '1',	// [1]
[0x10] = '0',	// [0]

[0x49] = 'I',	// [12][9]
[0x48] = 'H',	// [12][8]
[0x47] = 'G',	// [12][7]
[0x46] = 'F',	// [12][6]
[0x45] = 'E',	// [12][5]
[0x44] = 'D',	// [12][4]
[0x43] = 'C',	// [12][3]
[0x42] = 'B',	// [12][2]
[0x41] = 'A',	// [12][1]

[0x29] = 'R',	// [11][9]
[0x28] = 'Q',	// [11][8]
[0x27] = 'P',	// [11][7]
[0x26] = 'O',	// [11][6]
[0x25] = 'N',	// [11][5]
[0x24] = 'M',	// [11][4]
[0x23] = 'L',	// [11][3]
[0x22] = 'K',	// [11][2]
[0x21] = 'J',	// [11][1]

[0x19] = 'Z',	// [0][9]
[0x18] = 'Y',	// [0][8]
[0x17] = 'X',	// [0][7]
[0x16] = 'W',	// [0][6]
[0x15] = 'V',	// [0][5]
[0x14] = 'U',	// [0][4]
[0x13] = 'T',	// [0][3]
[0x12] = 'S',	// [0][2]
[0x11] = '/',	// [0][1]

// depends on "special code" setting
[0x40] = '?',	// [12]
[0x50] = '+',	// [12][0]
[0x20] = '!',	// [11]
[0x30] = '-',	// [11][0]

[0x0a] = '\'',	// [8][2]
[0x0b] = '=',	// [8][3]
[0x0c] = ':',	// [8][4]
[0x0d] = '<',	// [8][5]
[0x0e] = '>',	// [8][6]
[0x0f] = '&',	// [8][7]

[0x4a] = ';',	// [12][8][2]
[0x4b] = '.',	// [12][8][3]
[0x4c] = ')',	// [12][8][4]
[0x4d] = '%',	// [12][8][5]
[0x4e] = ']',	// [12][8][6] solid lozenge
[0x4f] = '|',	// [12][8][7] "|" Not HW

[0x2a] = '#',	// [11][8][2]
[0x2b] = '$',	// [11][8][3]
[0x2c] = '*',	// [11][8][4]
[0x2d] = '"',	// [11][8][5]
[0x2e] = '\\',	// [11][8][6] not equal
[0x2f] = '`',	// [11][8][7] broken bar (not HW)

[0x1a] = '@',	// [0][8][2]
[0x1b] = ',',	// [0][8][3]
[0x1c] = '(',	// [0][8][4]
[0x1d] = '~',	// [0][8][5] CR
[0x1e] = '[',	// [0][8][6] open lozenge
[0x1f] = '^',	// [0][8][7] cent
};

int nflg = 0;
int sflg = 0;
unsigned short card[80];
int num = 0;

static void set_special() {
	xlate_pun[0x20] = '-';
	xlate_pun[0x30] = '!';
	xlate_pun[0x40] = '+';
	xlate_pun[0x50] = '?';
}

// Converts Hollerith-based punch code to compact code (NOT binary punch!).
// Returns byte "0yxznnnn" where 'y' is zone 12, 'x' is zone 11,
// 'z' is zone zero, 'nnnn' = 1-9 if exactly one punch 1-9,
// 'nnnn' = 10-15 for punch [8][2-7], 'nnnn' = 0 if none (zone only).
// Returns 1xxxxxxx for invalid punch codes.
static int pun2code(int pun) {
	// some HW punch codes have 2 zone punches ([12][0] and [11][0]).
	int b = ((pun & 0x0e00) >> 5); // -yxz----
	int p = (pun & 0x01ff);
	if (p == 0) { // zone-only
		return b;
	}
	if ((p & (p - 1)) == 0) { // only one punch... simple
		int n = ffs(p); // 1 means 9, 2 is 8, ...
		b |= (10 - n);     // 1-9 for punches 1-9
		return b;
	}
	if ((p & 0x0103) == 0x0002) { // [8]+ but no [1] or [9]
		p &= ~0x0002;
		if ((p & (p - 1)) == 0) { // only one other punch... valid
			int n = ffs(p);
			// 2 means 7, 3 means 6,...
			// want 2 => 15, 3 => 14, ... 
			b |= (18 - n);
			return b;
		}       
	}
	return 0x80;	// invalid (unsupported) punch
}

static char pun2ascii(int pun) {
	int b = pun2code(pun);
	if (!b) return ' ';
	if (b & 0x80) return 0;	// TODO: what's the best "invalid char"?
	b = xlate_pun[b];
	if (!b) return 0; // TODO: what's the best "invalid char"?
	return b;
}

static void deck(char *file) {
	int c;
	int x;
	int fd = open(file, O_RDONLY);
	if (fd < 0) {
		perror(file);
		return;
	}
	while (read(fd, card, sizeof(card)) > 0) {
		++num;
		if (nflg) {
			printf("%5d: ", num);
		}
		for (c = 0; c < 80; ++c) {
			x = pun2ascii(card[c]);
			if (x) putchar(x);
			else putchar('{');
		}
		putchar('\n');
	}
	close(fd);
}

int main(int argc, char **argv) {
	int x;
	extern int optind;

	while ((x = getopt(argc, argv, "ns")) != EOF) {
		switch(x) {
		case 'n':
			++nflg;
			break;
		case 's':
			++sflg;
			break;
		}
	}
	if (optind >= argc) {
		fprintf(stderr, "Usage: %s [-n] [-s] pcd-file...\n", argv[0]);
		exit(1);
	}
	if (sflg) {
		set_special();
	}
	for (x = optind; x < argc; ++x) {
		deck(argv[x]);
	}
}
