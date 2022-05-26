/*
 * Program to dump BRF data, from cards or tape
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <strings.h>

static char hw2ascii[64] = {
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9','\'', '=', ':', ' ', '>', '&',
	'+', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', ';', '.', ')', '%', ']', '?',
	'-', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', '#', '$', '*', '"','\\', '!',
	'<', '/', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '@', ',', '(', '~', '[', '^',
};

// This defines how punch codes (condensed) are converted to HW codes
static char xlate_pun[128] = {
[0x00] = 015,	// no punch
[0x09] = 011,	// [9]
[0x08] = 010,	// [8]
[0x07] = 007,	// [7]
[0x06] = 006,	// [6]
[0x05] = 005,	// [5]
[0x04] = 004,	// [4]
[0x03] = 003,	// [3]
[0x02] = 002,	// [2]
[0x01] = 001,	// [1]
[0x10] = 000,	// [0]

[0x49] = 031,	// [12][9]
[0x48] = 030,	// [12][8]
[0x47] = 027,	// [12][7]
[0x46] = 026,	// [12][6]
[0x45] = 025,	// [12][5]
[0x44] = 024,	// [12][4]
[0x43] = 023,	// [12][3]
[0x42] = 022,	// [12][2]
[0x41] = 021,	// [12][1]

[0x29] = 051,	// [11][9]
[0x28] = 050,	// [11][8]
[0x27] = 047,	// [11][7]
[0x26] = 046,	// [11][6]
[0x25] = 045,	// [11][5]
[0x24] = 044,	// [11][4]
[0x23] = 043,	// [11][3]
[0x22] = 042,	// [11][2]
[0x21] = 041,	// [11][1]

[0x19] = 071,	// [0][9]
[0x18] = 070,	// [0][8]
[0x17] = 067,	// [0][7]
[0x16] = 066,	// [0][6]
[0x15] = 065,	// [0][5]
[0x14] = 064,	// [0][4]
[0x13] = 063,	// [0][3]
[0x12] = 062,	// [0][2]
[0x11] = 061,	// [0][1]

// depends on "special code" setting
[0x40] = 037,	// [12]
[0x50] = 020,	// [12][0]
[0x20] = 057,	// [11]
[0x30] = 040,	// [11][0]

[0x0a] = 012,	// [8][2]
[0x0b] = 013,	// [8][3]
[0x0c] = 014,	// [8][4]
[0x0d] = 060,	// [8][5]
[0x0e] = 016,	// [8][6]
[0x0f] = 017,	// [8][7]

[0x4a] = 032,	// [12][8][2]
[0x4b] = 033,	// [12][8][3]
[0x4c] = 034,	// [12][8][4]
[0x4d] = 035,	// [12][8][5]
[0x4e] = 036,	// [12][8][6] solid lozenge
[0x4f] = 015,	// [12][8][7] Not HW

[0x2a] = 052,	// [11][8][2]
[0x2b] = 053,	// [11][8][3]
[0x2c] = 054,	// [11][8][4]
[0x2d] = 055,	// [11][8][5]
[0x2e] = 056,	// [11][8][6] not equal
[0x2f] = 015,	// [11][8][7] broken bar (not HW)

[0x1a] = 072,	// [0][8][2]
[0x1b] = 073,	// [0][8][3]
[0x1c] = 074,	// [0][8][4]
[0x1d] = 075,	// [0][8][5] CR
[0x1e] = 076,	// [0][8][6] open lozenge
[0x1f] = 077,	// [0][8][7] cent
};

int cflg = 0;
int dflg = 0;
int oflg = 0;
int sflg = 0;
int reclen = 250;
uint8_t *rec;
int rno = 0;
int dist = 0;
unsigned short card[80];

static void set_special() {
	xlate_pun[0x20] = 040;
	xlate_pun[0x30] = 057;
	xlate_pun[0x40] = 020;
	xlate_pun[0x50] = 037;
}

static void printh(uint8_t *b, int l) {
	while (l-- > 0) {
		putchar(hw2ascii[*b++]);
	}
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

static char pun2hw(int pun) {
	int b = pun2code(pun);
	if (!b) return 015;
	if (b & 0x80) return 0;	// TODO: what's the best "invalid char"?
	b = xlate_pun[b];
	//if (!b) return 0; // TODO: what's the best "invalid char"?
	return b;
}

static int brfadr(uint8_t *buf, int idx, int len) {
	int adr;
	if (idx + 3 > len) return -1;
	adr = ((buf[idx] & 077) << 12) |
		((buf[idx + 1] & 077) << 6) |
		(buf[idx + 2] & 077);
	return adr;
}

static int get_seq(uint8_t *buf) {
	if (cflg) {
		return ((buf[1] & 017) * 100) +
			((buf[2] & 017) * 10) +
			(buf[3] & 017);
	} else {
		return ((buf[4] & 077) << 6) | (buf[5] & 077);
	}
}

static int brfdump(uint8_t *buf, int len) {
	int x = 0;
	int bnr;
	int ctl;
	int a1, a2;
	int f, n, p;

	bnr = buf[x++] & 077;
	switch (bnr) {
	case 050:
	case 054:
		n = get_seq(buf);
		printf("%02o: seq %d segment \"", bnr, n);
		printh(buf + 10, 8);
		printf("\" rev \"");
		printh(buf + 7, 3);
		if (!cflg) {
			printf("\" vis \"");
			printh(buf + 18, 6);
		}
		printf("\"\n");
		break;
	case 041:
	case 044:
	case 042: // should only be tape format
		n = get_seq(buf);
		printf("%02o: seq %d\n", bnr, n);
		break;
	default:
		printf("invalid banner %02o\n", bnr);
		return -1;	// quit processing file
	}
	x = buf[6];

	while (x < len) {
		if (oflg) {
			printf("%4d %3d: ", rno, x);
		} else if (dflg) {
			printf("%07o: ", dist);
		}
		ctl = buf[x++] & 077;
		if (ctl < 060) {
			n = ctl & 017;
			p = (ctl & 060) >> 4;
			dist += n;
			printf("string %c", "-WIR"[p]);
			if (n == 0) {
				printf(" zero length!");
			} else while (x < len && n-- > 0) {
				printf(" %02o", buf[x++] & 077);
			}
			if (x >= len) printf("!");
		} else switch (ctl) {
			case 060:	// set DIST
			case 070:
				printf("setdist");
				a1 = brfadr(buf, x, len);
				dist = a1 | ((ctl & 010) << 15);
				x += 3;
				if (a1 < 0) printf("!");
				else printf(" %07o", dist);
				break;
			case 061:	// end, START (also end record)
			case 071:
				printf("endload");
				a1 = brfadr(buf, x, len);
				if (a1 < 0) printf("!\n");
				else printf(" %07o\n", a1 | ((ctl & 010) << 15));
				return 0;	// TODO: check bnr?
			case 062:	// clear START END FILL
			case 072:
				printf("clear");
				a1 = brfadr(buf, x, len);
				x += 3;
				a2 = brfadr(buf, x, len);
				x += 3;
				f = (x < len ? buf[x++] & 077 : -1); 
				if (a1 < 0 || a2 < 0 || f < 0) printf("!");
				else printf(" %07o %07o %02o",
					a1 | ((ctl & 010) << 15),
					a2 | ((ctl & 010) << 15), f);
				break;
			case 063:	// SW DIST-1
				printf("set W %07o", dist - 1);
				break;
			case 064:	// SI DIST-1
				printf("set I %07o", dist - 1);
				break;
			case 077:	// end of record
				printf("eor -----\n");
				return 0;	// TODO: check bnr?
			default:
				printf("invalid: %02o", ctl);
				break;
		}
		printf("\n");
	}
	// should never get here
	printf("run off record end\n");
	return -1;
}

static void deck(char *file) {
	int c;
	int fd = open(file, O_RDONLY);
	if (fd < 0) {
		perror(file);
		return;
	}
	while (read(fd, card, sizeof(card)) > 0) {
		for (c = 0; c < 80; ++c) {
			rec[c] = pun2hw(card[c]);
		}
		if (brfdump(rec, 80)) {
			break;
		}
		++rno;
	}
	close(fd);
}

static void tape(char *file) {
	int fd = open(file, O_RDONLY);
	if (fd < 0) {
		perror(file);
		return;
	}
	while (read(fd, rec, reclen + 1) > 0) {
		if ((rec[reclen] & 0300) != 0300) {
			fprintf(stderr, "%s: record too long/short\n", file);
			exit(1);
		}
		if (brfdump(rec, reclen)) {
			break;
		}
		++rno;
	}
	close(fd);
}

int main(int argc, char **argv) {
	int x;
	extern int optind;
	extern char *optarg;

	while ((x = getopt(argc, argv, "cdD:or:s")) != EOF) {
		switch(x) {
		case 'c':
			++cflg;
			break;
		case 'd':
			++dflg;
			break;
		case 'D':
			++dflg;
			dist = strtoul(optarg, NULL, 0);
			break;
		case 'o':
			++oflg;
			break;
		case 'r':
			reclen = strtoul(optarg, NULL, 0);
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
	if (cflg) {
		reclen = 80;
	}
	rec = malloc(reclen + 1);
	if (rec == NULL) {
		fprintf(stderr, "out of memory\n");
		exit(1);
	}
	for (x = optind; x < argc; ++x) {
		if (cflg) {
			deck(argv[x]);
		} else {
			tape(argv[x]);
		}
	}
}
