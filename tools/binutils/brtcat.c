/*
 * Program to cat BRF tape segments together into a BRT
 * Adds "1HDR ", "1EOF ", and "1ERI " records (strips off existing ones).
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
extern char hw200[128];

int vflg = 0;
FILE *ofile = NULL;
int reclen = 250;
uint8_t *rec;
int rno = 0;
int seq = 0;
int nseg = 0;

static void printh(uint8_t *b, int l) {
	while (l-- > 0) {
		fputc(hw2ascii[*b++], stderr);
	}
}

static void print_rec(char *tag) {
	int x = 0;
	char *s = tag;
	while (x < reclen && *s) {
		rec[x++] = hw200[*s++ & 0x7f];
	}
	while (x < reclen) {
		rec[x++] = 0;
	}
	rec[x] = 0300;
	if (fwrite(rec, reclen + 1, 1, ofile) != 1) {
		perror(tag);
		return;
	}
	if (vflg) {
		fprintf(stderr, "%3d: %s\n", rno, tag);
	}
	++rno;
}

static int get_seq(uint8_t *buf) {
	return ((buf[4] & 077) << 6) | (buf[5] & 077);
}

static void put_seq(uint8_t *buf, int sq) {
	buf[4] = (sq >> 6) & 077;
	buf[5] = (sq & 077);
}

static int brfdump(uint8_t *buf, int len) {
	int bnr;
	int n;

	bnr = buf[0] & 077;
	switch (bnr) {
	case 050:
	case 054:
		if (nseg++) {
			put_seq(buf, seq + 1);
		}
		seq = 0;
		if (vflg) {
			n = get_seq(buf);
			fprintf(stderr, "%3d: ", rno);
			printh(buf + 10, 8);
			fprintf(stderr, " %d\n", n);
		}
		break;
	case 041:
	case 044:
		// non-header segment records
		break;
	case 042: // bootstrap records
		break;
	case 022: // bootstrap record
		if (vflg) {
			fprintf(stderr, "%3d: BOOTSTRAP\n", rno);
		}
		break;
	case 001: // HDR/EOF/ERI...
		return 1;
	default:
		printf("invalid banner %02o\n", bnr);
		return -1;	// quit processing file
	}
	if (fwrite(buf, reclen + 1, 1, ofile) != 1) {
		perror("fwrite");
		return -1;
	}
	return 0;
}

static void tape(char *file) {
	int e;
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
		e = brfdump(rec, reclen);
		if (e < 0) {
			break;
		}
		++seq;
		if (e == 0) ++rno;
	}
	close(fd);
}

int main(int argc, char **argv) {
	int x;
	extern int optind;
	extern char *optarg;

	while ((x = getopt(argc, argv, "o:r:v")) != EOF) {
		switch(x) {
		case 'o':
			ofile = fopen(optarg, "w");
			if (ofile == NULL) {
				perror(optarg);
				exit(1);
			}
			break;
		case 'r':
			reclen = strtoul(optarg, NULL, 0);
			break;
		case 'v':
			++vflg;
			break;
		}
	}
	if (optind >= argc) {
		fprintf(stderr, "Usage: %s [-v] [-o file] [-r len] mti-file...\n", argv[0]);
		exit(1);
	}
	if (ofile == NULL) ofile = stdout;
	rec = malloc(reclen + 1);
	if (rec == NULL) {
		fprintf(stderr, "out of memory\n");
		exit(1);
	}
	rno = 0;
	seq = 0;
	nseg = 0;
	print_rec("1HDR ");
	for (x = optind; x < argc; ++x) {
		tape(argv[x]);
	}
	print_rec("1EOF ");
	print_rec("1ERI ");
	print_rec("1ERI ");
	if (ofile != stdout) {
		fclose(ofile);
	}
	return 0;
}
