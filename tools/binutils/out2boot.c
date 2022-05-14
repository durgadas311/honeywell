/* convert a.out to a boot image, punch cards or mag tape */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#include <string.h>
#include "a.out.h"

static struct exec hdr;
static int reclen;
static uint16_t cardbuf[80];
static uint8_t tapebuf[251];

static char *ofname = NULL;
static int cflg;	// card deck output (else tape)
static int sflg;	// HW "special" punch card codes

extern unsigned short hw2pc[64];
extern unsigned short hw2pc_[64];	// if "special" code selected

static int tape_rec(uint8_t *buf, int len, FILE *of) {
	int x;
	for (x = 0; x < len; ++x) {
		tapebuf[x] = buf[x] & 077;
	}
	for (; x < reclen; ++x) {
		tapebuf[x] = 0;
	}
	tapebuf[250] = 0300;	// IRG
	fwrite(tapebuf, sizeof(tapebuf), 1, of);
}

static int card(uint8_t *buf, int len, FILE *of) {
	int x;
	for (x = 0; x < len; ++x) {
		if (sflg) {
			cardbuf[x] = hw2pc_[buf[x] & 077];
		} else {
			cardbuf[x] = hw2pc[buf[x] & 077];
		}
	}
	for (; x < reclen; ++x) {
		cardbuf[x] = 0;
	}
	fwrite(cardbuf, sizeof(cardbuf), 1, of);
	return 0;
}

static char *do_out(FILE *fp, FILE *of) {
	uint8_t *buf;
	int len = hdr.a_text + hdr.a_data;

	reclen = cflg ? 80 : 250;
	buf = malloc(len);
	if (buf == NULL) {
		return "out of memory";
	}
	fseek(fp, (off_t)sizeof(hdr), SEEK_SET);
	if (fread(buf, len, 1, fp) != 1) {
		return "corrupt file";
	}
	int num = len;
	int n;
	uint8_t *b = buf;
	while (num > 0) {
		n =  (num >= reclen ? reclen : num);
		if (cflg) {
			card(b, n, of);
		} else {
			tape_rec(b, n, of);
		}
		b += reclen;
		num -= n;
	}
	return NULL;
}

static void objdump(char *f) {
	FILE *fp;
	fp = fopen(f, "r");
	if (fp == NULL) {
		perror(f);
		return;
	}
	if (fread(&hdr, sizeof(hdr), 1, fp) != 1 ||
			N_BADMAG(hdr)) {
		fprintf(stderr, "%s: Not an object file\n", f);
		fclose(fp);
		return;
	}
	char *err = NULL;
	FILE *of = ofname ? fopen(ofname, "w") : stdout;
	if (of == NULL) {
		perror(ofname);
		return;
	}
	err = do_out(fp, of);
	if (err) {
		fprintf(stderr, "%s: %s\n", f, err);
	}
	fclose(fp);
	if (ofname) fclose(of);
}

int main(int argc, char **argv) {
	extern int optind;
	extern char *optarg;
	int x;
	while ((x = getopt(argc, argv, "co:s")) != EOF) {
		switch(x) {
		case 'c':
			++cflg;
			break;
		case 's':
			++sflg;
			break;
		case 'o':
			ofname = optarg;
			break;
		}
	}
	if (optind + 1 != argc) {
		fprintf(stderr,	"Usage: %s [options] <a.out-file>\n"
				"Options:\n"
				"    -c      Output card deck instead of tape\n"
				"    -s      Use HW special punch codes (-c)\n"
				"    -o file Ouput to file instead of stdout\n",
			argv[0]);
		return 0;
	}
	objdump(argv[optind]);
	return 0;
}
