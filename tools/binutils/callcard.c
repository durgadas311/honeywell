// Copyright (c) 2019 Douglas Miller <durgadas311@gmail.com>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

extern char hw200[128];
extern unsigned short hw2pc[64];
extern unsigned short hw2pc_[64];

static int dflg = 0;	// tape drive
static int sflg = 0;
static char *ofname = NULL;
static FILE *of = NULL;	// file or stdout
static char *prog = NULL;
static char *segm = NULL;
static char *halt = "        ";

static uint16_t card[80];

// Return string 'max' chars long, blank-padded or truncated.
// Also toupper(). Dot also terminates input string.
static char *trunc_pad(char *in, int max, int left) {
	char *out = malloc(max + 1);
	int x;
	if (out == NULL) {
		return NULL;
	}
	if (!left) {
		// 'in' might be a filename
		for (x = 0; x < max; ++x) {
			if (!in[x] || in[x] == '.') break;
			out[x] = toupper(in[x]);
		}
		while (x < max) {
			out[x++] = ' ';
		}
	} else {
		// 'in' is just alpha-numeric
		int l = strlen(in);
		x = 0;
		while (x < max - l) {
			out[x++] = '0';
		}
		while (x < max) {
			out[x] = toupper(in[x - (max - l)]);
			++x;
		}
	}
	out[x] = '\0';
	return out;
}

#define chcnv(c) (sflg ? hw2pc_[hw200[c]] : hw2pc[hw200[c]])

static char *punch_rec() {
	int x;
	char *s;

	x = 0;
	s = prog;
	while (*s) {
		card[x++] = chcnv(*s);
		++s;
	}
	s = segm;
	while (*s) {
		card[x++] = chcnv(*s);
		++s;
	}
	card[x++] = hw2pc[dflg & 07];
	s = halt;
	while (*s) {
		card[x++] = chcnv(*s);
		++s;
	}
	card[x++] = chcnv('*');
	while (x < 80) {
		card[x++] = 0x000;
	}
	if (fwrite(card, sizeof(card), 1, of) != 1) {
		return strerror(errno);
	}
	return NULL;
}

static void callcard() {
	char *err = NULL;
	of = ofname ? fopen(ofname, "a") : stdout;
	if (of == NULL) {
		perror(ofname);
		return;
	}
	err = punch_rec();
	if (err) {
		fprintf(stderr, "%s\n", err);
	}
	if (ofname) fclose(of);
}

int main(int argc, char **argv) {
	extern int optind;
	extern char *optarg;
	int x;
	while ((x = getopt(argc, argv, "d:o:sP:H:S:")) != EOF) {
		switch(x) {
		case 'd':
			dflg = strtoul(optarg, NULL, 0);
			break;
		case 's':
			++sflg;
			break;
		case 'o':
			ofname = optarg;
			break;
		case 'P':
			prog = trunc_pad(optarg, 6, 0);
			break;
		case 'H':
			halt = trunc_pad(optarg, 8, 0);
			break;
		case 'S':
			segm = trunc_pad(optarg, 2, 1);
			break;
		default:
			goto usage;
		}
	}
	if (optind != argc || !prog || !segm) {
usage:
		fprintf(stderr,	"Usage: %s [options]\n"
				"Options:\n"
				"    -s      Use HW special punch codes (-c)\n"
				"    -o file Append to file instead of stdout\n"
				"*   -P str  Use str as program name\n"
				"*   -S str  Use str as program segment\n"
				"    -H str  Use str as halt name\n"
				"* required options\n",
			argv[0]);
		return 0;
	}
	callcard();
	return 0;
}
