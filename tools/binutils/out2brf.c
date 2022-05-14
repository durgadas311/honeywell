// Copyright (c) 2019 Douglas Miller <durgadas311@gmail.com>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include "a.out.h"

extern char hw200[128];
extern unsigned short hw2pc[64];
extern unsigned short hw2pc_[64];

static struct exec hdr;
static int cflg = 0;
static int sflg = 0;
static char *ofname = NULL;
static FILE *of = NULL;	// file or stdout
static long visi = 0400000000000L;
static int revi = 0;
static char *prog = NULL;
static char *segm = "01";

static int dist = -1;
static int reclen;
static int reccnt;
static uint8_t *record;
static uint16_t card[80];
static int dirty = 0;
static int seq;
static long vis;
static int rev;

static int setup_brt();
static int fin_rec(int last);
static void put_len(int len);
static void put_seq(int seq);
static void init_rec();
static void put_str(char *str);
static int init_seg();
static int begin(int adr);
static void put_adr(int adr);
static void end_rec(int last);
static int mk_space(int len);
static int set_adr(int adr, int cc);
static int kludge(int adr, uint8_t *code, int len);
static int set_code_i(int adr, uint8_t *code, uint8_t ctrl, int start, int end);
static int set_code(int adr, uint8_t *code, int len);
static int clear(int start, int end, uint8_t fill);
static int range(int start, int end);
static int exec(int start);
static int end(int start);

// Return string 'max' chars long, blank-padded or truncated.
// Also toupper(). Dot also terminates input string.
static char *trunc_pad(char *in, int max) {
	char *out = malloc(max + 1);
	if (out == NULL) {
		return NULL;
	}
	int x;
	for (x = 0; x < max; ++x) {
		if (!in[x] || in[x] == '.') break;
		out[x] = toupper(in[x]);
	}
	while (x < max) {
		out[x++] = ' ';
	}
	out[x] = '\0';
	return out;
}

static int punch_rec(uint8_t *rec, int len) {
	int x;

	while (len < reclen) {
		rec[len++] = 015;	// blank on card
	}
	for (x = 0; x < reclen; ++x) {
		if (sflg) {
			card[x] = hw2pc_[rec[x] & 077];
		} else {
			card[x] = hw2pc[rec[x] & 077];
		}
	}
	return fwrite(card, sizeof(card), 1, of) == 1;
}

// return 0 (false) on failure
static int write_rec(uint8_t *rec, int len) {
	if (cflg) {
		return punch_rec(rec, len);
	}
	while (len < reclen) {
		rec[len++] = 0;
	}
	rec[len] = 0300;
	return fwrite(rec, reclen + 1, 1, of) == 1;
}
static int end_seg() {
	// close handled elsewhere
	return 1;
}
static int begin_seg(char *rev, char *prg, char *seg, long vis) {
	// "open" output file... already done
	return 1;
}

static int setup_brt() {
	reccnt = 0;
	seq = 0;
	return 0;
}

static int fin_rec(int last) {
	end_rec(last);
	++seq;
	put_len(reccnt);
	if (last) {
		record[0] &= ~07;
		record[0] |= 04;
	}
	if (!write_rec(record, reccnt)) {
		return 0;
	}
	dirty = 0;
	if (last && !end_seg()) {
		return 0;
	}
	return 1;
}

static void put_len(int len) {
	record[1] = (uint8_t)((len >> 12) & 077);
	record[2] = (uint8_t)((len >> 6) & 077);
	record[3] = (uint8_t)((len >> 0) & 077);
}

static void put_seq(int seq) {
	record[4] = (uint8_t)((seq >> 6) & 077);
	record[5] = (uint8_t)((seq >> 0) & 077);
}

static void init_rec() {
	reccnt = 0;
	record[0] = (uint8_t)041; // modified to 044 at end if last
	put_len(0);	// updated later...
	put_seq(seq);
	reccnt = 7;
	record[6] = (uint8_t)reccnt;
}

// Strings must have already been truncated/padded to exact field length.
static void put_str(char *str) {
	while (*str) {
		record[reccnt++] = hw200[*str++ & 0x7f];
	}
}

// initialize segment record (first of program, maybe also last)
static int init_seg() {
	char rv[8];
	record[0] = (uint8_t)050; // modified to 054 at end if last
	if (cflg) {
		reccnt = 1;
		sprintf(rv, "%03d", seq % 1000);
		put_str(rv);	// TODO: string or binary?
	} else {
		put_len(0);	// updated later...
		put_seq(seq);	//
	}
	reccnt = 7;
	sprintf(rv, "%03d", revi % 1000);
	put_str(rv);	// TODO: string or binary?
	put_str(prog);
	put_str(segm);
	// visi not used by card loaders...
	put_adr((int)(visi >> 18));
	put_adr((int)(visi));
	// assert reccnt == 24...
	reccnt = 24;
	record[6] = (uint8_t)reccnt; // header length
	seq = 1;
	return 1;
}

static int begin(int adr) {
	if (!init_seg()) {
		return 0;
	}
	// set_adr(adr); // let first set_code do this...
	dist = -1;
	dirty = 0;
	return 1;
}

void init_rec();
int fin_rec(int last);

static void put_adr(int adr) {
	record[reccnt++] = (uint8_t)((adr >> 12) & 077);
	record[reccnt++] = (uint8_t)((adr >> 6) & 077);
	record[reccnt++] = (uint8_t)((adr >> 0) & 077);
}

static void end_rec(int last) {
	if (!last) {
		record[reccnt++] = 077;	// read next record
	}
}

// Data always follows...
static int mk_space(int len) {
	dirty = 1;
	if (reccnt + len >= reclen) {
		if (!fin_rec(0)) {
			return 0;
		}
		init_rec();
	}
	return 1;
}

static int set_adr(int adr, int cc) {
	if (!mk_space(4)) {
		return 0;
	}
	if (cc == 060) dist = adr;
	if (adr > 0777777) cc |= 010;
	record[reccnt++] = (uint8_t)cc;
	put_adr(adr);
	return 1;
}

static int kludge(int adr, uint8_t *code, int len) {
	if (!set_code(adr, code, 1) || !set_code(adr + 1, code + 1, len - 1)) {
		return 0;
	}
	return 1;
}

// Only called for lengths <= 15
static int set_code_i(int adr, uint8_t *code, uint8_t ctrl, int start, int end) {
	int len = (end - start);
	ctrl |= len;
	if (!mk_space(len + 1)) {
		return 0;
	}
	record[reccnt++] = ctrl;
	for (int y = start; y < end; ++y) {
		record[reccnt++] = (uint8_t)(code[y] & 077);
	}
	dist += (end - start);
	return 1;
}

// TODO: reloc should be 0...
static int set_code(int adr, uint8_t *code, int len) {
	uint8_t ctrl = 0;
	// TODO: how is RM handled? Is RM ever at start of field?
	// 1-char segments use the post-punctuation method...
	if (len > 1 || (code[0] & 0300) != 0300) {
		if ((code[0] & 0300) == 0300) {
			// Must handle special case that doesn't fit BRT...
			return kludge(adr, code, len);
		} else if ((code[0] & 0100) != 0) {
			ctrl |= 0020;
		} else if ((code[0] & 0200) != 0) {
			ctrl |= 0040;
		}
	}
	if (dist != adr && !set_adr(adr, 060)) {
		return 0;
	}
	int n = 0;
	while (len - n > 15) {
		if (!set_code_i(adr, code, ctrl, n, n + 15)) {
			return 0;
		}
		n += 15;
		adr += 15;
		ctrl = 0;
	}
	if (!set_code_i(adr, code, ctrl, n, len)) {
		return 0;
	}
	if (len == 1 && (code[0] & 0300) != 0300) {
		return 1;
	}
	// handle punc in last char
	if ((code[len - 1] & 0100) != 0) {
		if (!mk_space(1)) {
			return 0;
		}
		record[reccnt++] = (uint8_t)063;
	}
	if ((code[len - 1] & 0200) != 0) {
		if (!mk_space(1)) {
			return 0;
		}
		record[reccnt++] = (uint8_t)064;
	}
	return 1;
}

// either (start > 0777777 && end > 0777777)
//     or (start <= 0777777 && end <= 0777777)
// TODO: if spans boundary, split into two CLEARs.
static int clear(int start, int end, uint8_t fill) {
	if (!mk_space(8) || !set_adr(start, 062)) {
		return 0;
	}
	put_adr(end);
	record[reccnt++] = fill;
	return 1;
}

static int range(int start, int end) {
	if (!set_adr(start, 060) || !set_adr(end, 060)) {
		return 0;
	}
	return 1;
}

static int exec(int start) {
	return end(start);
}

static int end(int start) {
	set_adr(start, 061);
	return fin_rec(1);
}

static char *do_out(FILE *fp) {
	uint8_t *buf;
	int len = hdr.a_text + hdr.a_data;
	int x, y;

	buf = malloc(len + 1);
	if (buf == NULL) {
		return "out of memory";
	}
	reclen = cflg ? 80 : 250;
	record = malloc(reclen + 1);
	if (record == NULL) {
		return "out of memory";
	}
	fseek(fp, (off_t)sizeof(hdr), SEEK_SET);
	if (fread(buf, len, 1, fp) != 1) {
		return "corrupt file";
	}
	begin(hdr.a_entry);
	// TODO: end of range inclusive or exclusive?
	range(hdr.a_entry, hdr.a_entry + len + hdr.a_bss + hdr.a_heap);
	x = 0;
	while (x < len) {
		for (y = x + 1; y < len; ++y) {
			if ((buf[y] & 0300) != 0) break;
		}
		set_code(x + hdr.a_entry, buf + x, y - x);
		x = y;
	}
	// TODO: add clear() for .bss? .heap?
	exec(hdr.a_entry);
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
	of = ofname ? fopen(ofname, "w") : stdout;
	if (of == NULL) {
		perror(ofname);
		return;
	}
	err = do_out(fp);
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
	while ((x = getopt(argc, argv, "co:sP:R:S:V:")) != EOF) {
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
		case 'P':
			prog = trunc_pad(optarg, 6);
			break;
		case 'R':
			revi = strtoul(optarg, NULL, 0);
			break;
		case 'S':
			segm = trunc_pad(optarg, 2);
			break;
		case 'V':
			visi = strtoul(optarg, NULL, 0);
			break;
		}
	}
	if (optind + 1 != argc) {
		fprintf(stderr,	"Usage: %s [options] <a.out-file>\n"
				"Options:\n"
				"    -c      Output card deck instead of tape\n"
				"    -s      Use HW special punch codes (-c)\n"
				"    -o file Ouput to file instead of stdout\n"
				"    -P str  Use str as program name\n"
				"    -R num  Use num as program revision\n"
				"    -S str  Use str as program segment\n"
				"    -V num  Use num as program visibility\n",
			argv[0]);
		return 0;
	}
	if (!prog) {
		if (ofname) {
			prog = trunc_pad(ofname, 6);
		} else {
			prog = trunc_pad(argv[optind], 6);
		}
	}
	objdump(argv[optind]);
	return 0;
}
