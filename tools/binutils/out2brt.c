// Copyright (c) 2019 Douglas Miller <durgadas311@gmail.com>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

extern char hw200[];

static int dist = -1;
static int reclen;
static int reccnt;
static uint8_t *record;
static int dirty = 0;
static int seq;
static long vis;
static int rev;

static int setup_brt(long vi, int rv, int rl);
static int fin_rec(int last);
static void put_len(int len);
static void put_seq(int seq);
static void init_rec();
static void put_str(char *str);
static int init_seg(char *prg, char *seg, long vis);
static int begin(int adr, char *prg, char *seg);
static int segment(char *prg, char *seg);
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

int write_rec(uint8_t *rec, int len);
int end_seg();
int begin_seg(char *rev, char *prg, char *seg, long vis);

static int setup_brt(long vi, int rv, int rl) {
	reclen = rl;
	reccnt = 0;
	record = malloc(reclen + 6);
	if (!record) {
		return -1;
	}
	vis = vi;
	rev = rv % 1000;
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

static int init_seg(char *prg, char *seg, long vis) {
	char rv[8];
	sprintf(rv, "%03d", rev);
	if (!begin_seg(rv, prg, seg, vis)) {
		return 0;
	}
	record[0] = (uint8_t)050; // modified to 054 at end if last
	put_len(0);	// updated later...
	put_seq(seq);	//
	reccnt = 7;
	put_str(rv);
	put_str(prg);
	put_str(seg);
	put_adr((int)(vis >> 18));
	put_adr((int)(vis));
	// assert reccnt == 24...
	reccnt = 24;
	record[6] = (uint8_t)reccnt;
	seq = 1;
	return 1;
}

static int begin(int adr, char *prg, char *seg) {
	if (!init_seg(prg, seg, vis)) {
		return 0;
	}
	// set_adr(adr); // let first set_code do this...
	dist = -1;
	dirty = 0;
	return 1;
}

static int segment(char *prg, char *seg) {
	if (dirty) {
		fprintf(stderr, "WARNING: SEG after code\n");
	}
	// only 'seg' should be different...
	return init_seg(prg, seg, vis);
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
	if (len > 1) {
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

int main(int argc, char **argv) {
	// Options: prog name, ...
	// Add BRT init of 'x1' at end of .bss
	// OS sets up prog name (e.g. MOD1), crt0 must reference in argv.
	return 0;
}
