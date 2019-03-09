#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#include "a.out.h"

static struct exec hdr;
static struct nlist *symtab = NULL;
static int nsyms = 0;
#define END ((uint16_t)-1)
static uint16_t by_val = END;

static int dflg = 0;
static int rflg = 0;	// sub-flag to -d
static int sflg = 0;
static char *jflg;
static int wflg = 80;
static int Hflg = 0;
static int hflg = 0;

#define OP_A	0x00100	// May have A operand
#define OP_B	0x00200	// May have B operand
#define OP_C	0x00400	// May have C operand
#define OP_V	0x00800	// May have Variant
#define RQ_A	0x01000	// Requires A operand
#define RQ_B	0x02000	// Requires B operand
#define RQ_C	0x04000	// Requires C operand
#define RQ_V	0x08000	// Requires Variant
#define B_BCT	0x00001	// Requires special handling

static char ascii[64] = {
//       00   01   02   03   04   05   06   07   10   11   12   13   14   15   16   17
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9','\'', '=', ':', ' ', '>', '&',
	'+', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', ';', '.', ')', '%', ']', '?',
	'-', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', '#', '$', '*', '"','\\', '!',
	'<', '/', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '@', ',', '(', '~', '[', '^',
};

struct instr {
	char *mn;
	int flg;
};
static struct instr dis[64] = {
[036] = 	{ "a",   OP_A | OP_B },
[037] = 	{ "s",   OP_A | OP_B },
[034] = 	{ "ba",  OP_A | OP_B },
[035] = 	{ "bs",  OP_A | OP_B },
[016] = 	{ "za",  OP_A | OP_B },
[017] = 	{ "zs",  OP_A | OP_B },
[026] = 	{ "m",   OP_A | OP_B },
[027] = 	{ "d",   OP_A | OP_B },
[031] = 	{ "ext", OP_A | OP_B },
[030] = 	{ "ha",  OP_A | OP_B },
[032] = 	{ "sst", OP_A | OP_B | OP_V },
[033] = 	{ "c",   OP_A | OP_B },
[065] = 	{ "b",   OP_A | B_BCT },	// need special handling...
[054] = 	{ "bcc", OP_A | OP_B | OP_V },
[055] = 	{ "bce", OP_A | OP_B | OP_V },
[056] = 	{ "bbe", OP_A | OP_B | OP_V },
[022] = 	{ "sw",  OP_A | OP_B },
[020] = 	{ "si",  OP_A | OP_B },
[023] = 	{ "cw",  OP_A | OP_B },
[021] = 	{ "ci",  OP_A | OP_B },
[045] = 	{ "h",   OP_A | OP_B | OP_V },
[040] = 	{ "nop", 0 },
[014] = 	{ "mcw", OP_A | OP_B },
[015] = 	{ "lca", OP_A | OP_B },
[024] = 	{ "scr", OP_A | OP_V },
[025] = 	{ "lcr", OP_A | OP_V },
[042] = 	{ "cam", OP_V },
[043] = 	{ "csm", OP_A | OP_B },
[010] = 	{ "exm", OP_A | OP_B },
[060] = 	{ "mat", OP_A | OP_B | OP_C | RQ_A | RQ_B | RQ_C },
[062] = 	{ "mit", OP_A | OP_B | OP_C | OP_V | RQ_A | RQ_B | RQ_C | RQ_V },
[077] = 	{ "lib", OP_A | OP_B | RQ_A },
[076] = 	{ "sib", OP_A | OP_B | RQ_A },
[057] = 	{ "tlu", OP_A | OP_B | OP_V },
[013] = 	{ "mos", OP_A | OP_B | OP_V },
[046] = 	{ "svi", OP_V | RQ_V },
[067] = 	{ "rvi", OP_A | RQ_A | OP_V | RQ_V },
[044] = 	{ "mc",  0 },
[041] = 	{ "rnm", OP_A | OP_B },
[074] = 	{ "mce", OP_A | OP_B },
[066] = 	{ "pdt", OP_A | RQ_A | OP_V | RQ_V },
[064] = 	{ "pcb", OP_A | RQ_A | OP_V | RQ_V },
//[000] = 	{ "iic", 0 },	// TBD
[007] = 	{ "fma", OP_A | RQ_A | OP_V | RQ_V },
[006] = 	{ "faa", OP_V | RQ_V },
[004] = 	{ "bms", OP_V | RQ_V },
[005] = 	{ "bim", OP_A | OP_B | RQ_A | RQ_B },
};

static char *hhdr = "%07o 1       2       3       4       5       6       7";

static void hdump(uint8_t *b, int s, int e, int ba) {
	int x;
	int m;
	int y, z;
	x = (s + ba);
	if (wflg < 128) {
		x &= ~077;
		m = 64;
	} else {
		x &= ~0177;
		m = 128;
	}
	x -= (s + ba);
	while (x < e) { 
		printf(hhdr, ba + x);
		if (wflg >= 128) {
			printf("       ");
			printf(hhdr, ba + x + 64);
		}
		printf("\n");
		z = x + m;
		for (y = x; y < z; ++y) {
			if (y < s || y >= e) printf(" ");
			else printf("%c", ascii[b[y] & 077]);
		}
		printf("\n");
		for (y = x; y < z; ++y) {
			if (y < s || y >= e) printf(" ");
			else printf("%d", (b[y] >> 3) & 07);
		}
		printf("\n");
		for (y = x; y < z; ++y) {
			if (y < s || y >= e) printf(" ");
			else printf("%d", b[y] & 07);
		}
		printf("\n");
		for (y = x; y < z; ++y) {
			if (y < s || y >= e) printf(" ");
			else printf("%c", " WIR"[(b[y] >> 6) & 03]);
		}
		printf("\n");
		x = z;
	}
}

static void sdump(uint8_t *b, int s, int e, int ba) {
	int x = s & ~0x0f;
	int y, z;
	int m;
	x = (s + ba);
	if (wflg < 90) {
		x &= ~007;
		m = 8;
	} else {
		x &= ~017;
		m = 16;
	}
	x &= ~017;
	x -= (s + ba);
	while (x < e) {
		printf("%07o:", ba + x);
		z = x + m;
		for (y = x; y < z; ++y) {
			if (y < s || y >= e) printf("    ");
			else printf(" %03o", b[y]);
		}
		printf("  ");
		for (y = x; y < z; ++y) {
			if (y < s || y >= e) printf(" ");
			else printf("%c", ascii[b[y] & 077]);
		}
		printf("\n");
		x = z;
	}
}

static void odump(uint8_t *b, int s, int e, int t) {
	while (s < e) {
		printf("%c%03o", t, b[s++]);
		t = ',';
	}
}

static int get_addr(uint8_t *b, int s) {
	int x;
	int v = 0;
	for (x = 0; x < 4; ++x) {
		v <<= 6;
		v |= b[s + x] & 077;
	}
	return v;
}

static int get_reloc(uint8_t *r, int s) {
	int x;
	int v = 0;
	for (x = 0; x < 4; ++x) {
		v <<= 8;
		v |= r[s + x];
	}
	return v;
}

static int get_symbol(int a, int y) {
	if (!(y & 0xe0000000)) {
		return -1;
	}
	y &= 0x1fffffff;
	int z;
	if (y & A_REXT) {
		y = A_RINDEX(y);
	} else if (by_val != END) {
		for (y = by_val; symtab[y].n_pad1 != END; y = z) {
			z = symtab[y].n_pad1;
			if (a < symtab[y].n_value) {
				continue;
			}
			if (a < symtab[z].n_value) {
				break; // 'y' is the symbol
			}
		}
	} else {
		y = -1;
	}
	return y;
}

// Assume symtab already loaded
static void print_addr(uint8_t *b, uint8_t *r, int s, int t) {
	int a = get_addr(b, s);
	int y = -1;
	// TODO: addr mode
	int m = a >> 19;
	a &= ((1 << 19) - 1);
	if (r) {
		y = get_reloc(r, s);
		y = get_symbol(a, y);
	}
	int c;
	if (!m) {
		printf("%c%07o", t, a);
	} else if (m == 0b10000) {
		printf("%c(%07o)", t, a);
	} else {
		if (m > 15) {
			c = 'y';
		} else {
			c = 'x';
		}
		printf("%c%d(%c%d)", t, a, c, m & 0b01111);
	}
	if (y >= 0) {
		int o = a - symtab[y].n_value;
		if (o) {
			printf("<%s+0x%x>", symtab[y].n_name, o);
		} else {
			printf("<%s>", symtab[y].n_name);
		}
	}
}

static int get_symtab(FILE *fp) {
	off_t o;
	int x, y;
	uint16_t z;
	if (symtab) {
		return 0;
	}
	symtab = malloc(hdr.a_syms);
	if (symtab == NULL) {
		return -1;
	}
	o = sizeof(hdr);
	o += (hdr.a_text + hdr.a_data);
	if (!(hdr.a_flag & A_NRELFLG)) {
		o += (hdr.a_text + hdr.a_data);
	}
	fseek(fp, o, SEEK_SET);
	if (fread(symtab, hdr.a_syms, 1, fp) != 1) {
		free(symtab);
		symtab = NULL;
		return -1;
	}
	nsyms = hdr.a_syms / sizeof(*symtab);
	// Do not want file symbols here
	for (x = 0; x < nsyms; ++x) {
		if ((symtab[x].n_type & N_TYPE) == N_FN) {
			continue;
		}
		if (by_val == END ||
				symtab[by_val].n_value > symtab[x].n_value) {
			symtab[x].n_pad1 = by_val;
			by_val = x;
			continue;
		}
		for (y = by_val; ; y = symtab[y].n_pad1) {
			z = symtab[y].n_pad1;
			if (z == END || symtab[z].n_value > symtab[x].n_value) {
				symtab[x].n_pad1 = z;
				symtab[y].n_pad1 = x;
				break;
			}
		}
	}
	return 0;
}

static void dis_symtab() {
	if (symtab) {
		free(symtab);
		symtab = NULL;
		nsyms = 0;
		by_val = -1;
	}
}

// Extract "value" (address) of first file symbol. Or "0".
static int get_file_addr(FILE *fp) {
	int a = 0;
	int x;
	get_symtab(fp);
	// Don't need to do all this... just get entry addr from header...
	// But we're still expected to load symtab.
#if 0
	for (x = 0; x < nsyms; ++x) {
		if ((symtab[x].n_type & N_TYPE) == N_FN) {
			a = symtab[x].n_value;
			break;
		}
	}
#else
	a = hdr.a_entry;
#endif
	return a;
}

static char *sect(FILE *fp, char *sec) {
	uint8_t *buf;
	int s = sizeof(hdr) + hdr.a_text;
	int e = hdr.a_data;
	if (sec == NULL) {
		printf("Dump of .text:\n");
		sect(fp, ".text");
		printf("Dump of .data:\n");
		sect(fp, ".data");
		return NULL;
	}
	int ba = get_file_addr(fp);
	if (ba < 0) {
		return "corrupt file";
	}
	if (strcasecmp(".text", sec) == 0) {
		s = sizeof(hdr);
		e = hdr.a_text;
	} else if (strcasecmp(".data", sec) == 0) {
		// defaults
		ba += hdr.a_text;
	} else {
		return "unknown section name";
	}
	buf = malloc(e);
	if (buf == NULL) {
		return "out of memory";
	}
	fseek(fp, (off_t)s, SEEK_SET);
	if (fread(buf, e, 1, fp) != 1) {
		return "corrupt file";
	}
	if (Hflg) {
		hdump(buf, 0, e, ba);
	} else {
		sdump(buf, 0, e, ba);
	}
	free(buf);
	return NULL;
}

// TODO: determine admode...
static char *disas(FILE *fp) {
	uint8_t *buf;
	uint8_t *rel = NULL;
	int x, y, f;
	int t;
	struct instr *op;
	int ba = get_file_addr(fp);
	if (ba < 0) {
		return "corrupt file";
	}
	buf = malloc(hdr.a_text);
	if (buf == NULL) {
		return "out of memory";
	}
	fseek(fp, (off_t)sizeof(hdr), SEEK_SET);
	if (fread(buf, hdr.a_text, 1, fp) != 1) {
		return "corrupt file";
	}
	if (rflg && !(hdr.a_flag & A_NRELFLG)) {
		rel = malloc(hdr.a_text);
		if (rel == NULL) {
			return "out of memory";
		}
		fseek(fp, (off_t)(sizeof(hdr) + hdr.a_text + hdr.a_data), SEEK_SET);
		if (fread(rel, hdr.a_text, 1, fp) != 1) {
			return "corrupt file";
		}
	}
	// TODO: -s: read symbols...
	for (x = 0; x < hdr.a_text; x = y) {
		// if no WM, warning?
		for (y = x + 1; y < hdr.a_text && !(buf[y] & 0100); ++y);
		op = &dis[buf[x] & 077];
		if (!op->mn) {
			odump(buf, x, y, '\t');
			goto next;
		}
		f = op->flg;
		if (rflg) {
			int i = get_symbol(x + ba, 0xf0000000);
			if (i >= 0) {
				if (x + ba - symtab[i].n_value == 0) {
					printf("%07o <%s>:\n", x + ba, symtab[i].n_name);
				}
			}
		}
		if (f & B_BCT) {
			if (y - x - 1 != 4) {
				printf("%07o:\tbct", x + ba);
			} else {
				printf("%07o:\tb", x + ba);
			}
		} else {
			printf("%07o:\t%s", x + ba, op->mn);
		}
		t = '\t';
		++x;
		if ((f & RQ_A) || ((f & OP_A) && y - x >= 4)) {
			print_addr(buf, rel, x, t);
			t = ',';
			x += 4;
		}
		if ((f & RQ_B) || ((f & OP_B) && y - x >= 4)) {
			print_addr(buf, rel, x, t);
			t = ',';
			x += 4;
		}
		if ((f & RQ_C) || ((f & OP_C) && y - x >= 4)) {
			print_addr(buf, rel, x, t);
			t = ',';
			x += 4;
		}
		if (x < y) {
			odump(buf, x, y, t);
		}
next:
		printf("\n");
	}
	free(buf);
	return NULL;
}

static char *fhdr(FILE *fp) {
	int idx = 0;
	int off = sizeof(hdr);
	int base = get_file_addr(fp);
	if (base < 0) {
		return "corrupt object file";
	}
	printf(		"Idx  Name   Size       VMA(oct)  LMA(oct)   File off\n");
	if (hdr.a_text) {
		printf(	"  %d  .text  %08x   %07o   %07o    %08x\n",
				idx++, hdr.a_text, base, 0, off);
		off += hdr.a_text;
		base += hdr.a_text;
	}
	if (hdr.a_data) {
		printf(	"  %d  .data  %08x   %07o   %07o    %08x\n",
				idx++, hdr.a_data, base, 0, off);
		off += hdr.a_data;
		base += hdr.a_data;
	}
	if (hdr.a_bss) {
		printf(	"  %d  .bss   %08x   %07o   %07o    %08x\n",
				idx++, hdr.a_bss, base, 0, off);
		// no 'off' change - .bss not in file
	}
	if (hdr.a_heap) {
		printf(	"  %d  (heap) %08x   %07o   %07o    %08x\n",
				idx++, hdr.a_heap, base, 0, off);
		// no 'off' change - heap not in file
	}
	if (!(hdr.a_flag & A_NRELFLG)) {
		printf(	"  %d  .reloc %08x   %07o   %07o    %08x\n",
				idx++, hdr.a_text + hdr.a_data, 0, 0, off);
		off += hdr.a_text + hdr.a_data;
	}
	if (hdr.a_syms) {
		printf(	"  %d  .syms  %08x   %07o   %07o    %08x\n",
				idx++, hdr.a_syms, 0, 0, off);
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
	if (dflg) {
		err = disas(fp);
//	} else if (rflg) {	// -r alone...
//		err = reloc(fp);
	} else if (sflg) {
		err = sect(fp, jflg);
	} else if (hflg) {
		err = fhdr(fp);
	}
	if (err) {
		fprintf(stderr, "%s: %s\n", f, err);
	}
	fclose(fp);
	dis_symtab();
}

int main(int argc, char **argv) {
	extern int optind;
	extern char *optarg;
	int x;
	while ((x = getopt(argc, argv, "dhHj:rsw:")) != EOF) {
		switch(x) {
		case 'd':
			++dflg;
			break;
		case 'h':
			++hflg;
			break;
		case 'H':
			++Hflg;
			break;
		case 'j':
			jflg = optarg;
			break;
		case 'r':
			++rflg;
			break;
		case 's':
			++sflg;
			break;
		case 'w':
			wflg = strtoul(optarg, NULL, 0);
			break;
		}
	}
	if (optind >= argc) {
		fprintf(stderr,	"Usage: %s [options] <obj-file>[...]\n"
				"Options:\n"
				"    -d      Disassemble .text\n"
				"    -r      Show reloc info for -d\n"
				"    -s      Dump section(s)\n"
				"    -j sect Section (.text or .data) for -s\n"
				"    -H      Use Honeywell style dump for -j\n"
				"    -w wid  Ouput width for -j (hint only)\n",
			argv[0]);
		return 0;
	}
	for (x = optind; x < argc; ++x) {
		objdump(argv[x]);
	}
	return 0;
}
