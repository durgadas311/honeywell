#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#include <string.h>
#include "a.out.h"

struct nnlist {
	struct nlist n;
	uint16_t link;
};
static struct exec hdr;
static struct nnlist *symtab = NULL;
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
static int Sflg = 0;
static int Nflg = 0;
static int base;
static int entry;

#define OP_A	0x00100	// May have A operand
#define OP_B	0x00200	// May have B operand
#define OP_C	0x00400	// May have C operand
#define OP_V	0x00800	// May have Variant
#define RQ_A	0x01000	// Requires A operand
#define RQ_B	0x02000	// Requires B operand
#define RQ_C	0x04000	// Requires C operand
#define RQ_V	0x08000	// Requires Variant
#define NO_WM	0x10000	// no WM required (2 * admode + 1)
#define B_BCT	0x00001	// Requires special handling

static int admode = 0;	// address mode for *current* item, if getreloc() used.

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
[022] = 	{ "sw",  OP_A | OP_B | NO_WM },
[020] = 	{ "si",  OP_A | OP_B | NO_WM },
[023] = 	{ "cw",  OP_A | OP_B },
[021] = 	{ "ci",  OP_A | OP_B },
[045] = 	{ "h",   OP_A | OP_B | OP_V },
[040] = 	{ "nop", OP_A | OP_B | OP_V },
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
	for (x = 0; x < admode; ++x) {
		v <<= 6;
		v |= b[s + x] & 077;
	}
	return v;
}

static int snoop_adm(uint8_t *r, int s) {
	int v = r[s];
	if (v & am_reltag(2)) {
		admode = 2;
	} else if (v & am_reltag(3)) {
		admode = 3;
	} else if (v & am_reltag(4)) {
		admode = 4;
	} else {
		// error: corrupt .reloc
	}
	return v;
}

static int get_reloc(uint8_t *r, int s) {
	int x = 0;
	int v = snoop_adm(r, s + x++);
	for (; x < admode; ++x) {
		v <<= 8;
		v |= r[s + x];
	}
	return v;
}

static int get_symbol(int a, int r) {
	if (!(r & ~am_relmsk(admode))) {
		return -1;
	}
	r &= am_relmsk(admode);
	int y, z;
	if (r & A_REXT) {
		y = A_RINDEX(r);
	} else if (r && by_val != END) {
		for (y = by_val; y != END; y = z) {
			z = symtab[y].link;
			if ((symtab[y].n.n_type & N_TYPE) != r) continue;
			if (symtab[y].n.n_pad1) {
				int b,e;
				if (symtab[y].n.n_pad1 < 0) {
					e = symtab[y].n.n_value;
					b = e + symtab[y].n.n_pad1 + 1;
				} else if (symtab[y].n.n_pad1 > 0) {
					b = symtab[y].n.n_value;
					e = b + symtab[y].n.n_pad1 - 1;
				} else {
					b = e = symtab[y].n.n_value;
				}
				if (a >= b && a <= e) {
					return y;
				}
				continue;
			}
			if (a < symtab[y].n.n_value) {
				continue;
			}
			if (a == symtab[y].n.n_value) {
				return y;
			}
			if (z != END && a < symtab[z].n.n_value &&
					(symtab[z].n.n_type & N_TYPE) == r) {
				return y; // 'y' is the symbol
			}
		}
		y = -1;
	} else {
		y = -1;
	}
	return y;
}

static int am_width() {
	switch(admode) {
	case 2: return 4;	// 12 bits in octal digits
	case 3: return 5;	// 15 bits in octal digits
	case 4: return 7;	// 19 bits in octal digits
	}
	return 0;
}

static uint32_t am_mask() {
	switch(admode) {
	case 2: return             0b111111111111;
	case 3: return       0b000111111111111111;
	case 4: return 0b000001111111111111111111;
	}
	return 0;
}

static uint32_t am_mod(int a) {
	switch(admode) {
	case 2: return 0;
	case 3: return (a & ~0b000111111111111111) >> 15;
	case 4: return (a & ~0b000001111111111111111111) >> 19;
	}
	return 0;
}

static int am_indir(int m) {
	switch(admode) {
	case 2: return 0;
	case 3: return (m == 0b00111);
	case 4: return (m == 0b10000);
	}
	return 0;
}

static int am_signx(int a) {
	switch(admode) {
	case 2: break;	// never called
	case 3: if (a & 00040000) return (a | ~00077777);
	case 4: if (a & 01000000) return (a | ~01777777);
	}
	return a;
}

// Assume symtab already loaded
static void print_addr(uint8_t *b, uint8_t *r, int s, int t) {
	int a;
	int y = -1;
	if (r) {
		// must do this first, to get admode set
		y = get_reloc(r, s);
	}
	a = get_addr(b, s);
	int m = am_mod(a);
	a &= am_mask();
	if (r) {
		y = get_symbol(a, y);
	}
	int c;
	if (!m) {
		printf("%c%0*o", t, am_width(), a);
	} else if (am_indir(m)) {
		printf("%c(%0*o)", t, am_width(), a);
	} else {
		if (m > 15) {
			c = 'y';
		} else {
			c = 'x';
		}
		a = am_signx(a);
		printf("%c%d(%c%d)", t, a, c, m & 0b01111);
	}
	if (r && y >= 0) {
		int o = a - symtab[y].n.n_value;
		if (o) {
			printf(" <%.8s+0x%x>", symtab[y].n.n_name, o);
		} else {
			printf(" <%.8s>", symtab[y].n.n_name);
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
	nsyms = hdr.a_syms / sizeof(struct nlist);
	symtab = malloc(nsyms * sizeof(struct nnlist));
	if (symtab == NULL) {
		return -1;
	}
	o = sizeof(hdr);
	o += (hdr.a_text + hdr.a_data);
	if (!(hdr.a_flag & A_NRELFLG)) {
		o += (hdr.a_text + hdr.a_data);
	}
	fseek(fp, o, SEEK_SET);
	// Do not want file symbols here
	for (x = 0; x < nsyms; ++x) {
		if (fread(&symtab[x].n, sizeof(struct nlist), 1, fp) != 1) {
			free(symtab);
			symtab = NULL;
			return -1;
		}
		if ((symtab[x].n.n_type & N_TYPE) == N_FN) {
			continue;
		}
		if (by_val == END ||
				symtab[by_val].n.n_value > symtab[x].n.n_value) {
			symtab[x].link = by_val;
			by_val = x;
			continue;
		}
		for (y = by_val; ; y = symtab[y].link) {
			z = symtab[y].link;
			if (z == END || symtab[z].n.n_value > symtab[x].n.n_value) {
				symtab[x].link = z;
				symtab[y].link = x;
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
		if ((symtab[x].n.n_type & N_TYPE) == N_FN) {
			a = symtab[x].n.n_value;
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

static void do_swsi(uint8_t *buf, int len, char *symb, char *op, int pu) {
	int y = 0;
	int z = -1;
	int n = len;
	int x;
	if (pu == 0100) { // for WM case only
		++n;	// place WM after last char
	}
	for (x = 0; x < n; ++x) {
		if (x < len && (buf[x] & pu) == 0) {
			continue;
		}
		if (!y) {
			printf("\t%s\t%s+%d,", op, symb, x);
			z = x;
			y = 1;
		} else {
			printf("%s+%d\n", symb, x);
			y = 0;
		}
	}
	if (y) {
		printf("%s+%d\n", symb, z);
	}
}

static char *swsi(FILE *fp) {
	uint8_t *buf;
	int len = hdr.a_text + hdr.a_data;
	int x;
	char symb[8];
	char symb2[8];

	get_symtab(fp);
	// TODO: get reference symbol for start of .text (or .data if no .text)
	strncpy(symb, "base", 8);
	strncpy(symb2, "entry", 8);
	if (Nflg) {
		printf("%s\t=\t0%o\n", symb, base);
		printf("%s\t=\t0%o\n", symb2, entry);
	} else {
		for (x = 0; x < nsyms; ++x) {
			if ((symtab[x].n.n_type & N_EXT) == 0 ||
				symtab[x].n.n_value != 0) continue;
			int t = symtab[x].n.n_type & N_TYPE;
			if (t == N_TEXT || t == N_DATA) {
				strncpy(symb, symtab[x].n.n_name, 8);
				strncpy(symb2, symtab[x].n.n_name, 8);
				printf("\t.globl\t%.8s\n", symb);
				break;
			}
		}
	}
	// TODO: auto-detect address mode...
	if (!admode) admode = 3;
	printf("\t.admode\t%d\n", admode);
	// TODO: handle complex linkage?
	buf = malloc(len);
	if (buf == NULL) {
		return "out of memory";
	}
	fseek(fp, (off_t)sizeof(hdr), SEEK_SET);
	if (fread(buf, len, 1, fp) != 1) {
		return "corrupt file";
	}
	do_swsi(buf, len, symb, "sw", 0100);
	do_swsi(buf, len, symb, "si", 0200);
	if (Nflg) {
		x = 1 + 2 * admode; // length of SW instruction
		printf("\tsw\t.+%d,.+%d\n", x, x + 1 + admode);
		printf("\tb\t%s\n", symb2);
	}
	return NULL;
}

static char *disas(FILE *fp) {
	uint8_t *buf;
	uint8_t *rel = NULL;
	int x, y, f;
	int t, u, w, z;
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
	// always get reloc data, if available.
	// either way, must have admode.
	if (!(hdr.a_flag & A_NRELFLG)) {
		rel = malloc(hdr.a_text);
		if (rel == NULL) {
			return "out of memory";
		}
		fseek(fp, (off_t)(sizeof(hdr) + hdr.a_text + hdr.a_data), SEEK_SET);
		if (fread(rel, hdr.a_text, 1, fp) != 1) {
			return "corrupt file";
		}
	} else if (admode == 0) {
		return "No relocation data and no address mode hint";
	}
	// TODO: -s: read symbols...
	// TODO: need to scan ahead to first reloc, to get initial admode...
	for (x = 0; x < hdr.a_text; x = y) {
		// if no WM, warning?
		op = &dis[buf[x] & 077];
		f = op->flg;
		if (rel) {
			// last-ditch effort to get the admode
			if (f & OP_A) {
				(void)snoop_adm(rel, x + 1);
			}
		}
		if (f & NO_WM) {
			w = 2 * admode + 1;	// max for SW/SI
		} else {
			w = 3 * admode + 1;	// enough for PCB?
		}
		for (y = x + 1; y < hdr.a_text && !(buf[y] & 0100); ++y) {
			if (y - x >= w) break;
		}
		int i = get_symbol(x + ba, am_reltag32(admode) | A_RTEXT);
#if 0	// TODO: find a way to distinguish start of function
//if (i >= 0) fprintf(stderr, "got %.8s\n", symtab[i].n.n_name);
		if (i >= 0 && (symtab[i].n.n_type & N_EXT)) {
			// TODO: recognize start of functions...
			if (x + ba == symtab[i].n.n_value) {
				printf("%07o <%.8s>:\n",
						x + ba, symtab[i].n.n_name);
				i = -1;
			}
		}
#endif
		printf("%7o: ", x + ba);
		w = 2 * admode + 4;	// covers all cases?
		z = ((9 + (w * 3) + 7) & ~7) - 9;
		w = z / 3;
		u = z % 3;
		for (z = 0; z < w; ++z) {
			if (z >= y - x) break;
			printf(" %02o", buf[x + z] & 077);
		}
		while (z < w) {
			printf("   ");
			++z;
		}
		printf("%.*s", u, "    ");
		if (i >= 0) {
			if (x + ba == symtab[i].n.n_value) {
				printf("%.8s:", symtab[i].n.n_name);
			} else if (y + ba - 1 == symtab[i].n.n_value) {
				printf("%.8s::", symtab[i].n.n_name);
			}
		}
		if (!op->mn) {
			printf("\t?");
			//odump(buf, x, y, '\t');
			goto next;
		}
		if (f & B_BCT) {
			if (y - x - 1 != admode) {
				printf("\tbct");
			} else {
				printf("\tb");
			}
		} else {
			printf("\t%s", op->mn);
		}
		t = '\t';
		++x;
		if ((f & RQ_A) || ((f & OP_A) && y - x >= admode)) {
			print_addr(buf, rel, x, t);
			t = ',';
			x += admode;
		}
		if ((f & RQ_B) || ((f & OP_B) && y - x >= admode)) {
			print_addr(buf, rel, x, t);
			t = ',';
			x += admode;
		}
		if ((f & RQ_C) || ((f & OP_C) && y - x >= admode)) {
			print_addr(buf, rel, x, t);
			t = ',';
			x += admode;
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
	printf(		"Idx  Name   Size(oct) VMA(oct)  LMA(oct)   File off\n");
	if (hdr.a_text) {
		printf(	"  %d  .text  %07o   %07o   %07o    %08x\n",
				idx++, hdr.a_text, base, 0, off);
		off += hdr.a_text;
		base += hdr.a_text;
	}
	if (hdr.a_data) {
		printf(	"  %d  .data  %07o   %07o   %07o    %08x\n",
				idx++, hdr.a_data, base, 0, off);
		off += hdr.a_data;
		base += hdr.a_data;
	}
	if (hdr.a_bss) {
		printf(	"  %d  .bss   %07o   %07o   %07o    %08x\n",
				idx++, hdr.a_bss, base, 0, off);
		base += hdr.a_bss;
		// no 'off' change - .bss not in file
	}
	if (hdr.a_heap) {
		printf(	"  %d  (heap) %07o   %07o   %07o    %08x\n",
				idx++, hdr.a_heap, base, 0, off);
		base += hdr.a_heap;
		// no 'off' change - heap not in file
	}
	if (!(hdr.a_flag & A_NRELFLG)) {
		printf(	"  %d  .reloc %07o   %07o   %07o    %08x\n",
				idx++, hdr.a_text + hdr.a_data, 0, 0, off);
		off += hdr.a_text + hdr.a_data;
	}
	if (hdr.a_syms) {
		printf(	"  %d  .syms  %07o   %07o   %07o    %08x\n",
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
	if (Sflg) {
		err = swsi(fp);
	} else if (dflg) {
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
	char *e;
	while ((x = getopt(argc, argv, "a:dhHj:N:rsSw:")) != EOF) {
		switch(x) {
		case 'a':
			admode = strtoul(optarg, NULL, 0);
			if (admode < 2 || admode > 4) {
				fprintf(stderr, "Invalid address mode \"%s\"\n", optarg);
				exit(1);
			}
			break;
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
		case 'S':
			++Sflg;
			break;
		case 'N':
			++Nflg;
			entry = base = strtoul(optarg, &e, 0);
			if (*e == ',') {
				entry = strtoul(e + 1, &e, 0);
			}
			if (*e) {
				fprintf(stderr, "Invalid -N: \"%s\"\n", optarg);
				goto usage;
			}
			break;
		case 'w':
			wflg = strtoul(optarg, NULL, 0);
			break;
		}
	}
	if (optind >= argc) {
usage:
		fprintf(stderr,	"Usage: %s [options] <obj-file>[...]\n"
				"Options:\n"
				"    -a adm  Adr mode hint\n"
				"    -d      Disassemble .text\n"
				"    -h      Display header info\n"
				"    -r      Show reloc info for -d\n"
				"    -s      Dump section(s)\n"
				"    -j sect Section (.text or .data) for -s\n"
				"    -H      Use Honeywell style dump for -j\n"
				"    -w wid  Ouput width for -s (hint only)\n"
				"    -S      Generate SW/SI preamble\n"
				"    -N adr[,adr]  Standalone SW/SI base adr, for -S\n",
			argv[0]);
		return 0;
	}
	for (x = optind; x < argc; ++x) {
		objdump(argv[x]);
	}
	return 0;
}
