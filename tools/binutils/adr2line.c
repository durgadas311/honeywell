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
static uint16_t by_file = END;
static uint16_t by_dbg = END;

static int aflag = 0;
static int fflag = 0;

static int get_file(int a) {
	int y = -1;
	int z;
	if (by_file != END) {
		if (a < symtab[by_file].n_value) {
			return -1;
		}
		for (y = by_file; symtab[y].n_pad1 != END; y = z) {
			z = symtab[y].n_pad1;
			if (a < symtab[y].n_value) {
				continue;
			}
			if (a < symtab[z].n_value) {
				break; // 'y' is the symbol
			}
		}
	}
	return y;
}

static int get_symbol(int a) {
	int y = -1;
	int z;
	if (by_val != END) {
		if (a < symtab[by_val].n_value) {
			return -1;
		}
		for (y = by_val; symtab[y].n_pad1 != END; y = z) {
			z = symtab[y].n_pad1;
			if (a < symtab[y].n_value) {
				continue;
			}
			if (a < symtab[z].n_value) {
				break; // 'y' is the symbol
			}
		}
	}
	return y;
}

static int get_debug(int a) {
	int y = -1;
	int z;
	if (by_dbg != END) {
		if (a < symtab[by_dbg].n_value) {
			return -1;
		}
		for (y = by_dbg; symtab[y].n_pad1 != END; y = z) {
			z = symtab[y].n_pad1;
			if (a < symtab[y].n_value) {
				continue;
			}
			if (a < symtab[z].n_value) {
				break; // 'y' is the symbol
			}
		}
	}
	return y;
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
	// Do want file symbols here, but separate
	// Also, only want function names... exclude labels...
	for (x = 0; x < nsyms; ++x) {
		if ((symtab[x].n_type & N_TYPE) == N_FN ||
				symtab[x].n_name[0] == '~' ||
				symtab[x].n_name[0] == 'L') {
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
	for (x = 0; x < nsyms; ++x) {
		if ((symtab[x].n_type & N_TYPE) != N_FN) {
			continue;
		}
		if (by_file == END ||
				symtab[by_file].n_value > symtab[x].n_value) {
			symtab[x].n_pad1 = by_file;
			by_file = x;
			continue;
		}
		for (y = by_file; ; y = symtab[y].n_pad1) {
			z = symtab[y].n_pad1;
			if (z == END || symtab[z].n_value > symtab[x].n_value) {
				symtab[x].n_pad1 = z;
				symtab[y].n_pad1 = x;
				break;
			}
		}
	}
	for (x = 0; x < nsyms; ++x) {
		if (symtab[x].n_name[0] != '~' || symtab[x].n_name[1] != '~') {
			continue;
		}
		if (by_dbg == END ||
				symtab[by_dbg].n_value > symtab[x].n_value) {
			symtab[x].n_pad1 = by_dbg;
			by_dbg = x;
			continue;
		}
		for (y = by_dbg; ; y = symtab[y].n_pad1) {
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
		by_file = -1;
	}
}

static void addr2line(int adr) {
	int min, max;
	min = hdr.a_entry;
	max = min + hdr.a_text;
	if (aflag) printf("%07o\n", adr);
	int f = get_file(adr);
	if (f < 0) {
		//printf("?");
	} else {
		min = symtab[f].n_value;
		if (symtab[f].n_pad1 != END) {
			max = symtab[symtab[f].n_pad1].n_value;
		}
		//int o = adr - symtab[f].n_value;
		//printf("%s", symtab[f].n_name);
		//if (o != 0) {
		//	printf("+0%o", o);
		//}
	}
	if (fflag) {
		int s = get_symbol(adr);
		if (s < 0 || symtab[s].n_value < min || symtab[s].n_value >= max) {
			printf("??\n");
		} else {
			printf("%s\n", symtab[s].n_name);
			//int o = adr - symtab[s].n_value;
			//if (o != 0) {
			//	printf("+0%o", o);
			//}
		}
	}
	if (f < 0) {
		printf("??");
	} else {
		printf("%s", symtab[f].n_name);
	}
	int d = get_debug(adr);
	if (d < 0 || symtab[d].n_value < min || symtab[d].n_value >= max) {
		printf(":0\n");
	} else {
		printf(":%s\n", symtab[d].n_name + 2);
	}
}

int main(int argc, char **argv) {
	extern int optind;
	extern char *optarg;
	char *file = NULL;
	FILE *fp;
	int x;
	int adr;
	while ((x = getopt(argc, argv, "ae:f")) != EOF) {
		switch(x) {
		case 'a':
			++aflag;
			break;
		case 'e':
			file = optarg;
			break;
		case 'f':
			++fflag;
			break;
		}
	}
	if (file == NULL) {
		file = "a.out";
	}
	if (optind >= argc) {
		fprintf(stderr,	"Usage: %s [-e <obj-file>] <addr>...\n",
			argv[0]);
		return 0;
	}
	fp = fopen(file, "r");
	if (fp == NULL) {
		perror(file);
		return 1;
	}
	if (fread(&hdr, sizeof(hdr), 1, fp) != 1 ||
			N_BADMAG(hdr)) {
		fprintf(stderr, "%s: Not an object file\n", file);
		fclose(fp);
		return 1;
	}
	get_symtab(fp);
	for (x = optind; x < argc; ++x) {
		adr = strtoul(argv[x], NULL, 0);
		// TODO: prune bad addresses here?
		addr2line(adr);
	}
	fclose(fp);
	return 0;
}
