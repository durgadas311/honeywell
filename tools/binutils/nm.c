/*
 * print symbol tables for object files
 *
 * This file is part of BKUNIX project, which is distributed
 * under the terms of the GNU General Public License (GPL).
 * See the accompanying file "COPYING" for more details.
 */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include "a.out.h"

int cflg = 0;
int nflg = 0;
int uflg = 0;
int rflg = 1;
int gflg = 0;
int pflg = 0;
int fflg = 0;

/*
 * Read a.out header. Return 0 on error.
 */
int
readhdr(int fd, struct exec *hdr) {
	if (read(fd, hdr, sizeof(*hdr)) != sizeof(*hdr)) {
		return 0;
	}
	return 1;
}

/*
 * Read a.out symbol. Return 0 on error.
 */
int
readsym(int fd, struct nlist *sym) {
	if (read(fd, sym, sizeof(*sym)) != sizeof(*sym)) {
		return 0;
	}
	return 1;
}

int
compare(const void *v1, const void *v2) {
	const struct nlist *p1 = v1;
	const struct nlist *p2 = v2;
	if (nflg) {
		if (p1->n_value > p2->n_value) {
			return rflg;
		}
		if (p1->n_value < p2->n_value) {
			return -rflg;
		}
	}
	return rflg * strncmp(p1->n_name, p2->n_name, sizeof(p1->n_name));
}

int
names(char *filename, int nameflg) {
	struct exec hdr;
	struct nlist *nlp;
	long off;
	int fi, n, i, j;

	fi = open(filename, O_RDONLY);
	if (fi < 0) {
		perror(filename);
		return 1;
	}
	if (!readhdr(fi, &hdr) || N_BADMAG(hdr)) {
		printf("%s: bad format\n", filename);
		return 1;
	}
	n = hdr.a_syms / sizeof(struct nlist);
	if (n == 0) {
		printf("%s: no name list\n", filename);
		return 1;
	}
	nlp = (struct nlist *)malloc(hdr.a_syms);
	if (!nlp) {
		printf("%s: out of memory\n", filename);
		return 1;
	}
	off = (N_SYMOFF(hdr));
	if (lseek(fi, off, 0) < 0) {
badsym:		printf("%s: cannot read symbol table\n", filename);
		return 1;
	}
	for (i = 0; i < n; i++) {
		if (!readsym(fi, nlp + i)) {
			goto badsym;
		}
	}
	if (pflg == 0) {
		qsort(nlp, n, sizeof(*nlp), compare);
	}
	if (nameflg) {
		printf("\n%s:\n", filename);
	}
	for (i=0; i<n; i++, nlp++) {
		if (gflg && ! (nlp->n_type & N_EXT))
			continue;
		if (cflg) {
			if (nlp->n_name[0] != '_')
				continue;
			for (j=0; j<sizeof(nlp->n_name)-1; j++)
				nlp->n_name[j] = nlp->n_name[j+1];
			nlp->n_name[sizeof(nlp->n_name)-1] = '\0';
		}
		j = nlp->n_type & N_TYPE;
		if (j == N_FN) {
			j = 6; // TODO: don't hard-code
		} else if (j > N_BSS) {
			j = N_ABS;
		}
		if (j==0 && nlp->n_value) {
			j = N_COMM;
		}
		if (uflg && j != 0) {
			continue;
		}
		if (! uflg || nameflg) {
			if (j == 0) {
				printf("        ");
			} else {
				printf("%07o ", nlp->n_value & 01777777);
			}
			if (j == N_FN) {
			} else {
				printf("%c ", (nlp->n_type & N_EXT ?
					"UATDBCF" : "uatdbcf") [j]);
			}
		}
		if (fflg && nlp->n_pad1) {
			printf("%.8s (%d)\n", nlp->n_name, nlp->n_pad1);
		} else {
			printf("%.8s\n", nlp->n_name);
		}
	}
	return 0;
}

int
main(int argc, char **argv) {
	int x;
	extern int optind;
	while ((x = getopt(argc, argv, "ncgurpf")) != EOF) {
		switch (x) {
		case 'n':		/* sort numerically */
			nflg++;
			break;
		case 'c':		/* c-style names */
			cflg++;
			break;
		case 'g':		/* globl symbols only */
			gflg++;
			break;
		case 'u':		/* undefined symbols only */
			uflg++;
			break;
		case 'r':		/* sort in reverse order */
			rflg = -1;
			break;
		case 'p':		/* don't sort -- symbol table order */
			pflg++;
			break;
		case 'f':		/* include extra information */
			fflg++;
			break;
		default:
			break;
		}
	}
	if (optind + 1 < argc) {
		while (optind < argc) {
			names(argv[optind++], 1);
		}
		return 0;
	} else if (optind < argc) {
		return names(argv[optind], 0);
	} else {
		return names("a.out", 0);
	}
}
