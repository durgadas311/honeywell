/*
 * size -- determine object size
 *
 * This file is part of BKUNIX project, which is distributed
 * under the terms of the GNU General Public License (GPL).
 * See the accompanying file "COPYING" for more details.
 */
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include "a.out.h"

int base;

/*
 * Read a.out header. Return 0 on error.
 */
int
readhdr(int fd, struct exec *hdr) {
	unsigned char buf [16];

	if (read(fd, hdr, sizeof(*hdr)) != sizeof(*hdr)) {
		return 0;
	}
	return 1;
}

void
size(char *filename) {
	struct exec hdr;
	int f;
	unsigned sum;
	static int header_printed;

	f = open(filename, O_RDONLY);
	if (f < 0) {
		perror(filename);
		return;
	}
	if (!header_printed) {
		printf("   text    data     bss    heap     %s     hex filename\n",
			base==8 ? "oct" : "dec");
		header_printed = 1;
	}
	if (!readhdr(f, &hdr) || N_BADMAG(hdr)) {
		printf("%s: bad format\n", filename);
		close(f);
		return;
	}
	sum = hdr.a_text + hdr.a_data + hdr.a_bss + hdr.a_heap;
	printf(base==16 ? "%#7x %#7x %#7x %#7x" :
		base==8 ? "%#7o %#7o %#7o %#7o" :
		"%7u %7u %7u %7u",
		hdr.a_text, hdr.a_data, hdr.a_bss, hdr.a_heap);
	printf(base==8 ? " %#7o" : " %7u", sum);
	printf(" %7x %s\n", sum, filename);
	close(f);
}

int
main(int argc, char **argv) {
	register char *cp;
	int nfiles;

	base = 10;
	nfiles = 0;
	while(--argc) {
		++argv;
		if (**argv != '-') {
			size (*argv);
			++nfiles;
			continue;
		}
		for (cp = *argv+1; *cp; cp++) {
			switch (*cp) {
			case 'o':       /* octal mode */
				base = 8;
				break;
			case 'd':       /* decimal mode */
				base = 10;
				break;
			case 'x':       /* hexadecimal mode */
				base = 16;
				break;
			default:
				fprintf (stderr, "Usage: size [-odx] file...\n");
				return (1);
			}
		}
	}
	if (nfiles == 0)
		size ("a.out");
	return 0;
}
