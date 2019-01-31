/*
 * Discard symbols from object files.
 *
 * This file is part of BKUNIX project, which is distributed
 * under the terms of the GNU General Public License (GPL).
 * See the accompanying file "COPYING" for more details.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include "a.out.h"

char	tname[] = "/tmp/sXXXXXX";
int	tf;

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
 * Write a.out header.
 */
void
writehdr(int fd, struct exec *hdr) {
	int d = write(fd, hdr, sizeof(*hdr));
}

int
copy(char *name, int fr, int to, long size) {
	register int s, n;
	char buf[512];

	while (size != 0) {
		s = 512;
		if (size < 512) {
			s = size;
		}
		n = read(fr, buf, s);
		if (n != s) {
			printf("%s unexpected eof\n", name);
			return(1);
		}
		n = write(to, buf, s);
		if (n != s) {
			printf("%s unexpected write eof\n", name);
			return(1);
		}
		size -= s;
	}
	return(0);
}

int
strip(char *name) {
	register int f;
	long size;
	int status;
	struct exec head;

	status = 0;
	f = open(name, O_RDONLY);
	if (f < 0) {
		perror(name);
		status = 1;
		goto out;
	}
	if (!readhdr(f, &head) || N_BADMAG(head)) {
		printf("%s not in a.out format\n", name);
		status = 1;
		goto out;
	}
	if (head.a_syms == 0 && (head.a_flag & A_NRELFLG)) {
		printf("%s already stripped\n", name);
		goto out;
	}
	size = (long)head.a_text + head.a_data;
	head.a_syms = 0;
	head.a_flag |= A_NRELFLG;

	lseek(tf, (off_t)0, SEEK_SET);
	writehdr(tf, &head);
	if (copy(name, f, tf, size) != 0) {
		status = 1;
		goto out;
	}
	size += sizeof(head);
	close(f);
	f = creat(name, 0666);
	if (f < 0) {
		perror(name);
		status = 1;
		goto out;
	}
	lseek(tf, (off_t)0, SEEK_SET);
	if (copy(name, tf, f, size) != 0)
		status = 2;
out:
	close(f);
	return status;
}

int
main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	int status;

	status = 0;
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	tf = mkstemp(tname);
	if (tf < 0) {
		printf("cannot create temp file\n");
		return 2;
	}
	for (i=1; i<argc; i++) {
		status = strip(argv[i]);
		if (status != 0)
			break;
	}
	close(tf);
	unlink(tname);
	return status;
}
