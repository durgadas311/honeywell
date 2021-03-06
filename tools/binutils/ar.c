/*
 * Archiver for unix v6.
 *
 * This file is part of BKUNIX project, which is distributed
 * under the terms of the GNU General Public License (GPL).
 * See the accompanying file "COPYING" for more details.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <time.h>
#include <sys/stat.h>
#include "ar.h"

struct ar_hdr arhdr;
struct stat stbuf;

#define	SKIP	1
#define	IODD	2
#define	OODD	4
#define	HEAD	8

#define	IFMT	060000
#define	DIR	0100000
#define	CHR	020000
#define	BLK	040000
#define	ISARG	01000
#define	LARGE	010000
#define	STXT	010000
#define	SUID	04000
#define	SGID	02000
#define	ROWN	0400
#define	WOWN	0200
#define	XOWN	0100
#define	RGRP	040
#define	WGRP	020
#define	XGRP	010
#define	ROTH	04
#define	WOTH	02
#define	XOTH	01
#define	RSTXT	01000

char	*man = { "mrxtdp" };
char	*opt = { "uvnbaic" };

void	(*comfun)();
char	flg[26];
char	**namv;
int	namc;
char	*arnam;
char	*ponam;
char	tfnam[] = "/tmp/vXXXXXX";
char	tf1nam[] = "/tmp/v1XXXXXX";
char	tf2nam[] = "/tmp/v2XXXXXX";
char	*file;
char	name[16];
int	af;
int	tf;
int	tf1 = -1;
int	tf2 = -1;
int	bastate;
char	buf[512];

int
getarhdr(struct ar_hdr *hdr, int fd) {
	if (read(fd, hdr, sizeof(*hdr)) != sizeof(*hdr)) {
		return 0;
	}
	return 1;
}

void
putarhdr(struct ar_hdr *hdr, int fd) {
	int d = write(fd, hdr, sizeof(*hdr));
}

void
done(int exitval) {
	unlink(tfnam);
	unlink(tf1nam);
	unlink(tf2nam);
	exit(exitval);
}

void
setcom(void (*fun)()) {
	if (comfun != NULL) {
		printf("only one of [%s] allowed\n", man);
		done(1);
	}
	comfun = fun;
}

void
init() {
	struct ar_file fh;
	fh.ar_magic = ARCMAGIC;
	int d;

	tf = mkstemp(tfnam);
	if (tf < 0) {
		printf("cannot create temp file\n");
		done(1);
	}
	d = write(tf, &fh, sizeof(fh));
}

int
getaf() {
	struct ar_file fh;

	af = open(arnam, 0);
	if (af < 0) {
		return(1);
	}
	if (read(af, &fh, sizeof(fh)) != sizeof(fh)) {
		printf("%s not in archive format\n", arnam);
		done(1);
	}
	if (fh.ar_magic != ARCMAGIC) {
		printf("%s not in archive format\n", arnam);
		done(1);
	}
	return(0);
}

void
noar() {
	printf("%s does not exist\n", arnam);
	done(1);
}

void
install() {
	register int i, d;

	for (i=1; i<4; i++)
		signal(i, SIG_IGN);
	close(af);
	af = creat(arnam, 0666);
	if (af < 0) {
		printf("cannot create %s\n", arnam);
		done(1);
	}
	lseek(tf, 0L, 0);
	while ((i = read(tf, buf, 512)) > 0)
		 d = write(af, buf, i);
	if (tf2 >= 0) {
		lseek(tf2, 0L, 0);
		while ((i = read(tf2, buf, 512)) > 0)
			 d = write(af, buf, i);
	}
	if (tf1 >= 0) {
		lseek(tf1, 0L, 0);
		while ((i = read(tf1, buf, 512)) > 0)
			 d = write(af, buf, i);
	}
}

/*
 * copy next file
 * size given in arhdr
 */
void
copyfil(fi, fo, flag)
	int fi, fo, flag;
{
	register int i, o, d;
	int pe;

	if (flag & HEAD)
		putarhdr(&arhdr, fo);
	pe = 0;
	while (arhdr.ar_size > 0) {
		i = o = 512;
		if (arhdr.ar_size < i) {
			i = o = arhdr.ar_size;
			if (i&1) {
				if (flag & IODD)
					i++;
				if (flag & OODD)
					o++;
			}
		}
		if (read(fi, buf, i) != i)
			pe++;
		if ((flag & SKIP) == 0)
			d = write(fo, buf, o);
		arhdr.ar_size -= 512;
	}
	if (pe)
		printf("phase error on %s\n", file);
}

char *
trim(s)
	char *s;
{
	register char *p1, *p2;

	for (p1 = s; *p1; p1++)
		;
	while (p1 > s) {
		if (*--p1 != '/')
			break;
		*p1 = 0;
	}
	p2 = s;
	for (p1 = s; *p1; p1++)
		if (*p1 == '/')
			p2 = p1+1;
	return(p2);
}

/*
 * insert the file 'file'
 * into the temporary file
 */
void
movefil(f)
	int f;
{
	register char *cp;
	register int i;

	cp = trim(file);
	for (i=0; i<14; i++) {
		arhdr.ar_name[i] = *cp;
		if (*cp)
			cp++;
	}
	arhdr.ar_size = stbuf.st_size;
	arhdr.ar_date = stbuf.st_mtime;
	arhdr.ar_uid = stbuf.st_uid;
#ifdef UNIX
	arhdr.ar_gid = stbuf.st_gid;
#endif
	arhdr.ar_mode = stbuf.st_mode;
	copyfil(f, tf, OODD+HEAD);
	close(f);
}

int
stats()
{
	register int f;

	f = open(file, 0);
	if (f < 0)
		return(f);
	if (fstat(f, &stbuf) < 0) {
		close(f);
		return(-1);
	}
	return(f);
}

void
mesg(c)
int c;
{
	if (flg['v'-'a'])
		if (c != 'c' || flg['v'-'a'] > 1)
			printf("%c - %s\n", c, file);
}

void
cleanup()
{
	register int i, f;

	for (i=0; i<namc; i++) {
		file = namv[i];
		if (file == 0)
			continue;
		namv[i] = 0;
		mesg('a');
		f = stats();
		if (f < 0) {
			printf("%s cannot open\n", file);
			continue;
		}
		movefil(f);
	}
	install();
}

int
getdir()
{
	register int i;

	if (!getarhdr(&arhdr, af)) {
		if (tf1 >= 0) {
			i = tf;
			tf = tf1;
			tf1 = i;
		}
		return(1);
	}
	for (i=0; i<14; i++)
		name[i] = arhdr.ar_name[i];
	file = name;
	return(0);
}

int
match()
{
	register int i;

	for (i=0; i<namc; i++) {
		if (namv[i] == 0)
			continue;
		if (strcmp(trim(namv[i]), file) == 0) {
			file = namv[i];
			namv[i] = 0;
			return(1);
		}
	}
	return(0);
}

void
bamatch()
{
	register int f;

	switch(bastate) {
	case 1:
		if (strcmp(file, ponam) != 0)
			return;
		bastate = 2;
		if (flg['a'-'a'])
			return;
	case 2:
		bastate = 0;
		f = mkstemp(tf1nam);
		if (f < 0) {
			printf("cannot create second temp\n");
			return;
		}
		tf1 = tf;
		tf = f;
	}
}

void
r_cmd()
{
	register int f;

	init();
	if (getaf()) {
		if (! flg['c'-'a']) {
			printf("creating %s\n", arnam);
		}
		cleanup();
		return;
	}
	while (!getdir()) {
		bamatch();
		if (namc == 0 || match()) {
			f = stats();
			if (f < 0) {
				if (namc)
					printf("cannot open %s\n", file);
				goto cp;
			}
			if (flg['u'-'a'])
				if (stbuf.st_mtime <= arhdr.ar_date) {
					close(f);
					goto cp;
				}
			mesg('r');
			copyfil(af, -1, IODD+SKIP);
			movefil(f);
			continue;
		}
cp:		mesg('c');
		copyfil(af, tf, IODD+OODD+HEAD);
	}
	cleanup();
}

void
d_cmd()
{
	init();
	if (getaf())
		noar();
	while (!getdir()) {
		if (match()) {
			mesg('d');
			copyfil(af, -1, IODD+SKIP);
			continue;
		}
		mesg('c');
		copyfil(af, tf, IODD+OODD+HEAD);
	}
	install();
}

void
x_cmd()
{
	register int f;

	if (getaf())
		noar();
	while (!getdir()) {
		if (namc == 0 || match()) {
			f = creat(file, arhdr.ar_mode & 0777);
			if (f < 0) {
				printf("%s cannot create\n", file);
				goto sk;
			}
			mesg('x');
			copyfil(af, f, IODD);
			close(f);
			continue;
		}
	sk:
		mesg('c');
		copyfil(af, -1, IODD+SKIP);
	}
}

void
p_cmd()
{
	if (getaf())
		noar();
	while (!getdir()) {
		if (namc == 0 || match()) {
			copyfil(af, 1, IODD);
			continue;
		}
		copyfil(af, -1, IODD+SKIP);
	}
}

void
m_cmd()
{
	init();
	if (getaf())
		noar();
	tf2 = mkstemp(tf2nam);
	if (tf2 < 0) {
		printf("cannot create third temp\n");
		done(1);
	}
	while (!getdir()) {
		bamatch();
		if (match()) {
			mesg('m');
			copyfil(af, tf2, IODD+OODD+HEAD);
			continue;
		}
		mesg('c');
		copyfil(af, tf, IODD+OODD+HEAD);
	}
	install();
}

void
pmode()
{
	static int m1[] = { 1, ROWN, 'r', '-' };
	static int m2[] = { 1, WOWN, 'w', '-' };
	static int m3[] = { 2, SUID, 's', XOWN, 'x', '-' };
	static int m4[] = { 1, RGRP, 'r', '-' };
	static int m5[] = { 1, WGRP, 'w', '-' };
	static int m6[] = { 2, SGID, 's', XGRP, 'x', '-' };
	static int m7[] = { 1, ROTH, 'r', '-' };
	static int m8[] = { 1, WOTH, 'w', '-' };
	static int m9[] = { 2, STXT, 't', XOTH, 'x', '-' };
	static int *m[] = { m1, m2, m3, m4, m5, m6, m7, m8, m9};
	register int *ap, n, **mp;

	for (mp = &m[0]; mp < &m[9]; mp++) {
		ap = *mp;
		n = *ap++;
		while (--n>=0 && (arhdr.ar_mode & *ap++)==0)
			ap++;
		putchar(*ap);
	}
}

void
t_cmd()
{
	register char *cp;

	if (getaf())
		noar();
	while (!getdir()) {
		if (namc == 0 || match()) {
			if (flg['v'-'a']) {
				pmode();
				printf("%3d/%1d", (unsigned char) arhdr.ar_uid,
					(unsigned char) arhdr.ar_gid);
				printf("%6ld", arhdr.ar_size);
				cp = ctime(&arhdr.ar_date);
				printf(" %-6.6s %-4.4s ", cp+4, cp+20);
			}
			printf("%s\n", trim(file));
		}
		copyfil(af, -1, IODD+SKIP);
	}
}

void killed()
{
	done(1);
}

void
usage()
{
	printf("usage: ar [%s][%s] archive files ...\n", opt, man);
	done(1);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	register char *cp;

	for (i=1; i<4; i++)
		if (signal(SIGINT, SIG_DFL) == SIG_DFL)
			signal(i, killed);
	if (argc < 3)
		usage();
	cp = argv[1];
	for (cp = argv[1]; *cp; cp++) {
		switch(*cp) {
		case 'v':
		case 'u':
		case 'n':
		case 'a':
		case 'b':
		case 'c':
		case 'i':
			flg[*cp - 'a']++;
			continue;

		case 'r':
			setcom(r_cmd);
			continue;

		case 'd':
			setcom(d_cmd);
			continue;

		case 'x':
			setcom(x_cmd);
			continue;

		case 't':
			setcom(t_cmd);
			continue;

		case 'p':
			setcom(p_cmd);
			continue;

		case 'm':
			setcom(m_cmd);
			continue;

		default:
			printf("bad option `%c'\n", *cp);
			done(1);
		}
	}
	if (flg['i'-'a'])
		flg['b'-'a']++;
	if (flg['a'-'a'] || flg['b'-'a']) {
		bastate = 1;
		ponam = trim(argv[2]);
		argv++;
		argc--;
		if (argc < 3)
			usage();
	}
	arnam = argv[2];
	namv = argv+3;
	namc = argc-3;
	if (comfun == 0) {
		if (flg['u'-'a'] == 0) {
			printf("one of [%s] must be specified\n", man);
			done(1);
		}
		setcom(r_cmd);
	}
	(*comfun)();

	for (i=0; i<namc; i++)
		if (namv[i])
			printf("%s not found\n", namv[i]);
	done(0);
	return 0;
}
