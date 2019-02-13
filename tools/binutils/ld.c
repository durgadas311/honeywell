/*
 * Link editor
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
#include <sys/stat.h>
#include <limits.h>
#include "a.out.h"
#include "ar.h"

#define	NSYM	1103	/* size of symbol table */
#define	NLIBF	256	/* max object files from libs */
#define	NPATH	10	/* max library directories */

FILE *text;
FILE *reloc;

struct	exec	filhdr;
struct	ar_hdr	archdr;

long	liblist[NLIBF];
long	*libp = liblist;

char	*libpath[NPATH];
char	**pathp = libpath;

struct	nlist	cursym;
struct	nlist	symtab[NSYM];
struct	nlist	*hshtab[NSYM+2];
struct	nlist	*symp = symtab;		/* first free symbol table slot */
struct	nlist	*p_etext;
struct	nlist	*p_edata;
struct	nlist	*p_end;

struct nlocal {
	int symno;
	struct nlist *symref;
};

struct	nlocal	local[NSYM/2];

unsigned aflag = 1340;	/* address to relocate, HW monitor reserved */
unsigned tflag;		/* stack size to add to .bss, default 0 */
int	xflag;		/* discard local symbols */
int	Xflag;		/* discard locals starting with 'L' */
int	rflag;		/* preserve relocation bits, don't define common */
int	arflag;		/* original copy of rflag */
int	sflag;		/* discard all symbols */
int	nflag;		/* pure procedure */
int	dflag;		/* define common even with rflag */
int	iflag;		/* I/D space separated */

char	*filname;
char 	*outname = "a.out";

unsigned int	tsize;
unsigned int	dsize;
unsigned int	bsize;
unsigned int	ssize;
unsigned int	nsym;

unsigned int	torigin;
unsigned int	dorigin;
unsigned int	borigin;

unsigned int	ctrel;
unsigned int	cdrel;
unsigned int	cbrel;

int	errlev;
char	tdname[] = "/tmp/laXXXXXX";
char	tsname[] = "/tmp/lbXXXXXX";
char	ttrname[] = "/tmp/lcXXXXXX";
char	tdrname[] = "/tmp/ldXXXXXX";
FILE	*toutb;
FILE	*doutb;
FILE	*troutb;
FILE	*droutb;
FILE	*soutb;

// TODO: just look for '_'? or...?
// Need to eliminate others, "Pxxxx", "@ssss", ".xxxx", ???
#define local_sym(s)    (s.n_name[0] != 'L')

void
cleanup() {
	unlink(tdname);
	unlink(tsname);
	unlink(ttrname);
	unlink(tdrname);
}

void
fatal() {
	cleanup();
	unlink(outname);
	exit(4);
}

void
error(severe, s)
	int severe;
	char *s;
{
	if (filname) {
		printf("%s", filname);
		if (archdr.ar_name[0])
			printf("(%.14s)", archdr.ar_name);
		printf(": ");
	}
	printf("%s\n", s);
	if (severe)
		fatal();
	errlev = 2;
}

/*
 * Find a name in symbol table.
 * Return a pointer to a (possibly empty) hash table slot.
 */
struct nlist **
lookup(name)
	char *name;
{
	int hash;
	register struct nlist **hp;
	register char *cp;

	hash = 0;
	for (cp=name; cp < name+sizeof(cursym.n_name) && *cp; )
		hash = (hash << 1) + *cp++;
	hash &= 077777;
	hp = &hshtab[hash % NSYM + 2];
	while (*hp != 0) {
		if (strncmp ((*hp)->n_name, name, sizeof(cursym.n_name)) == 0) {
			break;
		}
		++hp;
		if (hp >= &hshtab[NSYM+2])
			hp = hshtab;
	}
	return hp;
}

/*
 * Add a name from cursym to symbol table.
 * Return a pointer to the symbol table slot.
 */
struct nlist *
enter()
{
	register struct nlist *sp;

	sp = symp;
	if (sp >= &symtab[NSYM])
		error(1, "Symbol table overflow");
	memset(sp->n_name, 0, sizeof(sp->n_name));
	strncpy(sp->n_name, cursym.n_name, sizeof(sp->n_name));
	sp->n_type = cursym.n_type;
	sp->n_value = cursym.n_value;
	symp++;
	return sp;
}

/*
 * Make file symbol: set cursym to the needed value.
 */
void
mkfsym(s)
	char *s;
{
	if (sflag || xflag)
		return;
	memset(cursym.n_name, 0, sizeof(cursym.n_name));
	strncpy(cursym.n_name, s, sizeof(cursym.n_name));
	cursym.n_type = N_FN;
	cursym.n_value = torigin;
}

struct nlist *
lookloc(limit, r)
	register struct nlocal *limit;
	int r;
{
	register struct nlocal *clp;
	register int sn;

	sn = (r >> 4) & 07777;
	for (clp=local; clp<limit; clp++)
		if (clp->symno == sn)
			return clp->symref;
	error(1, "Local symbol botch");
	return 0;
}

int admode;	// address mode for *current* item, if getreloc() != 0.

/*
 * Write 24-bit value big-endian to file.
 */
void
putword(FILE *fd, uint32_t w, uint32_t p) {
	int c;
	switch(admode) {
	case 4:
		c = (w >> 18) & 077;
		c |= (p >> 24) & 0300;
		putc(c, fd);
		/* FALLTHROUGH */
	case 3:
		c = (w >> 12) & 077;
		c |= (p >> 16) & 0300;
		putc(c, fd);
		/* FALLTHROUGH */
	case 2:
		c = (w >> 6) & 077;
		c |= (p >> 8) & 0300;
		putc(c, fd);
		c = w & 077;
		c |= p & 0300;
		putc(c, fd);
		break;
	}
}

/*
 * Write 32-bit value big-endian to file.
 */
void
putint(uint32_t w, FILE *fd) {
	switch(admode) {
	case 4:
		putc((w >> 24) & 0xff, fd);
		/* FALLTHROUGH */
	case 3:
		putc((w >> 16) & 0xff, fd);
		/* FALLTHROUGH */
	case 2:
		putc((w >> 8) & 0xff, fd);
		putc(w & 0xff, fd);
	}
}

/*
 * Read 24-bit value/4-bytes big-endian from file.
 */
uint32_t
getword(FILE *fd, uint32_t *pp) {
	uint32_t w = 0;
	uint32_t p = 0;
	int c;
	int x = admode;
	while (x > 0) {
		c = getc(fd);
		w <<= 6;
		w |= c & 077;
		p <<= 8;
		p |= (c & 0300);
		--x;
	}
	if (pp) *pp = p;
	return w;
}

uint32_t
getreloc(FILE *fd) {
	int x;
	uint32_t r = getc(fd);
	if (r <= 0) {
		admode = 0;
		return 0;	// "0" means one byte only
	}
	if (r & am_reltag(2)) {
		admode = 2;
	} else if (r & am_reltag(3)) {
		admode = 3;
	} else if (r & am_reltag(4)) {
		admode = 4;
	} else {
		// error: corrupt .reloc
	}
	x = admode - 1;
	while (x > 0) {
		r <<= 8;
		r |= getc(fd);
		--x;
	}
	return r; // caller must ignore, but preserve, reloc tag bits
}

/*
 * Read a.out header. Return 0 on error.
 */
int
gethdr(FILE *fd, struct exec *hdr) {
	if (fread(hdr, sizeof(*hdr), 1, fd) != 1) {
		return 0;
	}
	return 1;
}

/*
 * Write a.out header.
 */
void
puthdr(FILE *fd, struct exec *hdr) {
	fwrite(hdr, sizeof(*hdr), 1, fd);
}

/*
 * Read archive header. Return 0 on error.
 */
int
getarhdr(FILE *fd, struct ar_hdr *hdr) {
	if (fread(hdr, sizeof(*hdr), 1, fd) != 1) {
		return 0;
	}
	return 1;
}

/*
 * Read a.out symbol. Return 0 on error.
 */
int
getsym(FILE *fd, struct nlist *sym) {
	if (fread(sym, sizeof(*sym), 1, fd) != 1) {
		return 0;
	}
	return 1;
}

/*
 * Write a.out symbol.
 */
void
putsym(FILE *fd, struct nlist *sym) {
	if (!fd) return;
	fwrite(sym, sizeof(*sym), 1, fd);
}

/*
 * Create temporary file.
 */
FILE *
tcreat(char *name) {
	int fd;

	fd = mkstemp(name);
	if (fd < 0) {
		error(1, "Can't create temp");
	}
	return fdopen(fd, "r+");
}

/*
 * Copy temporary file to output and close it.
 */
void
copy(FILE *fd) {
	register int n;
	char buf[512];

	if (!fd) return;
	fseek(fd, 0L, 0);
	while ((n = fread(buf, 1, 512, fd)) > 0) {
		fwrite(buf, 1, n, toutb);
	}
	fclose(fd);
}

void
readhdr(long loff) {
	register unsigned int st, sd;

	fseek(text, loff, 0);
	if (! gethdr(text, &filhdr)) {
		error(1, "Cannot read header");
	}
	if (filhdr.a_magic != A_FMAGIC) {
		error(1, "Bad format");
	}
	st = filhdr.a_text;
	cdrel = - st;
	sd = filhdr.a_data;
	cbrel = - (st + sd);
}

int
getfile(char *cp) {
	register int c;
	char **dir;
	static char namebuf[PATH_MAX];
	struct ar_file fh;

	filname = cp;
	archdr.ar_name[0] = '\0';
	if (cp[0] != '-' || cp[1] != 'l') {
		/* Open plain file. */
		text = fopen(filname, "r");
		if (!text)
			error(1, "Cannot open");
	} else {
		/* Search for library. */
		for (dir = libpath; dir < pathp; ++dir) {
			if (strlen(*dir) + strlen(cp+2) > sizeof(namebuf)-8) {
				continue;
			}
			strcpy (namebuf, *dir);
			strcat (namebuf, "/lib");
			strcat (namebuf, cp+2);
			strcat (namebuf, ".a");
			text = fopen(namebuf, "r");
			if (text) {
				filname = namebuf;
				break;
			}
		}
		if (dir >= pathp)
			error(1, "Library not found");
	}
	/* Is it an archive? */
	if (fread(&fh, sizeof(fh), 1, text) != 1 ||
			feof(text) || ferror(text)) {
		error(1, "Empty file");
	}
	return (fh.ar_magic == ARCMAGIC);
}

void
symreloc()
{
	switch (cursym.n_type & (N_TYPE | N_EXT)) {
	case N_TEXT:
	case N_EXT+N_TEXT:
		cursym.n_value += ctrel;
		return;
	case N_DATA:
	case N_EXT+N_DATA:
		cursym.n_value += cdrel;
		return;
	case N_BSS:
	case N_EXT+N_BSS:
		cursym.n_value += cbrel;
		return;
	case N_EXT+N_UNDF:
		return;
	}
	if (cursym.n_type & N_EXT)
		cursym.n_type = N_EXT + N_ABS;
}

int
load1(libflg, loff)
	int libflg;
	long loff;
{
	register struct nlist *sp, **hp, ***lp;
	struct nlist *ssymp;
	int ndef, nloc, n;

	readhdr(loff);
	ctrel = tsize;
	cdrel += dsize;
	cbrel += bsize;
	if (filhdr.a_flag & A_NRELFLG) {
		error(0, "No relocation bits");
		return(0);
	}
	ndef = 0;
	nloc = sizeof cursym;
	ssymp = symp;

	/* Use local[] as stack of symbol references. */
	lp = (struct nlist***) local;

	loff += sizeof filhdr + filhdr.a_text + filhdr.a_text +
		filhdr.a_data + filhdr.a_data;
	fseek(text, loff, 0);
	for (n=0; n<filhdr.a_syms; n+=sizeof cursym) {
		getsym(text, &cursym);
		if ((cursym.n_type & N_EXT) == 0) {
			if (Xflag==0 || !local_sym(cursym)) {
				nloc += sizeof cursym;
			}
			continue;
		}
		symreloc();
		hp = lookup(cursym.n_name);
		sp = *hp;
		if (sp == 0) {
			*hp = enter();
			*lp++ = hp;
			continue;
		}
		if (sp->n_type != N_EXT+N_UNDF)
			continue;
		if (cursym.n_type == N_EXT+N_UNDF) {
			if (cursym.n_value > sp->n_value)
				sp->n_value = cursym.n_value;
			continue;
		}
		if (sp->n_value != 0 && cursym.n_type == N_EXT+N_TEXT)
			continue;
		ndef++;
		sp->n_type = cursym.n_type;
		sp->n_value = cursym.n_value;
	}
	if (libflg==0 || ndef) {
		unsigned int total;

		tsize += filhdr.a_text;
		dsize += filhdr.a_data;
		bsize += filhdr.a_bss;
		ssize += nloc;

		total = tsize + dsize + bsize;
		if (total > 0xF000) {
			/*fprintf (stderr, "Size = %d(%x)\n", total, total);*/
			error(1, "Program is too big");
		}
		return(1);
	}
	/*
	 * No symbols defined by this library member.
	 * Rip out the hash table entries and reset the symbol table.
	 */
	symp = ssymp;
	while (lp > (struct nlist***) local)
		**--lp = 0;
	return(0);
}

void
load1arg(filename)
	register char *filename;
{
	long loff;
	register int nlinked;

	if (getfile(filename)==0) {
/*printf("load1arg: %s\n", filename);*/
		load1(0, 0L);
		fclose(text);
		return;
	}
again:
	loff = sizeof(struct ar_file);
	nlinked = 0;
	for (;;) {
/*printf("load1arg: seek %ld\n", loff);*/
		fseek(text, loff, 0);
		if (! getarhdr(text, &archdr)) {
			if (nlinked) {
				/* Scan archive again until
				 * no unreferenced symbols found. */
				goto again;
			}
			break;
		}
		if (load1(1, loff + sizeof(struct ar_hdr))) {
/*printf("load1arg: %s(%.14s) offset %ld\n", filename, archdr.ar_name, loff);*/
			*libp++ = loff;
			nlinked++;
		}
		loff += archdr.ar_size + sizeof(struct ar_hdr);
	}
	fclose(text);
	*libp++ = 0;
}

void
middle()
{
	register struct nlist *sp;
	register unsigned int t, csize;
	unsigned int nund, corigin;

	p_etext = *lookup("_etext");
	p_edata = *lookup("_edata");
	p_end = *lookup("_end");
	/*
	 * If there are any undefined symbols, save the relocation bits.
	 */
	if (rflag==0) for (sp=symtab; sp<symp; sp++) {
		if (sp->n_type==N_EXT+N_UNDF && sp->n_value==0
		 && sp!=p_end && sp!=p_edata && sp!=p_etext) {
			rflag++;
			dflag = 0;
			nflag = 0;
			iflag = 0;
			sflag = 0;
			break;
		}
	}
	/*
	 * Assign common locations.
	 */
	csize = 0;
	if (dflag || rflag==0) {
		for (sp=symtab; sp<symp; sp++)
			if (sp->n_type==N_EXT+N_UNDF && (t=sp->n_value)!=0) {
				t = (t+1) & ~01;
				sp->n_value = csize;
				sp->n_type = N_EXT+N_COMM;
				csize += t;
			}
		if (p_etext && p_etext->n_type==N_EXT+N_UNDF) {
			p_etext->n_type = N_EXT+N_TEXT;
			p_etext->n_value = tsize;
		}
		if (p_edata && p_edata->n_type==N_EXT+N_UNDF) {
			p_edata->n_type = N_EXT+N_DATA;
			p_edata->n_value = dsize;
		}
		if (p_end && p_end->n_type==N_EXT+N_UNDF) {
			p_end->n_type = N_EXT+N_BSS;
			p_end->n_value = bsize;
		}
	}
	/*
	 * Now set symbols to their final value
	 */
	if (nflag || iflag)
		tsize = (tsize + 077) & ~077;
	dorigin = tsize;
	if (nflag)
		dorigin = (tsize+017777) & ~017777;
	if (iflag)
		dorigin = 0;
	corigin = dorigin + dsize;
	borigin = corigin + csize;
	torigin += aflag;
	dorigin += aflag;
	corigin += aflag;
	borigin += aflag;

	nund = 0;
	for (sp=symtab; sp<symp; sp++) {
		switch (sp->n_type) {
		case N_EXT+N_UNDF:
			if (!rflag)
				errlev |= 01;
			if (arflag == 0 && sp->n_value == 0 && sp != p_end &&
			    sp != p_edata && sp != p_etext) {
				if (nund==0)
					printf("Undefined:\n");
				nund++;
				printf("\t%.8s\n", sp->n_name);
			}
			continue;

		case N_EXT+N_ABS:
		default:
			continue;

		case N_EXT+N_TEXT:
			sp->n_value += torigin;
			continue;

		case N_EXT+N_DATA:
			sp->n_value += dorigin;
			continue;

		case N_EXT+N_BSS:
			sp->n_value += borigin;
			continue;

		case N_EXT+N_COMM:
			sp->n_type = N_EXT+N_BSS;
			sp->n_value += corigin;
			continue;
		}
	}
	if (sflag || xflag)
		ssize = 0;
	bsize += csize;
	nsym = ssize / (sizeof cursym);
/*printf("middle: %d + %d (%d) + %d, nsym = %d\n", tsize, dsize, csize, bsize, nsym);*/
}

void
setupout()
{
	toutb = fopen(outname, "w");
	if (! toutb)
		error(1, "Can't create output file");
	doutb = tcreat(tdname);
	if (sflag==0 || xflag==0)
		soutb = tcreat(tsname);
	if (rflag) {
		troutb = tcreat(ttrname);
		droutb = tcreat(tdrname);
	}
	if (nflag)
		filhdr.a_magic = A_NMAGIC;
	else if (iflag)
		filhdr.a_magic = A_IMAGIC;
	else
		filhdr.a_magic = A_FMAGIC;
	filhdr.a_text = tsize;
	filhdr.a_data = dsize;
	filhdr.a_bss = bsize + tflag;
	filhdr.a_syms = sflag ? 0 : (ssize + (sizeof cursym)*(symp-symtab));
	filhdr.a_entry = aflag; /* use 'a_entry' for loading base address */
	filhdr.a_unused = 0;
	filhdr.a_flag = rflag ? 0 : A_NRELFLG;
	puthdr(toutb, &filhdr);
}

static uint32_t am_mask(int am) {
	switch(am) {
	case 2:	return             0b111111111111;
	case 3:	return       0b000111111111111111;
	case 4:	return 0b000001111111111111111111;
	}
	return 0;
}

void
load2td(bytes, lp, creloc, b1, b2)
	unsigned int bytes;
	struct nlocal *lp;
	int creloc;
	FILE *b1, *b2;
{
	register int r, t;
	register struct nlist *sp;
	uint32_t amm, p;

	while (bytes > 0) {
		r = getreloc(reloc); // sets admode
		if (feof(reloc) || ferror (reloc)) {
			error(1, "Relocation error");
		}
		if (!r) { // not a relocatable field...
			t = getc(text);
			putc(t, b1);
			if (rflag)
				putc(r, b2);
			--bytes;
			continue;
		}
		t = getword(text, &p);	// special format - might have adr mod!
		amm = (t & ~am_mask(admode));
		t &= am_mask(admode);
		switch (r & A_RMASK) {
		case A_RTEXT:
			t += ctrel;
			break;
		case A_RDATA:
			t += cdrel;
			break;
		case A_RBSS:
			t += cbrel;
			break;
		case A_REXT:
			sp = lookloc(lp, r);
			if ((sp->n_type & (N_TYPE | N_EXT)) == N_EXT+N_UNDF) {
				r = A_REXT + ((nsym + (sp - symtab)) << 4);
			} else {
				t += sp->n_value;
				r = (((sp->n_type & (N_TYPE | N_EXT)) - (N_EXT+N_ABS)) << 1);
			}
			r |= am_reltag32(admode);
			break;
		}
		t &= am_mask(admode); // trunc over/underflow
		putword(b1, t | amm, p);
		if (rflag) {
			putint(r, b2);
		}
		bytes -= admode;
	}
}

void
load2(loff)
	long loff;
{
	register struct nlist *sp;
	register struct nlocal *lp;
	register int symno, n;

	readhdr(loff);
	ctrel = torigin;
	cdrel += dorigin;
	cbrel += borigin;
	/*
	 * Reread the symbol table, recording the numbering
	 * of symbols for fixing external references.
	 */
	lp = local;
	symno = -1;
	loff += sizeof filhdr;
	fseek(text, loff + filhdr.a_text + filhdr.a_data +
		filhdr.a_text + filhdr.a_data, 0);
	for (n=0; n<filhdr.a_syms; n+=sizeof cursym) {
		symno++;
		getsym(text, &cursym);
		symreloc();
		if ((cursym.n_type & N_EXT) == 0) {
			if (! sflag && ! xflag && (! Xflag ||
					!local_sym(cursym))) {
				putsym(soutb, &cursym);
			}
			continue;
		}
		sp = *lookup(cursym.n_name);
		if (sp == 0)
			error(1, "Internal error: symbol not found");
		if (cursym.n_type == N_EXT+N_UNDF) {
			if (lp >= &local[NSYM/2])
				error(1, "Local symbol overflow");
			lp->symno = symno;
			lp->symref = sp;
			++lp;
			continue;
		}
		if (cursym.n_type != sp->n_type ||
		    cursym.n_value != sp->n_value) {
			printf("%.8s: ", cursym.n_name);
			error(0, "Multiply defined");
		}
	}
	if (filhdr.a_text > 0) {
		fseek(text, loff, 0);
		fseek(reloc, loff + filhdr.a_text + filhdr.a_data, 0);
		load2td(filhdr.a_text, lp, ctrel, toutb, troutb);
	}
	if (filhdr.a_data > 0) {
		fseek(text, loff + filhdr.a_text, 0);
		fseek(reloc, loff + filhdr.a_text + filhdr.a_text + filhdr.a_data, 0);
		load2td(filhdr.a_data, lp, cdrel, doutb, droutb);
	}
	torigin += filhdr.a_text;
	dorigin += filhdr.a_data;
	borigin += filhdr.a_bss;
}

void
load2arg(filename)
	register char *filename;
{
	register long *lp;
	char *p;

	if (getfile(filename) == 0) {
/*printf("load2arg: %s\n", filename);*/
		reloc = fopen(filename, "r");
		p = strrchr (filename, '/');
		if (p)
			filename = p+1;
		mkfsym(filename);
		putsym(soutb, &cursym);
		load2(0L);
		fclose(text);
		fclose(reloc);
		return;
	}
	reloc = fopen(filname, "r");
	for (lp = libp; *lp > 0; lp++) {
		fseek(text, *lp, 0);
		if (! getarhdr(text, &archdr)) {
/*printf("load2arg: %s(%.14s) offset %ld\n", filename, archdr.ar_name, *lp);*/
			error(1, "Cannot read archive header");
		}
/*printf("load2arg%d/%d: %s(%.14s) offset %ld\n", fileno(text), fileno(reloc), filename, archdr.ar_name, *lp);*/
		mkfsym(archdr.ar_name);
		putsym(soutb, &cursym);
		load2(*lp + sizeof(struct ar_hdr));
	}
	libp = ++lp;
	fclose(text);
	fclose(reloc);
}

// TODO: ensure .text ends with a word mark (e.g. halt instruction).
// (if output is executable) Or else, make 'out2brt' (et al.) do that.
void
finishout()
{
	register int n;
	struct nlist *p;

	if (nflag || iflag) {
#if 0	// TODO: when is this needed?
		n = torigin;
		while (n & 077) {
			n += 2;
			putword(0, toutb);
			if (rflag)
				putword(0, troutb);
		}
#endif
	}
	copy(doutb);
	if (rflag) {
		copy(troutb);
		copy(droutb);
	}
	if (sflag==0) {
		if (xflag==0)
			copy(soutb);
		for (p=symtab; p < symp; p++)
			putsym(toutb, p);
	}
	fclose(toutb);
}

int
getnum(ap)
	char *ap;
{
	register int n, c;
	register char *p;
	int f;
	int base;

	p = ap;
	n = 0;
	f = 0;
	base = 10;
loop:
	while(*p == ' ' || *p == '	')
		p++;
	if(*p == '-') {
		f++;
		p++;
		goto loop;
	}
	if (*p == '0') {
		p++;
		base = 8;
		if (*p == 'x' || *p == 'X') {
			p++;
			base = 16;
		}
	}
	while(*p) {
		char sch;
		sch = '0';
		if (base == 8 && !(*p >= '0' && *p <= '7'))
			break;
		else if (base == 10 && !(*p >= '0' && *p <= '9'))
			break;
		else if (base == 16) {
		   if (*p >= 'A' && *p <= 'F')
		   	sch = 0x37;
		   else if (*p >= 'a' && *p <= 'f')
		   	sch = 0x57;
		   else if (!(*p >= '0' && *p <= '9'))
			break;
		}
		n = n*base + *p++ - sch;
	}
	if(f)
		n = -n;
	return(n);
}

int
main(argc, argv)
	int argc;
	char **argv;
{
	register int c;
	register char *ap, **p;
	struct nlist **hp;
	char *option;

	if (signal(SIGINT, SIG_DFL) == SIG_DFL)
		signal(SIGINT, fatal);
	if (argc == 1) {
		printf ("ld: no input files\n");
		exit(4);
	}
	p = argv + 1;
	for (c = 1; c<argc; c++) {
		filname = 0;
		ap = *p++;
		if (*ap == '-') switch (ap[1]) {
		case 'u':
			if (ap[2]) {		/* option -uN */
				option = ap+2;
			} else {		/* option -u N (with space) */
				if (++c >= argc)
					error(1, "Bad -u");
				option = *p++;
			}
			hp = lookup(option);
			if (*hp == 0) {
				*hp = symp;
				memset(cursym.n_name, 0, sizeof(cursym.n_name));
				strncpy(cursym.n_name, option, sizeof(cursym.n_name));
				cursym.n_type = N_EXT + N_UNDF;
				cursym.n_value = 0;
				enter();
			}
			continue;
		case 'l':
			break;
		case 'L':
			if (pathp >= &libpath[NPATH])
				error(1, "Too many -L");
			*pathp++ = &ap[2];
			continue;
		case 'x':
			xflag++;
			continue;
		case 'X':
			Xflag++;
			continue;
		case 'r':
			rflag++;
			arflag++;
			continue;
		case 's':
			sflag++;
			xflag++;
			continue;
		case 'n':
			nflag++;
			continue;
		case 'd':
			dflag++;
			continue;
		case 'i':
			iflag++;
			continue;
		case 'a':
			if (ap[2]) {		/* option -aN */
				option = ap+2;
			} else {		/* option -a N (with space) */
				if (++c >= argc)
					error(1, "Bad -a");
				option = *p++;
			}
			aflag = getnum (option);
			continue;
		case 't':
			if (ap[2]) {		/* option -tN */
				option = ap+2;
			} else {		/* option -t N (with space) */
				if (++c >= argc)
					error(1, "Bad -t");
				option = *p++;
			}
			tflag = getnum (option);
			continue;
		case 'o':
			if (ap[2]) {		/* option -oN */
				option = ap+2;
			} else {		/* option -o N (with space) */
				if (++c >= argc)
					error(1, "Bad -o");
				option = *p++;
			}
			outname = option;
			continue;
		}
		load1arg(ap);
	}
	middle();
	setupout();
	p = argv+1;
	libp = liblist;
	for (c=1; c<argc; c++) {
		ap = *p++;
		if (*ap == '-') switch (ap[1]) {
		case 'l':
			break;
		case 'a':
		case 'u':
		case 'o':
			if (ap[2])
				continue;	/* option -aN */
			/* option -a N (with space) */
			++c;
			++p;
		default:
			continue;
		}
		load2arg(ap);
	}
	finishout();
	cleanup();
	if (errlev != 0) {
		unlink(outname);
		exit(errlev);
	}
	return 0;
}
