/*
 *		Assembler output & location counter routines
 */

#include <unistd.h>
#include "as.h"
#include "a.out.h"

int		ofile;		/* fd of output file */
int		symseek;	/* offset of symbol table in a.out */

/* initialize segments (at the beginning of each pass) */

void seginit()
{
	data.loc = data.maxloc = 0;
	text.loc = text.maxloc = 0;
	bss.loc  = bss.maxloc  = 0;

	curseg = &text;
	currel = RTEXT;
}

struct exec hdr;

/* write out the a.out header and initialize output buffers
   (at the beginning of code-generation pass(es)) */

void outhdr()
{
	int n;

	if (text.maxloc < text.loc) text.maxloc = text.loc;
	if (data.maxloc < data.loc) data.maxloc = data.loc;
	if (bss.maxloc < bss.loc)  bss.maxloc = bss.loc;

	hdr.a_magic	= A_FMAGIC;
	hdr.a_text	= text.maxloc;
	hdr.a_data	= data.maxloc;
	hdr.a_bss	= bss.maxloc;
	hdr.a_syms	= symcount * sizeof(struct nlist);
	hdr.a_entry	= 0;
	hdr.a_unused 	= 0;
	hdr.a_flag  	= 0;

	n = write(ofile, &hdr, sizeof(hdr));

	text.tseek  = n = sizeof(hdr);
	data.tseek  = (n += hdr.a_text);
	text.rseek  = (n += hdr.a_data);
	data.rseek  = (n += hdr.a_text);
	symseek     = (n += hdr.a_data);

	text.nchar  = 0;
	data.nchar  = 0;
}

/*
 * Flush the output buffer for the specified segment
 */
void oflush(sp)
	SEGMNT *sp;
{
	int n, d;

	if ((n = sp->nchar) == 0)
		return;

	lseek(ofile, (long)sp->tseek, 0);
	d = write(ofile, sp->tbuf, n);
	lseek(ofile, (long)sp->rseek, 0);
	d = write(ofile, sp->rbuf, n);
	sp->tseek += n;
	sp->rseek += n;
}

/*
 * Define the symbol in curlab to have the given relocatability and value
 *	- converts relocation bits to symbol type
 *	- checks for illegal redefinitions
 *	- redefinition of absolute symbols is always permitted
 * 	- allow redefintion of common symbols to data symbols (the linker
 * 	  will resolve this)
 */
void deflab(int rel, uint32_t val) {
	int type, otype;

	if (!curlab)
		return;

	/*
	 * convert relocation bits to symbol type
	 */
	if (rel&REXT)
		type = (rel & ~RSEG)<<4;
	else
		type = (rel>>1)+1;

	/*
	 * check for illegal redefinitions
	 */
	otype = curlab->type & SSEG;
	if (pass == 0) {
		if (otype!=SUNDEF && (otype!=SABS || type!=SABS)) {
			cerror(errm);
		}
	} else if (otype != type) {
		cerror(errm);
	}
	else if (pass_gen && type!=SABS && !(curlab->type&SEXT) && curlab->value!=val) {
		cerror(errp);
	}

	// must preserve SREV...
	if (curlab->type & SREV) type |= SREV;
	curlab->type = (curlab->type & SEXT) | type;
	curlab->value = val;
}

/*
 * Emit a string of <len> characters
 */
void putstr(len)
	int len;
{
	SEGMNT	*sp;
	register int	i, n;

	if ((sp = curseg) != &data && sp != &text)
		xerror(errd);

	sp->loc += len;

	if (pass_gen) {
		n = sp->nchar;
		for (i = 0; i<len; i++) {
			if (n >= OBSIZE) {
				sp->nchar = n;
				oflush(sp);
				n = 0;
			}
			sp->tbuf[n] = strbuf[i];
			sp->rbuf[n] = (n&1) ? RABS : 0;
			++n;
		}
		sp->nchar = n;
	}

	if (pass_lst)
		for (i = 0; i<len; i++)
			lstb(strbuf[i], RABS);
}

/*
 * Relocate a value in segment rel
 */
int reloc(uint32_t val, int rel) {
	switch (rel) {
	case RBSS:	val += hdr.a_data;
	case RDATA:	val += hdr.a_text;
	}
	return val;
}

/*
 * Emit a string in HW200 character set
 */
void putstrhw(char *val, int pnc) {
	int v;
	char *vp = val;

	while (*vp) {
		v = hw200[*vp & 0x7f];
		if (vp == val) {
			v |= (pnc >> 8);
		}
		++vp;
		if (!*vp) {
			v |= (pnc & 0377);
		}
		putb(v, 1);
	}
}

/*
 * Emit an address
 */
void putaddr(uint32_t val, int rel, int pnc) {
	SEGMNT	*sp;
	int	n, x;
	uint32_t v, r;

	if ((sp = curseg) != &text && sp != &data) {
		xerror(errd);
	}
	sp->loc += admode;
	if (pass_gen) {
		val = reloc(val, rel);
		if ((n = sp->nchar) + admode >= OBSIZE) {
			oflush(sp);
			n = 0;
		}
		v = val;
		r = rel;
		if (r) {
			if (rel & ~am_relmsk(admode)) {
				// error: sym index overflow
			}
			r |= am_reltag32(admode);
		}
		for (x = admode - 1; x >= 0; --x) {
			sp->tbuf[n + x] = (v & 077);
			sp->rbuf[n + x] = (r & 0xff);
			v >>= 6;
			r >>= 8;
		}
		if (pnc & 0377) {
			sp->tbuf[n + admode - 1] |= (pnc & 0377);
		}
		pnc >>= 8;
		if (pnc & 0377) {
			sp->tbuf[n] |= (pnc & 0377);
		}
		sp->nchar += admode;
	}

	if (pass_lst)
		lstaddr(val, rel);
}

/*
 * Emit a byte (character). May include punctuation.
 */
void putb(int val, int lst) {
	SEGMNT	*sp;
	int	n;

	if ((sp = curseg) != &data && sp != &text)
		xerror(errd);

	sp->loc++;

	if (pass_gen) {
		if ((n = sp->nchar) + 1 >= OBSIZE) {
			oflush(sp);
			n = 0;
		}
		sp->tbuf[n] = val;
		sp->rbuf[n] = RABS;
		sp->nchar += 1;
	}
	if (pass_lst && lst)
		lstb(val, RABS);
}

/*
 * Align generated code to the specified boundary by padding with zeroes
 */
void align(boundary)
	int boundary;
{
	SEGMNT	*sp;
	int		n;

	n = boundary - 1;
	if ((sp = curseg) != &text && sp != &data) {
		sp->loc = (sp->loc + n) & ~n;
		return;
	}
	while (sp->loc&n)
		putb(0, 0);
}

/*
 * Move the current location counter to the specified location
 */
void org(loc)
	int loc;
{
	register SEGMNT	*sp;
	register int	off;

	sp = curseg;
	if ((off = loc - sp->loc) == 0)
		return;
	if (off < 0)
		xerror(erro);

	if (pass_gen && (sp == &data || sp == &text)) {
		while (sp->loc <  loc) putb(0, 0);
	}
	else
		sp->loc = loc;

	if (sp->maxloc < sp->loc)
		sp->maxloc = sp->loc;
}

/*
 * Append symbol table to object file
 */
void symout()
{
	register SYMBOL *sp;
	struct nlist symout;
	int d;

	lseek(ofile, (long)symseek, 0);
	for (sp = firstsym; sp; sp = sp->link) {

		/* relocate */
		switch (sp->type & SSEG) {

		case SUNDEF:
			if( uflag ) sp->type |= SEXT;
			break;

		case SBSS:
			sp->value += hdr.a_data;
		case SDATA:
			sp->value += hdr.a_text;

		}
		memcpy(symout.n_name, sp->name, sizeof(symout.n_name));
		symout.n_type = sp->type;
		symout.n_value = sp->value;
		d = write(ofile, &symout, sizeof(symout));
	}
}
