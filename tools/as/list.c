/*
 *		Assembler listing routines
 */

#include "as.h"

// TODO: include punctuation in listing

/* buffer for listing */
struct {
	char	l_loc[7];		/* location counter */
	char	l_fill1;		/* space */
	char	l_data[18];		/* location contents */
	char	l_ovfl;			// overflow
	char	l_fill2;		/* space */
	char	l_punc[2];		// punctuation
	char	l_fill3[2];		/* space */
	char	l_term;		/* null (followed by source line) */
} lstbuf;

char		*listp;			/* current output position in l_data */
extern char	linebuf[];		/* source line */


/* initialize listing buffer */
void lstinit()
{
	char	*p;
	int	n;

	memset(&lstbuf, ' ', sizeof(lstbuf));
	lstbuf.l_term = 0;
	listp = &lstbuf.l_data[0];
}

// TODO: how to do this for multiple operands?
static void lstreloc(int rel) {
	switch (rel & RSEG) {
	case RABS:	lstbuf.l_fill3[1] = 'a'; break;
	case RTEXT:	lstbuf.l_fill3[1] = 't'; break;
	case RDATA:	lstbuf.l_fill3[1] = 'd'; break;
	case RBSS: 	lstbuf.l_fill3[1] = 'b'; break;
	case REXT:	lstbuf.l_fill3[1] = 'U'; break;
	}
}

// TODO: how/when to pass punctuation?
static void lstpunc(int pnc, int idx) {
	switch (pnc >> 6) {
	case 1:
		lstbuf.l_punc[idx] = 'w';
		break;
	case 2:
		lstbuf.l_punc[idx] = 'i';
		break;
	case 3:
		lstbuf.l_punc[idx] = 'r';
		break;
	}
}

/* list number in octal with leading zeroes */
void lstx(value, nibbles, buf, rel)
	int value, nibbles, rel;
	char *buf;
{
	char	*p;

	for (p = buf+nibbles; --p >= buf; ) {
		*p = '0' + (value & 07);
		value >>= 3;
	}
}

/* list a char of data */
void lstb(int val, int rel) {
	int w = 2;
	int n = &lstbuf.l_data[sizeof(lstbuf.l_data)] - listp; // remaining space
	if (w > n) {
		w = n;
		val >>= (n * 3);
		lstbuf.l_ovfl = '>';
	}
	if (w) {
		lstx(val, w, listp, rel);
		listp += w;
	}
}

/* list an address of code/data */
void lstaddr(uint32_t val, int rel) {
	int w = admode * 2;
	int n = &lstbuf.l_data[sizeof(lstbuf.l_data)] - listp; // remaining space
	if (w > n) {
		w = n;
		val >>= (n * 3);
		lstbuf.l_ovfl = '>';
	}
	if (w) {
		lstx(val, w, listp, rel);
		listp += w;
	}
}

#include <unistd.h>

/* print out listing line */
void putline(int pnc) {
	if (pass_lst) {
		if (listp != lstbuf.l_data) { // is this a good test?
			lstpunc(pnc >> 8, 0);
			lstpunc(pnc & 0300, 1);
		}
		printf("%s", (char *)&lstbuf);
		printf(" %05d ", line);
		printf(" %s\n", linebuf);
	}
}

/* list location counter value */
void lstloc()
{
	int loc;
	
	loc = reloc(curseg->loc, currel);
	lstx(loc, 7, lstbuf.l_loc, currel);
}

/* list EQU statements */
void lstequ(int rel, int val)
{
	int loc;

	loc = reloc(val, rel);
	lstx(loc, 7, lstbuf.l_loc, currel);
	lstbuf.l_data[0] = '=';
}
