/*
 *		Assembler input & lexical scan routines
 */

#include "as.h"
#include <math.h>

char*	getstr();
int	getsym();
int	getnum(int base);

/*
 * Parser information
 */

char	linebuf[128];	/* input line buffer */
int	line;		/* line counter */

int	nexttoken;	/* look-ahead token */
char	strbuf[128];	/* returned value of last string */
long	conbuf;		/* returned value of last constant */
int	strsiz;		/* returned length of last string */


/*
 * Character types
 */
#define	C_ALPHA		0x80	/* Alphabetic (i.e. legal within symbol) */
#define	C_HEXDIGIT	0x40	/* Hexadecimal digit [0-9 a-f A-F] */
#define C_DIGIT		0x20	/* Decimal digit [0-9] */
#define	C_TOKEN		0x10	/* Other special characters */
#define	C_VALUE		0x0f	/* C_HEXDIGIT - numerical value of digit */

/*
 * Character type table
 */
unsigned char ctab[128] = {
	0x10,	0,	0,	0,	0,	0,	0,	0,
	0,	0x10,	0x10,	0,	0,	0x10,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0x10,	0x10,	0x10,	0,	0x10,	0,	0x10,	0x10,
	0x10,	0x10,	0x10,	0x10,	0x10,	0x10,	0x80,	0x10,
	0x60,	0x61,	0x62,	0x63,	0x64,	0x65,	0x66,	0x67,
	0x68,	0x69,	0x10,	0x10,	0x10,	0x10,	0x10,	0,
	0x10,	0xca,	0xcb,	0xcc,	0xcd,	0xce,	0xcf,	0x80,
	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,
	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,
	0x80,	0x80,	0x80,	0,	0,	0,	0,	0x80,
	0,	0xca,	0xcb,	0xcc,	0xcd,	0xce,	0xcf,	0x80,
	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,
	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,	0x80,
	0x80,	0x80,	0x80,	0,	0x10,	0,	0x80,	0
};

// TODO: minimize... (no ctrl chars in HW200)
int xlate_c(c)
	int c;
{
	switch (c) {
	case 'a': return '\a';
	case 'b': return '\b';
	case 'f': return '\f';
	case 'n': return '\n';
	case 'r': return '\r';
	case 't': return '\t';
	case 'v': return '\v';
	case '0': return '\0';
	default:
		return c;
	}
}

/*
 * Read next input line, truncating to 71 chars
 *	- resets scan pointer to start of line
 *	- initializes listing buffer
 *	- returns 0 at end-of-file, '\n' otherwise
 */

char *scanp;				/* input scan cursor */
int   docount;

static char	ibuf[512];
static char	*next;
static int	nleft;

int get_line()
{
	char *nx, *p;
	int   nl, ncol;

	scanp = p = linebuf;
	lstinit();

	ncol = 71;
	for (nx = next, nl = nleft; ;) {
		if (--nl <= 0) {
			while ((nl = read(fd_in, ibuf, 512)) <= 0)
				if (!nextfile()) {
					nleft = 0;
					return(0);
				}
			nx = ibuf;
		}
		if ((*p = *nx++) == '\n') {
			*p    = '\0';
			next  = nx;
			nleft = nl;
			line++;
			return('\n');
		}
		if (--ncol > 0)
			p++;
	}
}

/*
 * Return next lexical token from input line
 *	- if constant, value is in conbuf
 *	- if symbol & lookup flag is true, symbol pointer is in cursym
 */
int tok(lookup)
	int lookup;
{
	int ret, c;
	int ctype;
	char *p;

	/* first check look-ahead token */
	if ((ret = nexttoken)) {
		nexttoken = 0;
		return (ret);
	}

	/* Skip whitespace */
	p = scanp;
	while ((c = *p) == ' ' || c == '\t') p++;
	scanp = p;
  
	for(;;) {

		p = scanp;
		c = *p;
		ctype = ctab[c];

		/* first character "alphabetic" */
		if ( ctype & C_ALPHA) {
			if (c=='.' && !(ctab[(int)*(p+1)] & C_ALPHA) ) {
				++scanp;
				return (DOT);
			}
			getsym();
			if (lookup)
				symlook(2);
			if (*scanp == COLON) {
				++scanp;
				if (*scanp == COLON) {
					++scanp;
				}
				return (LABEL);
			} else {
				return (IDENT);
			}
		}
		/*
		 * first character numeric -- numeric constant or label
		 */
		if (ctype & C_DIGIT) {
			if (c == '0') {
				if (p[1]=='x') {
					scanp = p+2;
					conbuf = getnum(16);
				} else if (p[1]=='b') {
					scanp = p+2;
					conbuf = getnum(2);
				} else {
					scanp = p;
					conbuf = getnum(8);
					goto testnlab;
				}
			} else {
				conbuf = getnum(10);
testnlab:
				if (conbuf>=0 && conbuf<=9) {
					c = *scanp;
					switch(c) {
					case 'b':
						conbuf = -conbuf;
					case 'f':
						++scanp;
						return(NIDENT);
					case COLON:
						++scanp;
						return(NLABEL);
					}
				}
			}
			return(CON);
		}
		/*
		 * first character QUOTE
		 */
		if (c == QUOTE) {
			c = *++p;
			conbuf = (c == '\\') ? xlate_c(*++p) : c;
			if (p[1] != QUOTE) {
				conbuf <<= 8;
				c = *++p;
				conbuf += (c == '\\') ? xlate_c(*++p) : c;
			}
			if (*++p != QUOTE)
				xerror(errx);
			scanp = p + 1;
			return(CON);
		}
		/*
		 * first character DQUOTE
		 */
		if (c == DQUOTE) {
			scanp = p+1;
			conbuf = (long) getstr();
			return (STRING);
		}

		/* special characters */
		scanp = ++p;
		if (!ctype)
			xerror(errg);

		switch (c) {

			/* white space */
			case ' ':
			case '\t':
				while ((c = *p) == ' ' || c == '\t')
					p++;
				scanp = p;
				continue;

			case '/':
				if (*p=='/') {
					while (*p++);
					scanp = --p;
					return (COMMENT);
				} else
					return (c);

			/* don't scan past end of line */
			case 0:
			case '\n':
			case '\r':
				scanp = --p;
				return (EOL);

			/* single-character tokens */
			default:
				return (c);

		}
	}
}

int token()
{
	return tok(1);
}

static int isodigit(int c) {
	return (c >= '0' && c <= '7');
}

static int isbdigit(int c) {
	return (c == '0' || c == '1');
}

static int cvxdigit(int c) {
	if (c <= '9') {
		return (c - '0');
	} else {
		return (c - '7');
	}
}
static int cvodigit(int c) {
	return (c - '0');
}
static int cvbdigit(int c) {
	return (c - '0');
}

void check_punc(int *pnc) {
	int t, c;
	while (*scanp == ' ' || *scanp == '\t') ++scanp;
	if (!isalpha(*scanp) || scanp[1] != COLON) {
		return;
	}
	c = punct[toupper(*scanp) - 'A'];
	if (c < 0) {
		return;
	}
	scanp += 2; // skip COLON
	*pnc = c;
	while (*scanp == ' ' || *scanp == '\t') ++scanp;
}

static char *field_width(char *p, int *w) {
	char *e;
	unsigned long n;
	if (*p != '#') {
		return p;
	}
	n = strtoul(++p, &e, 10);
	if (p == e || n == 0) {
		cerror(errv);
	}
	*w = n;
	return e;
}

int scanit(int pnc, int (*isdig)(int), int (*cvdig)(int), int bpd) {
	int c = 0;
	int v = 0, vv;
	int n = 0;
	int w;
	char *p = scanp, *e;
	while (isdig(*p)) ++p;
	if (scanp == p) {
		xerror(errv);
	}
	c = (p - scanp) * bpd; // num bits to scan
	w = (c + 5) / 6;
	e = field_width(p, &w);
	// TODO: check termination
	// guarantee we end on 6-bit
	if (c > w * 6) { // truncate constant
		scanp += (c - w * 6) / bpd;
		c = 0;
	} else if (c < w * 6) { // left-fill
		c = w * 6 - c;
		while (c >= 6) {
			c -= 6;
			putb(0, 1);
			++n;
		}
	} else {
		c = 0;
	}
	while (scanp < p) {
		v <<= bpd;
		v |= cvdig(*scanp++);
		c += bpd;
		if (c >= 6) {
			c -= 6;
			vv = ((v >> c) & 077);
			if (!n) {
				vv |= (pnc >> 8);
			}
			if (scanp == p) {
				vv |= (pnc & 0377);
			}
			putb(vv, 1);
			++n;
		}
	}
	scanp = e;
	return token();
}

// scan an arbitrary-length binary number, in hex
int scanhex(int pnc) {
	return scanit(pnc, isxdigit, cvxdigit, 4);
}

// scan an arbitrary-length binary number, in octal
int scanoct(int pnc) {
	return scanit(pnc, isodigit, cvodigit, 3);
}

// scan an arbitrary-length binary number, in binary
int scanbin(int pnc) {
	return scanit(pnc, isbdigit, cvbdigit, 1);
}

// scan an arbitrary-length binary number, in decimal
// TODO: how to control field size? Leading zeroes not allowed.
int scandec(int pnc) {
	// TODO: how to do this?
#if 0
	char *p = scanp;
	while (isdigit(*p)) ++p;
	if (scanp == p) {
#endif
		xerror(errv);
#if 0
	}
	c = ((p - scanp) + 1) / 2; // num bytes
	v = malloc(c);	// [0] is LSD
	memset(v, 0, c);
	m = 0;	// max used bit
	while (scanp < p) {
		mult10(v, c, &m);
		add(v, c, *scanp++ - '0', &m);
	}
	for (;;) {
		putb(vv, 1);
	}
	return token();
#endif
}

int scan_bin(int pnc) {
	if (*scanp == '0') {
		if (scanp[1] == 'x') {
			scanp += 2;
			return scanhex(pnc);
		} else if (scanp[1] == 'b') {
			scanp += 2;
			return scanbin(pnc);
		} else {
			return scanoct(pnc);
		}
	} else {
		return scandec(pnc);
	}
}

// scan a floating-point number
int scanfp(int pnc) {
	char *e;
	char *p = --scanp; // backup to 't'
	double d;
	int x;
	long long m, h;
	int ms;

	d = strtod(p, &e);
	if (e == p) {
		xerror(errv);
	}
	// TODO: check end...
	scanp = e;
	x = ilogb(d);			// exponent
	m = (long long)significand(d);	// mantissa
	if (m == 0) {
		h = 0;
	} else {
		ms = (m < 0);
		if (ms) {
			m = -m;
		}
		// TODO: 'ms'
		h = (m << 12) | (x & 0xfff);
	}
	for (x = 42; x >= 0; x -= 6) {
		ms = (h >> x) & 077;
		if (x == 42) {
			ms |= (pnc >> 8);
		}
		if (x == 0) {
			ms |= (pnc & 0377);
		}
		putb(ms, 1);
	}
	return token();
}

int scanbcd(int pnc) {
	int v, c;
	if (*scanp == PLUS) {
		++scanp;
	} else if (*scanp == MINUS) {
		++scanp;
		pnc |= NEG;
	}
	char *p = scanp;
	while (isdigit(*p)) ++p;
	if (scanp == p) {
		cerror(errv);
	}
	// TODO: check termination
	c = 0;
	while (scanp < p) {
		v = *scanp++ - '0';
		if (!c) {
			v |= (pnc >> 8);
		}
		if (scanp == p) {
			v |= (pnc & 0377);
		}
		++c;
		putb(v, 1);
	}
	return token();
}

int scanstr(int pnc) {
	char *s;
	if (*scanp != DQUOTE) {
		cerror(errv);
	}
	++scanp;
	s = getstr();
	putstrhw(s, pnc);
	return token();
}

/*
 * Scan a decimal or hexadecimal number (with optional sign)
 *	 from input line
 */
int getnum(base)
	int base;
{
	char	*p;
	int		c, n, sign;

	p = scanp;
	sign = 1;
	if ((c = *p) == '-') {
		sign = -1;
		c = *++p;
	}
	else if (c == '+')
		c = *++p;
	if (((c = ctab[c])&C_HEXDIGIT) == 0 || (n = c&C_VALUE) >= base)
		xerror(errn);
	while ((c = ctab[(int)*++p])&C_HEXDIGIT && (c &= C_VALUE) < base) {
		n *= base;
		n += c;
	}
	scanp = p;
	return (n*sign);
}

/*
 * Scan a character string from input line until ending quote
 *	- translate pairs of quotes to single quote
 *	- leave string in strbuf, and length in strlen
 * Returns a pointer to strbuf
 */

char* getstr()
{
	char	*s, *p, *e = strbuf + sizeof(strbuf) - 1;
	int	c;

	s = strbuf;
	for (p = scanp; s < e; p++) {
		if ((c = *p) == '\\') {
			c = xlate_c( *++p );
		}
		else if (c == '"')
			break;
		else if (c == '\n')
			xerror(errq);
		*s++ = c;
	}
	if (c != '"') xerror(errq);
	*s = 0;
	strsiz = s - strbuf;
	scanp = p + 1;
	return (strbuf);
}

/*
 * Scan a symbol from the input line into symbuf
 *	truncate long symbols to 8 characters (without warning)
 *
 *	returns	0:	no legal symbol found
 *		1:	symbol found
 */

int	ucase = 0;

int getsym()
{
	int c, ctype;
	char *p, *s;

	p = scanp;
	c = *p;
	ctype = ctab[c];
	if (!(ctype & C_ALPHA))
		return(0);

	s = symbuf;
	s[0] = s[1] = s[2] = s[3] = 0;
	s[4] = s[5] = s[6] = s[7] = 0;
	do {
		if (s < &symbuf[8]) {
			if (ucase && (c>='A' && c<='Z'))
				c += 'a'-'A';
			*s++ = c;
		}
		c = *++p;
		ctype = ctab[c];
	} while ( ctype & (C_ALPHA|C_DIGIT) );

	scanp = p;
	// this doesn't belong here?
	symrev = (*scanp == COLON && scanp[1] == COLON);
	return(1);
}

/*
 * Determine whether scan pointer is at end of line
 */
int eol()
{
	int c;

	c = ctab[(int)*scanp];
	return( c == EOL || c == SPACE );
}

/*
 * Save / restore scan pointer (for backtracking)
 */

static char	*scansave;
static int   nxt_save;

void sscan()
{
	nxt_save = nexttoken;
	scansave = scanp;
}

void rscan()
{
	nexttoken = nxt_save;
	scanp = scansave;
}
