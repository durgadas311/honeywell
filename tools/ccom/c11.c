/*
 * C compiler
 */
#include "c1.h"

int getwd();

int
degree(t)
register union tree *t;
{
	register union tree *t1;

	if (t==NULL || t->t.op==0)
		return(0);
	if (t->t.op == CON)
		return(-3);
	if (t->t.op == AMPER)
		return(-2);
	if (t->t.op==ITOL) {
		if ((t1 = isconstant(t)) && (t1->c.value>=0 || uns(t1)))
			return(-2);
		if (uns(t1 = t->t.tr1) && opdope[t1->t.op]&LEAF)
			return(-1);
	}
	if ((opdope[t->t.op] & LEAF) != 0) {
		if (t->t.type==CHAR || t->t.type==UNCHAR || t->t.type==FLOAT)
			return(1);
		return(0);
	}
	return(t->t.degree);
}

static char *curfnc = NULL;
extern void prconlab(int lab, int flag);

void
pname(p, flag, neg)
register union tree *p;
int flag, neg;
{
	register int i;
	long ltmp;

loop:
	switch(p->t.op) {

	case LCON:
		prconlab(p->l.label, neg);
		return;

	case SFCON:
	case CON:
	case CCON:
		prconlab(p->c.label, 0);
		return;

	case FCON:
		prconlab(p->f.label, neg);
		return;

	case CALL1:
		printf("x5");
		return;

	case NAME:
		i = p->n.offset;
		if (flag>10)
			i += SZPTR;
		if (i) {
			psoct(i);
			if (p->n.class!=OFFS) {
				// This will never work?
				// e.g.		lca	40+.buf,0(x1)
				error("Illegal use of offset (i) NAME.x");
				putchar('+');
				neg = 1;
			}
			if (p->n.class==REG)
				error("Illegal use of register (i) NAME.REG");
		}
		switch(p->n.class) {

		case SOFFS:
		case XOFFS:
			pbase(p, neg);

		case OFFS:
			if( p->n.regno==BPREG )
				printf("(x2)");
			else
				error("Illegal use of offset");
			return;

		case EXTERN:
		case STATIC:
			pbase(p, neg);
			return;

		case REG:
			if( p->n.nloc==BPREG ) {
				printf("x2");
			} else {
				error("Illegal use of reg NAME.REG");
			}
			return;

		}
		error("Compiler error: pname");
		return;

	case STAR:
		p = p->t.tr1;
		neg = 0;	// need addr-of ptr variable?
		goto loop;

	case AMPER:
		p = p->t.tr1;
#if 0	// is this still relevant?
		if (p->t.op==NAME && p->n.class==REG) {
			error("Illegal use of register AMPER.NAME.REG");
		}
#endif
		neg &= ~1;	// need addr-of variable
		goto loop;

	case AUTOI:
		error("Illegal use of auto-incr");
		return;

	}
dumptree(p, 0, 'e');
	error("compiler error: bad pname");
}

void
pbase(p, f)
register union tree *p;
int f;
{
	char *s;
	if (p->n.class==SOFFS || p->n.class==STATIC) {
		prconlab(p->n.nloc, f);
	} else {
		s = p->x.name;
		if (f) {
			if (*s == '_') ++s;
			printf("^%s", s);
		} else {
			printf("%s", s);
		}
	}
}

int
xdcalc(p, nrleft)
register union tree *p;
int nrleft;
{
	register int d;

	if (p==NULL)
		return(0);
	d = dcalc(p, nrleft);
	if (d < DREG && (p->t.type == CHAR || p->t.type == UNCHAR)) {
		if (nrleft>=1)
			d = DREG;
		else
			d = DNRG;
	}
	return(d);
}

int
dcalc(p, nrleft)
register union tree *p;
int nrleft;
{
	int c;

	if (p==NULL)
		return(0);
	switch (p->t.op) {

	case NAME:
		if (p->n.class==REG && p->n.type!=CHAR && p->n.type!=UNCHAR)
			return(DCHR);
		return(DADR);

	case AMPER:
		return(DADR);

	case FCON:
	case AUTOI:
		return(DADR);

	case LCON:
	case SFCON:
	case CON:
		if (p->c.value==0)
			return(DZER);
		if (p->c.value==1)
			return(DONE);
		if (p->c.value==2)
			return(DTWO);
		return(DCON);
	case CCON:
		return(DCON);

	}
	if (p->t.type==LONG || p->t.type==UNLONG)
		nrleft--;
	// nrleft = num regs left (available)
	return(p->t.degree <= nrleft? DREG: DNRG);
}

int
notcompat(p, ast, deg, op)
register union tree *p;
int ast, deg, op;
{
	unsigned register at, st;

	at = p->t.type;
	/*
	 * an e or n UNCHAR is to be considered an UNSIGNED,
	 * as long as it is not pointed to.
	 */
	if (at == UNCHAR && deg < DPTR && deg >= DREG) {
		at = UNSIGN;
	}
	st = ast;
	if (st == 0) {		/* word, byte */
		return(at != CHAR && at != INT && at != UNSIGN && at < PTR);
	}
	if (st == 1) {		/* word */
		return(at != INT && at != UNSIGN && at < PTR);
	}
	if (st == UNSIGN + 2 && (at & XTYPE)) {
		return(0);
	}
	st -= 2;
	if ((at & ~(TYPE + XTYPE)) != 0) {
		at = INT | PTR;
	}
	if ((at & ~TYPE) != 0) {
		at = (at & TYPE) | PTR;
	}
	if (st == FLOAT && at == DOUBLE) {
		at = FLOAT;
	}
	if (p->t.op == NAME && p->n.class == REG && op == ASSIGN && st == CHAR) {
		return(0);
	}
	return(st != at);
}

int
prins(op, c, itable, tab)
int op, c, tab;
struct instab *itable;
{
	register struct instab	*insp;
	register char	*ip;

	for (insp = itable; insp->iop != 0; insp++) {
		if (insp->iop == op) {
			ip = c ? insp->str2: insp->str1;
			if (!ip)
				break;
			if (!tab) putchar('\t');
			if (*ip == '@') {
				printf("lcr\t%s,064\n\tcsm", ip);
			} else {
				fputs(ip, stdout);
			}
			return(0);
		}
	}
	error("No match for op %d", op);
 	return(0);
}

int
collcon(p)
register union tree *p;
{
	register int op;

	if (p==NULL)
		return(0);
	if (p->t.op==PLUS) {
		op = p->t.tr2->t.op;
		if (op==CON || op==AMPER)
			return(1);
	}
	return(0);
}

int
isfloat(t)
register union tree *t;
{

	if ((opdope[t->t.op]&RELAT)!=0)
		t = t->t.tr1;
	if (t->t.type==FLOAT || t->t.type==DOUBLE) {
		nfloat = 1;
		return('f');
	}
	return(0);
}

int
oddreg(t, reg)
register union tree *t;
register int reg;
{

	// Unclear when this undo is needed. If we adjust here
	// for *MOD, we still use the wrong reg (X5).
	// It seems the next expression will revert to X5 anyway.
	if (!isfloat(t)) {
#if 0
		if (opdope[t->t.op]&RELAT) {
			if (t->t.tr1->t.type==LONG || t->t.tr1->t.type==UNLONG)
				return((reg+1) & ~01);
			return(reg);
		}
#endif
#if 0
		switch(t->t.op) {

		case MOD:
		case ASMOD:
		case UMOD:
		case ASUMOD:
			reg--;
			break;
		}
#endif
	}
	return(reg);
}

// This size of a type in an argument
int
arlength(t)
int t;
{
	if (t>=PTR)
		return(SZPTR);
	switch(t) {

	case INT:
	case CHAR:
	case UNSIGN:
	case UNCHAR:
		return(SZINT);

	case UNLONG:
	case LONG:
		return(SZLONG);

	case FLOAT:
	case DOUBLE:
		return(SZDOUB);
	}
	error("botch: peculiar type %d", t);
	return(1024);
}

// This absolute size of a type
int
ellength(t)
int t;
{
	if (t>=PTR)
		return(SZPTR);
	switch(t) {

	case INT:
	case UNSIGN:
		return(SZINT);
	case CHAR:
	case UNCHAR:
		return(SZCHAR);

	case UNLONG:
	case LONG:
		return(SZLONG);

	case FLOAT:
	case DOUBLE:
		return(SZDOUB);
	}
	error("botch: peculiar type %d", t);
	return(1024);
}

/*
 * Strings for switch code.
 */

/*
 * Modified Memorial day May 80 to uniquely identify switch tables
 * (as on Vax) so a shell script can optionally include them in RO code.
 * This is useful in overlays to reduce the size of data space load.
 * wfj 5/80
 */
char	dirsw[] = {
	"ci	r2,%d\n"
	"bjh	L%d\n"
	"sla	r2,1\n"
	"mov	@L%d(r2),r0\n"
	"b	(r0)\n"
	"\t.data\n"
	"L%d:"
};

char	hashsw[] = {
	"mov	r2,r3\n"
	"clr	r2\n"
	"li	r0,%d\n"
	"div	r0,r2\n"
	"sla	r3,1\n"
	"mov	@L%d(r3),r0\n"
	"b	(r0)\n"
	"\t.data\n"
	"L%d:"
};

/*
 * If the unsigned casts below won't compile,
 * try using the calls to lrem and ldiv.
 */

void
pswitch(afp, alp, deflab)
struct swtab *afp, *alp;
int deflab;
{
	int ncase, i, j, tabs = 0, worst, best, range;
	register struct swtab *swp, *fp, *lp;
	int *poctab;

	fp = afp;
	lp = alp;
	if (fp==lp) {
		printf("\tb\tL%d\n", deflab);
		return;
	}
	isn++;
	if (sort(fp, lp))
		return;
	ncase = lp-fp;
	lp--;
	range = lp->swval - fp->swval;
	/* direct switch */
	if (range>0 && range <= 3*ncase) {
		if (fp->swval)
			printf("ai\tr2,-%d\n", UNS(fp->swval));
		printf(dirsw, UNS(range), deflab, isn, isn);
		isn++;
		for (i=fp->swval; ; i++) {
			if (i==fp->swval) {
				printf("L%d\n", fp->swlab);
				if (fp==lp)
					break;
				fp++;
			} else
				printf("L%d\n", deflab);
		}
		printf("\t.text\n");
		return;
	}
	/* simple switch */
	if (ncase<10) {
		for (fp = afp; fp<=lp; fp++)
			breq(fp->swval, fp->swlab);
		printf("\tb\tL%d\n", deflab);
		return;
	}
	/* hash switch */
	best = 077777;
	poctab = (int *)getblk(((ncase+2)/2) * sizeof(*poctab));
	for (i=ncase/4; i<=ncase/2; i++) {
		for (j=0; j<i; j++)
			poctab[j] = 0;
		for (swp=fp; swp<=lp; swp++)
			/* lrem(0, swp->swval, i) */
			poctab[(unsigned)swp->swval%i]++;
		worst = 0;
		for (j=0; j<i; j++)
			if (poctab[j]>worst)
				worst = poctab[j];
		if (i*worst < best) {
			tabs = i;
			best = i*worst;
		}
	}
	i = isn++;
	printf(hashsw, UNS(tabs), i, i);
	isn++;
	for (i=0; i<tabs; i++)
		printf("L%d\n", isn+i);
	printf(".text\n");
	for (i=0; i<tabs; i++) {
		printf("L%d:", isn++);
		for (swp=fp; swp<=lp; swp++) {
			/* lrem(0, swp->swval, tabs) */
			if ((unsigned)swp->swval%tabs == i) {
				/* ldiv(0, swp->swval, tabs) */
				breq((int)((unsigned)swp->swval/tabs), swp->swlab);
			}
		}
		printf("\tb\tL%d\n", deflab);
	}
}

void
breq(v, l)
int v, l;
{
	if (v==0)
		printf("\tmov\tr2,r2\n");
	else
		printf("\tci\tr2,%d\n", UNS(v));
	printf("\tbjeq\tL%d\n", l);
}

int
sort(afp, alp)
struct swtab *afp, *alp;
{
	register struct swtab *cp, *fp, *lp;
	int intch, t;

	fp = afp;
	lp = alp;
	while (fp < --lp) {
		intch = 0;
		for (cp=fp; cp<lp; cp++) {
			if (cp->swval == cp[1].swval) {
				error("Duplicate case (%d)", cp->swval);
				return(1);
			}
			if (cp->swval > cp[1].swval) {
				intch++;
				t = cp->swval;
				cp->swval = cp[1].swval;
				cp[1].swval = t;
				t = cp->swlab;
				cp->swlab = cp[1].swlab;
				cp[1].swlab = t;
			}
		}
		if (intch==0)
			break;
	}
	return(0);
}

int
ispow2(tree)
register union tree *tree;
{
	register int d;

	if (!isfloat(tree) && tree->t.tr2->t.op==CON) {
		d = tree->t.tr2->c.value;
		if (d>1 && (d&(d-1))==0)
			return(d);
	}
	return(0);
}

union tree *
pow2(tree)
register union tree *tree;
{
	register int d, i;

	if ( (d = ispow2(tree)) ) {
		for (i=0; (d>>=1)!=0; i++);
		// need to reference a different constant!
		// can't just change value...
		tree->t.tr2 = tconst(i, tree->t.tr2->t.type, 0);
		switch (tree->t.op) {

		case TIMES:
			tree->t.op = LSHIFT;
			break;

		case ASTIMES:
			tree->t.op = ASLSH;
			break;

		case PTOI:
			if (i==1 && tree->t.tr1->t.op==MINUS && !isconstant(tree->t.tr1->t.tr2)) {
				tree->t.op = PTOI1;
				tree->t.tr1 = tnode(LTOI, INT, tree->t.tr1, TNULL);
				return(optim(tree));
			}
			tree->t.op = RSHIFT;
			tree->t.tr2->c.value = i;
			i = tree->t.type;
			tree->t.type = LONG;
			tree = tnode(LTOI, i, tree, TNULL);
			break;

		case DIVIDE:
			tree->t.op = URSH;
			tree->t.tr2->c.value = i;
			break;

		case ASDIV:
			tree->t.op = ASURSH;
			tree->t.tr2->c.value = i;
			break;

		case MOD:
			tree->t.op = AND;
			tree->t.tr2->c.value = (1<<i)-1;
			break;

		case ASMOD:
			tree->t.op = ASAND;
			tree->t.tr2->c.value = (1<<i)-1;
			break;

		default:
			error("pow2 botch");
		}
		tree = optim(tree);
	}
	return(tree);
}

void
cbranch(atree, lbl, cond, reg)
union tree *atree;
register int lbl, reg, cond;
{
	int l1, op;
	register union tree *tree;

again:
	if ((tree=atree)==NULL)
		return;
	switch(tree->t.op) {

	case LOGAND:
		if (cond) {
			cbranch(tree->t.tr1, l1=isn++, 0, reg);
			cbranch(tree->t.tr2, lbl, 1, reg);
			label(l1);
		} else {
			cbranch(tree->t.tr1, lbl, 0, reg);
			cbranch(tree->t.tr2, lbl, 0, reg);
		}
		return;

	case LOGOR:
		if (cond) {
			cbranch(tree->t.tr1, lbl, 1, reg);
			cbranch(tree->t.tr2, lbl, 1, reg);
		} else {
			cbranch(tree->t.tr1, l1=isn++, 1, reg);
			cbranch(tree->t.tr2, lbl, 0, reg);
			label(l1);
		}
		return;

	case EXCLA:
		cbranch(tree->t.tr1, lbl, !cond, reg);
		return;

	case SEQNC:
		rcexpr(tree->t.tr1, efftab, reg);
		atree = tree->t.tr2;
		goto again;

	case ITOL:
		tree = tree->t.tr1;
		break;

	case QUEST:
		l1 = isn;
		isn += 2;
		cbranch(tree->t.tr1, l1, 0, reg);
		cbranch(tree->t.tr2->t.tr1, lbl, cond, reg);
		branch(l1+1, 0, 0);
		label(l1);
		cbranch(tree->t.tr2->t.tr2, lbl, cond, reg);
		label(l1+1);
		return;

	}
	op = tree->t.op;
	if (opdope[op]&RELAT
	 && tree->t.tr1->t.op==ITOL && tree->t.tr2->t.op==ITOL
	 && uns(tree->t.tr1->t.tr1) == uns(tree->t.tr2->t.tr1)) {
		tree->t.tr1 = tree->t.tr1->t.tr1;
		tree->t.tr2 = tree->t.tr2->t.tr1;
		if (op>=LESSEQ && op<=GREAT
		 && uns(tree->t.tr1))
			tree->t.op = op = op+LESSEQP-LESSEQ;
	}
	if (tree->t.type==LONG || tree->t.type==UNLONG
	  || (opdope[op]&RELAT&&(tree->t.tr1->t.type==LONG || tree->t.tr1->t.type==UNLONG))) {
		longrel(tree, lbl, cond, reg);
		return;
	}
	rcexpr(tree, cctab, reg);
	op = tree->t.op;
	if ((opdope[op]&RELAT)==0)
		op = NEQUAL;
	if (isfloat(tree))
		printf("cfcc\n");
	branch(lbl, op, !cond);
}

void
branch(lbl, aop, c)
int lbl, aop, c;
{
	register int op;
	struct boptab *bt;

	if (aop) {
		op = 040; // do not branch
		for (bt = branchtab; bt->iop != 0; ++bt) {
			if (bt->iop == aop) {
				if (c) {
					op = bt->ropc;
				} else {
					op = bt->opc;
				}
				break;
			}
		}
		printf("\tbct\tL%d,0%o\n", lbl, op);
	} else {
		printf("\tb\tL%d\n", lbl);
	}
}

void
longrel(atree, lbl, cond, reg)
union tree *atree;
int lbl, cond, reg;
{
	int xl1, xl2, xo, xz;
	register int op, isrel;
	register union tree *tree;

	if (reg&01)
		reg++;
	reorder(&atree, cctab, reg);
	tree = atree;
	isrel = 0;
	if (opdope[tree->t.op]&RELAT) {
		isrel++;
		op = tree->t.op;
	} else
		op = NEQUAL;
	if (!cond)
		op = notrel[op-EQUAL];
	xl1 = xlab1;
	xl2 = xlab2;
	xo = xop;
	xlab1 = lbl;
	xlab2 = 0;
	xop = op;
	xz = xzero;
	xzero = !isrel || (tree->t.tr2->t.op==ITOL && tree->t.tr2->t.tr1->t.op==CON
		&& tree->t.tr2->t.tr1->c.value==0);
	if (cexpr(tree, cctab, reg) < 0) {
		reg = rcexpr(tree, regtab, reg);
		printf("mov\tr%d,r0\njne\t.+4\nmov\tr%d,r0\n", reg, reg+1);
		branch(xlab1, op, 0);
	}
	xlab1 = xl1;
	xlab2 = xl2;
	xop = xo;
	xzero = xz;
}

/*
 * Tables for finding out how best to do long comparisons.
 * First dimen is whether or not the comparison is with 0.
 * Second is which test: e.g. a>b->
 *	cmp	a,b
 *	bgt	YES		(first)
 *	blt	NO		(second)
 *	cmp	a+2,b+2
 *	bhi	YES		(third)
 *  NO:	...
 * Note some tests may not be needed.
 *
 * EQUAL = 60
 * NEQUAL= 61
 * LESSEQ= 62
 * LESS  = 63
 * GREATEQ=64
 * GREAT  =65
 * LESSEQP=66
 * LESSP  =67
 * GREATQP=68
 * GREATP =69
 *
 * Third dimension (lrtab[][][x]) indexed by "x - EQUAL".
 */
char	lrtab[2][3][10] = {
    {
	{ 0,      NEQUAL, LESS,    LESS,  GREAT,   GREAT,  LESSP,   LESSP,  GREATP,  GREATP  },
	{ NEQUAL, 0,      GREAT,   GREAT, LESS,    LESS,   GREATP,  GREATP, LESSP,   LESSP },
	{ EQUAL,  NEQUAL, LESSEQP, LESSP, GREATQP, GREATP, LESSEQP, LESSP,  GREATQP, GREATP },
    }, {
	{ 0,      NEQUAL, LESS,  LESS, GREATEQ, GREAT,  LESSP,  LESSP, GREATQP, GREATP },
	{ NEQUAL, 0,      GREAT, 0,    0,       LESS,   GREATP, 0,     0,       LESSP  },
	{ EQUAL,  NEQUAL, EQUAL, 0,    0,       NEQUAL, EQUAL,  0,     0,       NEQUAL }
    }
};

int
xlongrel(f)
int f;
{
	register int op, bno;

	op = xop;
	if (f==0) {
		if ( (bno = lrtab[xzero][0][op-EQUAL]) )
			branch(xlab1, bno, 0);
		if ( (bno = lrtab[xzero][1][op-EQUAL]) ) {
			xlab2 = isn++;
			branch(xlab2, bno, 0);
		}
		if (lrtab[xzero][2][op-EQUAL]==0)
			return(1);
	} else {
		branch(xlab1, lrtab[xzero][2][op-EQUAL], 0);
		if (xlab2)
			label(xlab2);
	}
	return(0);
}

void
label(l)
int l;
{
	prconlab(l, 0);
	putchar(':');
}

static void adjstk(char *op, union tree *sz, char *f) {
	printf("\t%s\t", op);
	if (sz->t.op == LABEL) {
		prconlab(-sz->c.label, 0);
	} else if (sz->t.op == NAME) {
		if (sz->n.class == EXTERN) {
			printf("%s", sz->x.name);
		} else {
			prconlab(sz->n.nloc, 0);
		}
	} else if (sz->t.op == CON) {
		prconlab(sz->c.label, 0);
	} else {
		printf("0");
		error("Stack adjust reference botch");
	}
	printf(",x1\n");
}

void
popstk(union tree *sz, char *f) {
	adjstk("ba", sz, f);
}

void
pushstk(union tree *sz, char *f) {
	adjstk("bs", sz, f);
}

void
savestk(int a, char *f) {
}

void
werror(s)
char *s;
{

	fprintf(stderr, "%d: %s\n",line,s);
}

/* VARARGS1 */
void
error(s, p1, p2, p3, p4, p5, p6)
char *s;
long p1,p2,p3,p4,p5,p6; /* make sure ptr fits on 32 & 64 bit cpu's */
{

	nerror++;
	fprintf(stderr, "%d: ", line);
	fprintf(stderr, s, p1, p2, p3, p4, p5, p6);
	putc('\n', stderr);
}

void
psoct(an)
int an;
{
	// TODO: need to sign-extend? from 24 to 32 bits?
	printf("%d", an);
}

static void
outname(s)
register char *s;
{
	register int c;

	while ( (c = getchar()) )
		*s++ = c;
	*s++ = '\0';
}

/*
 * Read in an intermediate file.
 */
#define	STKS	100
void
getree()
{
	union tree *expstack[STKS], **sp;
	register union tree *tp;
	register int t, op, lastop;
	char s[80], *ss;	/* big for asm() stuff & long variable names */
	struct swtab *swp;
	long outloc = 0;
	int lbl, cond = 0;
	double atof();
	char *funcbase;
	char *cp;

	lastop = -1;
	funcbase = (char *)resetblk();
	sp = expstack;
	for (;;) {
		if (sp >= &expstack[STKS])
			error("Stack overflow botch");
		op = getwd();
		if ((op&0177400) != 0177000) {
			error("Intermediate file error");
			exit(1);
		}
		lbl = 0;
		switch(op &= 0377) {

	case SINIT:
		printf("\t.word\t%d\n", UNS(geti()));
		break;

	case EOFC:
		return;

	case BDATA:
		if (getwd() == 1) {
			printf("\t.byte\t");
			for (;;)  {
				t = UNS(geti());
				// We don't really know the user's intent
				if (t < ' ' || t > '~') {
					printf("%d", t);
				} else {
					printf("'%c'", t);
				}
				if (getwd() != 1)
					break;
				printf(",");
			}
			printf("\n");
		}
		break;

	case BSTR2:	// string initor. indir ref, two labels...
		// primary label already emitted!
		// get string - label name
		outname(s); // will have '_' prepended...
#if 0	// label was already setup, just add ':'
		ss = s;
		if (*ss == '_') ++ss;
		printf(	"\t.word\t^%s\n"
			"^%s::", ss, ss);
#else
		putchar(':');	// point to start of space (left side)
#endif
		goto getstring;

	case BSTR:	// string constant. indir ref, two labels...
		t = geti();	// label number
		printf(	"L%d:\t.word\tP%d\n"
			"P%d::", t, t, t);
		// FALLTHROUGH
	case BSTR0:
getstring:
		if ((t = getwd()) == 1) {
			int c;
			int n = 0;
			// TODO: need to ensure any label is "left aligned"
			// forcing newline is a crude solution.
			printf("\t.string\tn:\"");
			for (;;)  {
				int c = getwd();
				if (!c) { // end of string...
					printf("\"");
					getwd(); // gobble '0'
					break;
				}
				// 'as' does not support \000.
				// ctrl chars useless anyway.
				if (c < ' ' || c > '~') c = '_';
				printf("%c", c);
				++n;
				if ((t = getwd()) != 1) {
					printf("\"");
					break;
				}
			}
			printf("\n");
		}
		if (t != 2) printf("\t.string\tc:\"_\"\n");
		break;

	case PROG:
		printf("\t.text\n");
		break;

	case DATA:
		printf("\t.data\n");
		break;

	case BSS:
		printf("\t.bss\n");
		break;

	case SYMDEF:
		outname(s);
		if (*s) printf("\t.globl\t%s\n", s);
		sfuncr.nloc = 0;
		break;

	case RETRN:
		t = geti();	// line number
		if (gflag) printf("\t.line %d\n", t);
		// return value is in x5...
		printf(	"\tlca\tx2,x1\n"
			"\tlca\t-4(x1),x2\n"
			"\tlcr\t0(x1),077\n");
		break;

	case CSPACE:
		outname(s);
		printf("\t.comm\t%s,%d\n", s, UNS(geti()));
		break;

	case SSPACE:
		printf("\t.space\t%d\n", UNS(t=geti()));
		totspace += (unsigned)t;
		break;

	case SAVE:
		// nothing to do, as all is abstracted.
		break;

	case SETSTK:
		t = geti();	// auto var size
		cp = curfnc;
		if (*cp == '_') ++cp;
		printf(	"\t.data\n"
			"@%s:\t.bin\t0x%x#4\n",
			cp, t);
		break;

	case PROFIL:
		t = geti();
		outname(s);
		printf(	"// profile %s L%d @mcount\n",
			s, t);
		break;

	case ASSEM:
		outname(s);
		printf("%s\n", s);
		break;

	case SNAME:
		outname(s);
		printf("~%s=L%d\n", s+1, geti());
		break;

	case ANAME:
		outname(s);
		printf("~%s=%d\n", s+1, geti());
		break;

	case RNAME:
		outname(s);
		printf("~%s=r%d\n", s+1, geti());
		break;

	case SWIT:
		t = geti();
		line = geti();
		if (gflag) printf("\t.line %d\n", line);
		funcbase = (char *)resetblk();
		while(swp=(struct swtab *)getblk(sizeof(*swp)), swp->swlab = geti())
			swp->swval = geti();
		pswitch((struct swtab *)funcbase, swp, t);
		break;

	case C3BRANCH:		/* for fortran [sic] */
		error("fortran c3branch not supported\n");
		exit(1);
    
	case CBRANCH:
		lbl = geti();
		cond = geti();
		/* fall through */

	case EXPR:
		line = geti();
		if (gflag) printf("\t.line %d\n", line);
		if (sp != &expstack[1]) {
			error("Expression input botch");
			exit(1);
		}
		--sp;
		regpanic = 0;
		if (setjmp(jmpbuf)) {
			regpanic = 10;
			fseek(stdout, outloc, 0);
		}
		nstack = 0;
		panicposs = 0;
		*sp = tp = optim(*sp);
		if (regpanic==0 && panicposs)
			outloc = ftell(stdout);
		if (op==CBRANCH)
			cbranch(tp, lbl, cond, RSTART);
		else
			rcexpr(tp, efftab, RSTART);
		funcbase = (char *)resetblk();
		break;

	case NAME:
		t = geti();
		if (t==EXTERN) {
			tp = getblk(sizeof(struct xtname));
			tp->t.type = geti();
			outname(s);
			tp->x.name = (char *)getblk(strlen(s) + 1);
			strcpy(tp->x.name, s);
		} else {
			tp = getblk(sizeof(struct tname));
			tp->t.type = geti();
			tp->n.nloc = geti();
		}
		tp->t.op = NAME;
		tp->n.class = t;
		tp->n.regno = 0;
		tp->n.offset = 0;
		*sp++ = tp;
		break;

	case CON:
		t = geti();
		*sp++ = tconst(geti(), t, 0);
		break;

	case CCON:
		t = geti();
		*sp++ = tconst0(CCON, geti(), t, 0);
		break;

	case LCON:
		geti();	/* ignore type, assume long */
		op = geti();	// lo bit
		t = geti();	// hi bit
		if ((t==0 && op>=0) || (t == -1 && op<0)) {
			*sp++ = tnode(ITOL, LONG, tconst(op, INT, 0), TNULL);
			break;
		}
		// TODO: try and share constants...
		tp = getblk(sizeof(struct lconst));
		tp->l.op = LCON;
		tp->l.type = LONG;
		tp->l.label = isn++;
		tp->l.lvalue = ((long)t<<32) | UNS(op);	/* nonportable */
		*sp++ = tp;
		break;
	case FCON:
		t = geti();
		outname(s);
		tp = getblk(sizeof(struct ftconst));
		tp->f.op = FCON;
		tp->f.type = t;
		tp->f.label = isn++;
		tp->f.fvalue = atof(s);
		*sp++ = tp;
		break;
	case FSEL:
		tp = tnode(FSEL, geti(), *--sp, TNULL);
		t = geti();
		tp->t.tr2 = tnode(COMMA, INT, tconst(geti(), INT, 0), tconst(t, INT, 0));
		if (tp->t.tr2->t.tr1->c.value==16)
			tp = paint(tp->t.tr1, tp->t.type);
		*sp++ = tp;
		break;

	case STRASG:
		tp = getblk(sizeof(struct fasgn));
		tp->t.op = STRASG;
		tp->t.type = geti();
		tp->F.mask = geti();
		tp->t.tr1 = *--sp;
		tp->t.tr2 = NULL;
		*sp++ = tp;
		break;

	case NULLOP:
		*sp++ = tnode(0, 0, TNULL, TNULL);
		break;

	case LABEL:
		if (lastop == LABEL) putchar('\n');
		label(geti());
		break;

	case NLABEL:	// only one case, public .data variable definition
		outname(s);
		op = getwd();
		t = getwd();
		ss = s;
		if (*ss == '_') ++ss;
		if (op == EXTERN) printf("\t.globl\t%s,^%s\n", s, ss);
		printf( "%s:\t.word\t^%s\n"
			"^%s:", s, ss, ss); // needs to be on same line for punc control
		break;

	case SLABEL:	// non-array, in .bss, with simple size
		outname(s);
		t = op = geti();	// total size
		goto bss_label;

	case ALABEL:	// array, in .bss, with element and num-elements
		outname(s);
		t = getwd();	// elem type
		op = geti();	// total size
		// TODO: more interpretation of type?
		t = ellength(t);
bss_label:
		ss = s;
		if (*ss == '_') ++ss;
		printf(	"\t.data\n"
			"%s:\t.word\t^%s\n"
			"\t.bss\n"
			"^%s:", s, ss, ss);
		if (t == 1) {
			printf(	":\t.space\t%d\n", op);
		} else {
			printf(	"\t.space\t%d\n", t);
			if (op - t) {
				printf("\t.space\t%d\n", op - t);
			}
		}
		break;

	case RLABEL:
		t = geti();	// line number
		outname(s);
		if (curfnc) free(curfnc);
		curfnc = strdup(s);
		cp = curfnc;
		if (*cp == '_') ++cp;
		printf("%s:", curfnc);
		if (gflag) printf("\t.line %d\n", t);
		printf(	"\tscr\t0(x1),070\n"
			"\tlca\tx2,-4(x1)\n"
			"\tlca\tx1,x2\n"
			"\tbs\t@%s,x1\n",
			cp);
		break;

	case BRANCH:
		branch(geti(), 0, 0);
		break;

	case SETREG:
		nreg = geti()-1;
		break;

	default:
		if (opdope[op]&BINARY) {
			if (sp < &expstack[1]) {
				error("Binary expression botch");
				exit(1);
			}
			tp = *--sp;
			sp[-1] = tnode(op, geti(), sp[-1], tp);
		} else
			sp[-1] = tnode(op, geti(), sp[-1], TNULL);
		break;
	}
	lastop = op;
	}
}

/* Read a little-endian 16 bit word from the input stream */
int
getwd()
{
	register int i;

	i = getchar() & 0xff;
	i |= (getchar() & 0xff) << 8;
	return(i);
}

/* Read a little-endian 32 bit word from the input stream */
int
geti()
{
	register int i;

	i = getchar() & 0xff;
	i |= (getchar() & 0xff) << 8;
	i |= (getchar() & 0xff) << 16;
	i |= (getchar() & 0xff) << 24;
	return(i);
}

void
strasg(atp)
union tree *atp;
{
	register union tree *tp;
	register int nwords, i;

	nwords = atp->F.mask/sizeof(short);
	tp = atp->t.tr1;
	while (tp->t.op == SEQNC) {
		rcexpr(tp->t.tr1, efftab, RSTART);
		tp = tp->t.tr2;
	}
	if (tp->t.op != ASSIGN) {
		if (tp->t.op==RFORCE) {	/* function return */
			if (sfuncr.nloc==0) {
				sfuncr.nloc = isn++;
				printf(".bss\nL%d:.=.+%d\n.text\n", sfuncr.nloc,
					UNS(nwords*sizeof(short)));
			}
			atp->t.tr1 = tnode(ASSIGN, STRUCT, (union tree *)&sfuncr, tp->t.tr1);
			strasg(atp);
			printf("li\tr2,L%d\n", sfuncr.nloc);
			return;
		}
		if (tp->t.op==CALL) {
			rcexpr(tp, efftab, RSTART);
			return;
		}
		error("Illegal structure operation");
		return;
	}
	tp->t.tr2 = strfunc(tp->t.tr2);
	if (nwords==1)
		paint(tp, INT);
	else if (nwords==sizeof(short))
		paint(tp, LONG);
	else {
		if ((tp->t.tr1->t.op!=NAME && tp->t.tr1->t.op!=STAR)
		 || (tp->t.tr2->t.op!=NAME && tp->t.tr2->t.op!=STAR)) {
			error("unimplemented structure assignment");
			return;
		}
		tp->t.tr1 = tnode(AMPER, STRUCT+PTR, tp->t.tr1, TNULL);
		tp->t.tr2 = tnode(AMPER, STRUCT+PTR, tp->t.tr2, TNULL);
		tp->t.op = STRSET;
		tp->t.type = STRUCT+PTR;
		tp = optim(tp);
		rcexpr(tp, efftab, RSTART);
		if (nwords < 7) {
			for (i=0; i<nwords; i++)
				printf("mov\t(r3)+,(r2)+\n");
			return;
		}
		printf("li\tr0, %d\n", UNS(nwords));
		printf("L%d:mov	(r3)+,(r2)+\ndec\tr0\nbjne\tL%d\n", isn, isn);
		isn++;
		return;
	}
	rcexpr(tp, efftab, RSTART);
}

/*
 * Reduce the degree-of-reference by one.
 * e.g. turn "ptr-to-int" into "int".
 */
int
decref(t)
register int t;
{
	if ((t & ~TYPE) == 0) {
		error("Illegal indirection");
		return(t);
	}
	return((((unsigned)t>>TYLEN) & ~TYPE) | (t&TYPE));
}

/*
 * Increase the degree of reference by
 * one; e.g. turn "int" to "ptr-to-int".
 */
int
incref(t)
int t;
{
	return(((t&~TYPE)<<TYLEN) | (t&TYPE) | PTR);
}
