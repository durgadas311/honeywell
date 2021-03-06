/*
 * C compiler, part 2
 *
 * (long)btodb(l) produced 'no code table error for op: >>(17) type: 6'
 * allow both long and ulong at line ~341.  1996/6/19
 */
#include <stdlib.h>
#include <ctype.h>

#include "c1.h"

#ifdef	DEBUG
#define	dbprint(op)	printf("	// %d, %s", op, opntab[op])
#else
#define	dbprint(op)	/* */
#endif

/* static int debug = 0; */

char maprel[] = { EQUAL, NEQUAL,  GREATEQ, GREAT,  LESSEQ,
                  LESS,  GREATQP, GREATP,  LESSEQP, LESSP
};

char notrel[] = { NEQUAL, EQUAL, GREAT, GREATEQ, LESS,
		  LESSEQ, GREATP, GREATQP, LESSP, LESSEQP
};

union tree *czero;
union tree *cone;

struct tname sfuncr = { NAME, STRUCT, STATIC, 0, 0, 0 };

struct	table	*cregtab;

int	nreg	= 8;
int	isn	= 10000;

int gflag = 0;

int
main(argc, argv)
int	argc;
char	*argv[];
{
	extern void prcons();
	if (argc<4) {
		error("Arg count");
		exit(1);
	}
	if (argc > 4 && strcmp(argv[4], "-g") == 0) {
		++gflag;
	}
	if (freopen(argv[1], "r", stdin)==NULL) {
		error("Missing temp file");
		exit(1);
	}
	if ((freopen(argv[3], "w", stdout)) == NULL) {
		error("Can't create %s", argv[3]);
		exit(1);
	}
	// TODO: load "global constants" from file?
	czero = tconst(0, INT, 1);	// these will pick up globals...
	cone = tconst(1, INT, 1);	// these will pick up globals...
	getree();
	/*
	 * tack on the string file.
	 */
	printf("\t.data\n");
	// TODO: store "global constants" to file? and don't print?
	// first, dump all constants...
	prcons();

	if (*argv[2] != '-') {
		if (freopen(argv[2], "r", stdin)==NULL) {
			error("Missing temp file");
			exit(1);
		}
		getree();
	}
	if (totspace >= (unsigned)56000)
		werror("possibly too much data");
	exit(nerror!=0);
}

extern struct optab *optab;

/*
 * Given a tree, a code table, and a
 * count of available registers, find the code table
 * for the appropriate operator such that the operands
 * are of the right type and the number of registers
 * required is not too large.
 * Return a ptr to the table entry or 0 if none found.
 */
struct optab *
match(tree, table, nrleft, nocvt)
union tree *tree;
struct table *table;
int nrleft, nocvt;
{
#define	NOCVL	1
#define	NOCVR	2
	int op, d1, d2 = 0, dope, i;
	union tree *p2 = 0;
	register union tree *p1;
	register struct optab *opt;

	if (tree==NULL) {
		return(NULL);
	}
	if (table == lsptab) {
		table = sptab;
	}
	if ((op = tree->t.op) == 0) {
		return(0);
	}
	dope = opdope[op];
	if ((dope & LEAF) == 0) {
		p1 = tree->t.tr1;
	} else {
		p1 = tree;
	}
	d1 = dcalc(p1, nrleft);
	if ((dope & BINARY) != 0) {
		p2 = tree->t.tr2;
		// TODO: probably can't do any of this...
		/*
		 * If a subtree starts off with a conversion operator,
		 * try for a match with the conversion eliminated.
		 * E.g. int = double can be done without generating
		 * the converted int in a register by
		 * movf double,fr0; movfi fr0,int .
		 */
		if ((opdope[p2->t.op] & CNVRT) && (nocvt & NOCVR) == 0
			 && (opdope[p2->t.tr1->t.op] & CNVRT) == 0) {
			tree->t.tr2 = p2->t.tr1;
			if ( (opt = match(tree, table, nrleft, NOCVL)) ) {
				return(opt);
			}
			tree->t.tr2 = p2;
		} else if (opdope[p1->t.op] & CNVRT && (nocvt & NOCVL) == 0
		 && (opdope[p1->t.tr1->t.op] & CNVRT) == 0) {
			tree->t.tr1 = p1->t.tr1;
			if ( (opt = match(tree, table, nrleft, NOCVR)) ) {
				return(opt);
			}
			tree->t.tr1 = p1;
		}
		d2 = dcalc(p2, nrleft);
	}
	i = 0;
	for (; table->tabop != op; table++, i++) {
		if (table->tabop == 0) {
			return(0);
		}
	}
/*	fprintf(stderr, "op=%d, index=%d, ", op, i); */
	i = 0;
	for (opt = table->tabp; opt->tabdeg1 != 0; opt++, i++) {
		if (d1 > (opt->tabdeg1 & DALL)
		 || (opt->tabdeg1 >= DPTR && (p1->t.op != STAR))) {
			continue;
		}
		if (notcompat(p1, opt->tabtyp1, opt->tabdeg1, op)) {
			continue;
		}
		if ((opdope[op]&BINARY) != 0 && p2 != 0) {
			if (d2 > (opt->tabdeg2 & DALL)
			 || ((opt->tabdeg2 >= DPTR) && (p2->t.op != STAR)) ) {
				continue;
			}
			if (notcompat(p2, opt->tabtyp2, opt->tabdeg2, 0)) {
				continue;
			}
			if ((opt->tabdeg2 & 077) == DREG && xdcalc(p2, nrleft) > DREG) {
				continue;
			}
		}
/*		fprintf(stderr, "offset %d\n", i); */
		return(opt);
	}
	return(0);
}

static int syscall(union tree *tree) {
	unsigned int scn = 0;
	union tree *t1 = tree->t.tr1;
	if (t1->t.op != NAME || (t1->t.type & XTYPE) != FUNC ||
			t1->x.class != EXTERN ||
			sscanf(t1->x.name, "__sc%o", &scn) != 1 ||
			scn > 077) {
		return 0;
	}
	printf(	"\tmc\n"
		"\t.byte\t0%03o\n", scn | 0100);
	return 1;
}

/*
 * Given a tree, a code table, and a register,
 * produce code to evaluate the tree with the appropriate table.
 * Registers reg and upcan be used.
 * If there is a value, it is desired that it appear in reg.
 * The routine returns the register in which the value actually appears.
 * This routine must work or there is an error.
 * If the table called for is cctab, sptab, or efftab,
 * and tree can't be done using the called-for table,
 * another try is made.
 * If the tree can't be compiled using cctab, regtab is
 * used and a "tst" instruction is produced.
 * If the tree can't be compiled using sptab,
 * regtab is used and the register is pushed on the stack.
 * If the tree can't be compiled using efftab,
 * just use regtab.
 * Regtab must succeed or an "op not found" error results.
 *
 * A number of special cases are recognized, and
 * there is an interaction with the optimizer routines.
 */
int
rcexpr(atree, atable, reg)
union tree *atree;
struct table *atable;
int reg;
{
	register int r;
	int modf, nargs, recurf;
	register union tree *tree;
	register struct table *table;
	char *cp;

	table = atable;
	recurf = 0;
	if (reg < 0) {
		recurf++;
		reg = ~reg;
		// TODO: this looked wrong... still does
		if (reg > REND) {
			reg = RSTART;
			recurf++;
		}
	}
again:
	if((tree=atree)==0) {
		return(0);
	}
	if (tree->t.type==VOID) {
		if (table!=efftab)
			error("Illegal use of void");
		tree->t.type = INT;
	}
	if (opdope[tree->t.op]&RELAT && tree->t.tr2->t.op==CON
	    && tree->t.tr2->c.value==0
	    && table==cctab)
		tree = atree = tree->t.tr1;
	/*
	 * fieldselect(...) : in efftab mode,
	 * ignore the select, otherwise
	 * do the shift and mask.
	 */
	if (tree->t.op == FSELT) {
		if (table==efftab)
			atree = tree = tree->t.tr1;
		else {
			tree->t.op = FSEL;
			atree = tree = optim(tree);
		}
	}
	switch (tree->t.op)  {

	/*
	 * Structure assignments
	 */
	case STRASG:
		strasg(tree);
		return(0);

	/*
	 * An initializing expression
	 */
	case INIT:
		tree = optim(tree);
		doinit(tree->t.type, tree->t.tr1);
		return(0);

	/*
	 * Put the value of an expression in r0,
	 * for a switch or a return
	 */
	case RFORCE:
		tree = tree->t.tr1;
		if((r=rcexpr(tree, regtab, reg)) != RSTART)
			movreg(r, RSTART, tree);
		return(0);

	/*
	 * sequential execution
	 */
	case SEQNC:
		r = nstack;
		rcexpr(tree->t.tr1, efftab, reg);
		nstack = r;
		atree = tree = tree->t.tr2;
		goto again;

	/*
	 * Handle a subroutine call. It has to be done
	 * here because if cexpr got called twice, the
	 * arguments might be compiled twice.
	 * There is also some fiddling so the
	 * first argument, in favorable circumstances,
	 * goes to (sp) instead of -(sp), reducing
	 * the amount of stack-popping.
	 */
	case CALL:
		r = 0;
		nargs = 0;
		modf = 0;
		if (tree->t.tr1->t.op == NAME && tree->t.tr1->n.class == EXTERN) {
			cp = tree->t.tr1->x.name;
		} else {
			cp = NULL;
		}
#ifdef notdef
		/*
		 * The following code would catch instances of foo(...) where
		 * "foo" was anything other than a simple name.  In particular
		 * f(...), (fp(...))(...) and (ffp(...))(...) where "f" is a
		 * pointer to a function, "fp" is a function returning a
		 * pointer to a function and "ffp" is a pointer to a function
		 * returning a pointer to a function.  The catch would among
		 * other things cause the (sp)/-(sp) stack optimization to
		 * stop working.  The compiler has been tested in all these
		 * different cases with the catch commented out and all the
		 * code generated was correct.  So what was it here for?
		 * If a strange error crops up, uncommenting the catch might
		 * be tried ...
		 */
		if (tree->t.tr1->t.op!=NAME || tree->t.tr1->n.class!=EXTERN) {
			nargs++;
			nstack++;
		}
#endif
		if (r) savestk(r, cp);
		tree = tree->t.tr2;
		if(tree->t.op) {
			while (tree->t.op==COMMA) {
				r += comarg(tree->t.tr2, &modf);
				tree = tree->t.tr1;
				nargs++;
			}
			r += comarg(tree, &modf);
			nargs++;
		}
		tree = atree;
		tree->t.op = CALL2;
		if (modf && tree->t.tr1->t.op==NAME
		   && tree->t.tr1->n.class==EXTERN)
			tree->t.op = CALL1;
		if (r) pushstk(r, cp);
		// Special case for system calls...
		// avoids extra call frame and routines.
		// "__sc###" translates to "mc;.byte ###"
		if (!syscall(tree) && cexpr(tree, regtab, reg)<0) {
			error("compiler botch: call");
		}
		if (r) popstk(r, cp);
		nstack -= nargs;
		if (table==efftab || table==regtab)
			return(RSTART);
		r = RSTART;
		goto fixup;

	/*
	 * Try to change * to shift.
	 */
	case TIMES:
	case ASTIMES:
		tree = pow2(tree);
	}
	/*
	 * Try to find postfix ++ and -- operators that can be
	 * pulled out and done after the rest of the expression
	 */
	if (table!=cctab && table!=cregtab && recurf<2
	 && (opdope[tree->t.op]&LEAF)==0) {
		if ( (r=delay(&atree, table, reg)) ) {
			tree = atree;
			table = efftab;
			reg = r-1;
		}
	}
	/*
	 * Basically, try to reorder the computation
	 * so  reg = x+y  is done as  reg = x; reg += y
	 */
	if (recurf==0 && reorder(&atree, table, reg)) {
		if (table==cctab && atree->t.op==NAME)
			return(reg);
	}
	tree = atree;
	if (table==efftab && tree->t.op==NAME)
		return(reg);
	if ((r=cexpr(tree, table, reg))>=0) {
		if (table==cregtab && (tree->t.op==INCAFT
		    || tree->t.op==DECAFT || tree->t.op==TIMES)) {
			goto fixup;
		}
		return(r);
	}
	if (table!=regtab && (table!=cctab||(opdope[tree->t.op]&RELAT)==0)) {
		if((r=cexpr(tree, regtab, reg))>=0) {
			if (tree->t.op != LOAD) {
				return r;
			}
	fixup:
			modf = isfloat(tree);
			dbprint(tree->t.op);
			if (table==sptab || table==lsptab) {
				printf("\tlca\tx%d,%d(x1) // fixup\n",
					r, -(nstack * SZPTR));
				nstack++;
			}
			if (table==cctab || table==cregtab) {
				// need to test non-zero?
				printf("\tc\t");
				pname(czero, 0, 0);
				printf(",x%d //c10.410\n", r);
			}
			return(r);
		}
	}
	/*
	 * Special grace for unsigned chars as right operands
	 */
	if (opdope[tree->t.op]&BINARY && tree->t.tr2->t.type==UNCHAR) {
		tree->t.tr2 = tnode(LOAD, UNSIGN, tree->t.tr2, TNULL);
		return(rcexpr(tree, table, reg));
	}
	/*
	 * There's a last chance for this operator
	 */
	if (tree->t.op==LTOI) {
		r = rcexpr(tree->t.tr1, regtab, reg);
		if (r >= 0) {
			r++;
			goto fixup;
		}
	}

	r = tree->t.op;
	if (tree->t.type == STRUCT)
		error("Illegal operation on structure");
	else if (r > 0 && r < MINSTAT && opntab[r])
		error("No code table for op: %s(%d) type: %d", opntab[r], r,
			tree->t.type);
	else
		error("No code table for op %d", r);
	return(reg);
}

/*
 * Try to compile the tree with the code table using
 * registers areg and up.  If successful,
 * return the register where the value actually ended up.
 * If unsuccessful, return -1.
 *
 * Most of the work is the macro-expansion of the
 * code table.
 */
int
cexpr(tree, table, areg)
register union tree *tree;
struct table *table;
int areg;
{
	int c, n, r, tab;
	register union tree *p, *p1;
	struct table *ctable;
	union tree *p2;
	char *string;
	int reg, reg1, rreg, flag, opd;
	int neg;
	struct optab *opt;

	tab = 0;
	neg = 1;	// start accessing true variable
	reg = areg;
	p1 = tree->t.tr2;
	c = tree->t.op;
	opd = opdope[c];
	/*
	 * When the value of a relational or a logical expression is
	 * desired, more work must be done.
	 */
	if ((opd&RELAT||c==LOGAND||c==LOGOR||c==EXCLA) && table!=cctab) {
		cbranch(tree, c=isn++, 1, reg);
		rcexpr(czero, table, reg);
		branch(isn, 0, 0);
		label(c);
		rcexpr(cone, table, reg);
		label(isn++);
		return(reg);
	}
	if(c==QUEST) {
		if (table==cctab)
			return(-1);
		cbranch(tree->t.tr1, c=isn++, 0, reg);
		flag = nstack;
		rreg = rcexpr(p1->t.tr1, table, reg);
		nstack = flag;
		branch(r=isn++, 0, 0);
		label(c);
		reg = rcexpr(p1->t.tr2, table, rreg);
		if (rreg!=reg)
			movreg(reg, rreg, tree->t.tr2);
		label(r);
		return(rreg);
	}
	reg = oddreg(tree, reg);
	reg1 = reg+1;
	/*
	 * long values take 2 registers.
	 */
	if ((tree->t.type==LONG||tree->t.type==UNLONG||
		(opd&RELAT&&(tree->t.tr1->t.type==LONG||tree->t.tr1->t.type==UNLONG)))
	   && tree->t.op!=ITOL)
		reg1++;
	// everything after this must exit via 'out:'
	/*
	 * Leaves of the expression tree
	 */
	if ((r = chkleaf(tree, table, reg)) >= 0) {
		reg = r;
		goto out;
	}

	/*
	 * Because of the way the TI990 instruction set works,
	 * char = *intreg++ cannot go through.
 	 */
	if (tree->t.tr2 && (tree->t.tr2->t.op==AUTOI)
	 && (tree->t.tr1->t.type==CHAR || tree->t.tr1->t.type==UNCHAR)
	 && tree->t.tr2->t.type!=CHAR && tree->t.tr2->t.type!=UNCHAR) {
		tree->t.tr2 = tnode(LOAD, tree->t.tr2->t.type, tree->t.tr2, TNULL);
	}
	/*
	 * Another peculiarity of the PDP11 table manifested itself when
	 * amplifying the move3: table.  The same case which optimizes
	 * u_char to char moves is used to move a u_char to a register. This
	 * is wrong, leading to sign extension.  Rather than lose the ability
	 * to generate better code when moving a u_char to a char, a check
	 * is made here to prevent sign extension.
	 *
	 * If the opcode is assign, the destination is a register and the
	 * source is u_char then do a conversion.
	 *
	 * u_char handling in the compiler is a bit awkward, it would be nice
	 * if %aub in the tables had a more unique meaning.
	*/
	if (tree->t.tr2 && tree->t.tr1->t.op == NAME
	 && tree->t.tr1->n.class == REG && tree->t.op == ASSIGN
	 && tree->t.tr2->t.type == UNCHAR) {
		tree->t.tr2 = tnode(LOAD, UNSIGN, tree->t.tr2, TNULL);
	}
	if (table==cregtab) {
		table = regtab;
	}
	/*
	 * The following peculiar code depends on the fact that
	 * if you just want the codition codes set, efftab
	 * will generate the right code unless the operator is
	 * a shift or
	 * postfix ++ or --. Unravelled, if the table is
	 * cctab and the operator is not special, try first
	 * for efftab;  if the table isn't, if the operator is,
	 * or the first match fails, try to match
	 * with the table actually asked for.
	 */
	/*
	 * Account for longs and oddregs; below is really
	 * r = nreg - reg - (reg-areg) - (reg1-reg-1);
	 */
	r = nreg - reg + areg - reg1 + 1;
	if (table!=cctab || c==INCAFT || c==DECAFT || tree->t.type==LONG || tree->t.type==UNLONG
/*	 || c==ASRSH || c==ASLSH || c==ASULSH || tree->t.tr1->t.type==UNCHAR */
	 || c==ASRSH || c==ASLSH
	 || (opt = match(tree, efftab, r, 0)) == 0) {
		if ((opt=match(tree, table, r, 0))==0) {
			reg = -1;
			goto out;
		}
	}
	string = opt->tabstring;
	p1 = tree->t.tr1;
	p2 = 0;
	if (opdope[tree->t.op]&BINARY) {
		p2 = tree->t.tr2;
	}

loop:
	/*
	 * The 0200 bit asks for a tab.
	 */
	if ((c = *string++) & 0200) {
		c &= 0177;
		putchar('\t');
		tab = 1;
	}
	switch (c) {

	/* At the end of snippet expansion, adjust the register to the
	 * one with the result for 'mpy' and 'div' instructions. This
	 * balances with the adjustment made in 'oddreg()'.
	 */
	case '\0':
		// @div returns quotient in X5, remainder in X6.
		// for *MOD, we need to ensure X6 gets used.
		// previous versions used oddreg() to undo,
		// but that's not working right.
		// TODO: when, if ever, does this need undoing?
		if (!isfloat(tree)) {
			switch (tree->t.op) {
			case MOD:
			case UMOD:
			case ASMOD:
			case ASUMOD:
				reg++;
				break;
			}
		}
		goto out;

	/* A1 */
	case 'A':
		p = p1;
		goto adr;

	/* A2 */
	case 'B':
		p = p2;
		//goto adr;

	adr:
		c = 0;
		while (*string=='\'') {
			c++;
			string++;
		}
		if (*string=='+') {
			c = 100;
			string++;
		}
		if ((p->t.type&XTYPE) == FUNC) {
			pname(p, c, 0);
		} else {
			pname(p, c, neg);
		}
		tab = 0;
		goto loop;

	/* I */
	case 'M':
		if ((c = *string)=='\'') {
			string++;
		} else if (c=='"') {
			string++;
			error("Illegal immediate reference");
			goto loop;
		} else {
			c = 0;
		}
		prins(tree->t.op, c, instab, tab);
		tab = 0;
		goto loop;

	case 'N':	// Need reference to a constant
		c = 0;
		n = 0;
		if (*string == 'N') {
			++string;
			n = 1;
		}
		while (isdigit(*string)) {
			c *= 10;
			c += (*string++ - '0');
		}
		if (n) c = -c;
		p = tconst(c, INT, 0);
		goto adr;

	/* B1 */
	case 'C':
		if ((opd&LEAF) != 0)
			p = tree;
		else
			p = p1;
		goto loop;

	/* BF */
	case 'P':
		p = tree;
		goto loop;

	/* B2 */
	case 'D':
		p = p2;
		goto loop;

	case '<':
		c = *string++;
		if ( (c=='1' && (p1->t.type==CHAR || p1->t.type==UNCHAR)) ||
		     (c=='2' && (p2->t.type==CHAR || p2->t.type==UNCHAR)) ) {
			printf("sla");
			tab = 0;
		} else {
			while( *string++!='\n' );
		}
		goto loop;

	case '>':
		c = *string++;
		if ((c=='1' && p1->t.type==CHAR) ||
		    (c=='2' && p2->t.type==CHAR)) {
			printf("sra");
			tab = 0;
		} else if ((c=='1' && p1->t.type==UNCHAR) ||
			 (c=='2' && p2->t.type==UNCHAR)) {
			printf("srl");
			tab = 0;
		} else {
			while( *string++!='\n' );
		}
		goto loop;

	/* F */
	case 'G':
		p = p1;
		flag = 01;
		goto subtre;

	/* S */
	case 'K':
		p = p2;
		flag = 02;
		goto subtre;

	/* H */
	case 'H':
		p = tree;
		flag = 04;

	subtre:
		ctable = regtab;
		if (flag&04)
			ctable = cregtab;
		c = *string++ - 'A';
		if (*string=='!') {
			string++;
			c |= 020;	/* force right register */
		}
		if (*string=='?') {
			string++;
			c |= 040;	/* force condition codes */
		}
		if ((c&02)!=0)
			ctable = sptab;
		if ((c&04)!=0)
			ctable = cctab;
		if ((flag&01) && ctable==regtab && (c&01)==0
		  && ((c&040)||tree->t.op==DIVIDE||tree->t.op==MOD
		   || tree->t.op==ASDIV||tree->t.op==ASMOD||tree->t.op==ITOL))
			ctable = cregtab;
		if ((c&01)!=0) {
			p = p->t.tr1;
			if(collcon(p) && ctable!=sptab) {
				p = p->t.tr1;
			}
		}
		if (table==lsptab && ctable==sptab) {
			ctable = lsptab;
		}
		if (c&010)
			r = reg1;
		else
			if (opdope[p->t.op]&LEAF || p->t.degree < 2)
				r = reg;
			else
				r = areg;
		rreg = rcexpr(p, ctable, r);
		if (ctable!=regtab && ctable!=cregtab)
			goto loop;
		if (c&010) {
			if (c&020 && rreg!=reg1)
				movreg(rreg, reg1, p);
			else
				reg1 = rreg;
		} else if (rreg!=reg) {
			if ((c&020)==0 && oddreg(tree, 0)==0 && tree->t.type!=LONG
			&& tree->t.type!=UNLONG
			&& (flag&04
			  || (flag&01&&xdcalc(p2,nreg-rreg-1)<=(opt->tabdeg2&077))
			  || (flag&02&&xdcalc(p1,nreg-rreg-1)<=(opt->tabdeg1&077)))) {
				reg = rreg;
				reg1 = rreg+1;
			} else
				movreg(rreg, reg, p);
		}
		tab = 0;
		goto loop;

	/* R, R-, R= */
	case 'I':
		r = reg;
		c = *string;
		if (c=='-' || c=='=') {
			string++;
			if (c != '=' || tree->t.op != ASUMOD)
				r--;
		}
		goto preg;

	/* R1 */
	case 'J':
		r = reg1;
	preg:
		if (*string=='+') {
			string++;
			r++;
		}
		if (r>nreg || (r>=4 && tree->t.type==DOUBLE)) {
			if (regpanic)
				error("Register overflow: simplify expression");
			else
				longjmp(jmpbuf, 1);
		}
		printf("x%d", r);
		tab = 0;
		goto loop;

	case '&':
		printf("%d", -(nstack * SZPTR));
		tab = 0;
		goto loop;

	case 'Q':
		nstack++;
		goto loop;

	/* #1 */
	case '#':	// index(R) notation
		p = p1->t.tr1;
		goto nmbr;

	/* #2 */
	case '"':	// index(R) notation
		p = p2->t.tr1;

	nmbr:	// This requires the true address of the variable
		if (collcon(p)) {
			if ((p = p->t.tr2)->t.op == CON) {
				psoct(p->c.value);
			} else if (p->t.op==AMPER) {
				// "1" means true address...
				pname(p->t.tr1, 0, 1);
			}
		} else if (*string=='+') {
			// should never get here
			fprintf(stderr, "%s %d: cexpr '+' warning\n", __FILE__, __LINE__);
			string++;
		} else {
			printf("0"); // index must be non-blank
		}
		tab = 0;
		goto loop;

	/*
	 * Sign extend int to long for / %
	 */
	case 'T':
		fprintf(stderr, "%s %d: cexpr 'T' warning\n", __FILE__, __LINE__);
		tab = 0;
		goto loop;

	case '\n':
		dbprint(tree->t.op);
		putchar('\n');
		tab = 0;
		goto loop;

	case 'V':	/* adc sbc, clr, or sxt as required for longs */
		fprintf(stderr, "%s %d: cexpr 'V' warning\n", __FILE__, __LINE__);
		goto loop;

	/*
	 * Mask used in field assignments
	 */
	case 'Z':
		printf("%d", UNS(tree->F.mask));
		goto loop;

#if 0
	/*
	 * Relational on long values.
	 * Might bug out early. E.g.,
	 * (long<0) can be determined with only 1 test.
	 */
	case 'X':
		if (xlongrel(*string++ - '0')) {
			goto out;
		}
		goto loop;
#endif
	}
	tab = (c == '\t');
	putchar(c);
	goto loop;
out:
	return(reg);
}

/*
 * This routine just calls sreorder (below)
 * on the subtrees and then on the tree itself.
 * It returns non-zero if anything changed.
 */
int
reorder(treep, table, reg)
union tree **treep;
struct table *table;
int reg;
{
	register int r, o;
	register union tree *p;

	p = *treep;
	o = p->t.op;
	if (opdope[o]&LEAF||o==LOGOR||o==LOGAND||o==SEQNC||o==QUEST||o==COLON)
		return(0);
	while(sreorder(&p->t.tr1, regtab, reg, 1))
		;
	if (opdope[o]&BINARY)
		while(sreorder(&p->t.tr2, regtab, reg, 1))
			;
	r = 0;
	if (table!=cctab)
	while (sreorder(treep, table, reg, 0))
		r++;
	*treep = optim(*treep);
	return(r);
}

/*
 * Basically this routine carries out two kinds of optimization.
 * First, it observes that "x + (reg = y)" where actually
 * the = is any assignment op is better done as "reg=y; x+reg".
 * In this case rcexpr is called to do the first part and the
 * tree is modified so the name of the register
 * replaces the assignment.
 * Moreover, expressions like "reg = x+y" are best done as
 * "reg = x; reg += y" (so long as "reg" and "y" are not the same!).
 */
int
sreorder(treep, table, reg, recurf)
union tree **treep;
struct table *table;
int reg, recurf;
{
	register union tree *p, *p1;

	p = *treep;
	if (opdope[p->t.op]&LEAF)
		return(0);
	if (p->t.op==PLUS && recurf) {
		if (reorder(&p->t.tr2, table, reg)) {
			*treep = p = optim(p);
		}
	}
	if ((p1 = p->t.tr1)==TNULL)
		return(0);
	if (p->t.op==STAR || p->t.op==PLUS) {
		if (recurf && reorder(&p->t.tr1, table, reg)) {
			*treep = p = optim(p);
			if (opdope[p->t.op]&LEAF)
				return(0);
		}
		p1 = p->t.tr1;
	}
	if (p1->t.op==NAME) switch(p->t.op) {
		case ASLSH:
		case ASRSH:
		case ASSIGN:
			if (p1->n.class != REG || p1->n.type==CHAR
			  || isfloat(p->t.tr2))
				return(0);
			if (p->t.op==ASSIGN) switch (p->t.tr2->t.op) {
			case RSHIFT:
				if (p->t.type==UNSIGN)
					return(0);
				goto caseGEN;
			case TIMES:
				if (!ispow2(p->t.tr2))
					break;
				p->t.tr2 = pow2(p->t.tr2);
			case PLUS:
			case MINUS:
			case AND:
			case OR:
			case EXOR:
			case LSHIFT:
			caseGEN:
				p1 = p->t.tr2->t.tr2;
				if (xdcalc(p1, 16) > 12
				 || (p1->t.op==NAME
				     &&(p1->n.nloc==p->t.tr1->n.nloc
				        || p1->n.regno==p->t.tr1->n.nloc)))
					return(0);
				p1 = p->t.tr2;
				p->t.tr2 = p1->t.tr1;
				if (p1->t.tr1->t.op!=NAME
				 || p1->t.tr1->n.class!=REG
				 || p1->t.tr1->n.nloc!=p->t.tr1->n.nloc)
					rcexpr(p, efftab, reg);
				p->t.tr2 = p1->t.tr2;
				p->t.op = p1->t.op + ASPLUS - PLUS;
				*treep = p;
				return(1);
			}
			goto OK;

		case ASTIMES:
			if (!ispow2(p))
				return(0);
		case ASPLUS:
		case ASMINUS:
		case ASAND:
		case ASOR:
		case ASXOR:
		case INCBEF:
		case DECBEF:
		OK:
			if (table==cctab||table==cregtab)
				reg += 020;
			rcexpr(optim(p), efftab, ~reg);
			*treep = p1;
			return(1);
	}
	return(0);
}

/*
 * Delay handles postfix ++ and --
 * It observes that "x + y++" is better
 * treated as "x + y; y++".
 * If the operator is ++ or -- itself,
 * it calls rcexpr to load the operand, letting
 * the calling instance of rcexpr to do the
 * ++ using efftab.
 * Otherwise it uses sdelay to search for inc/dec
 * among the operands.
 */
int
delay(treep, table, reg)
union tree **treep;
struct table *table;
int reg;
{
	register union tree *p, *p1;
	register int r;

	p = *treep;
	if ((p->t.op==INCAFT||p->t.op==DECAFT)
	 && p->t.tr1->t.op==NAME) {
		r = p->t.tr1->n.class;
		/* Should all r == ... be in parens? */
		if (r == EXTERN || r == OFFS || (r == STATIC &&
				p->t.tr1->t.type == UNCHAR))
			return(1+rcexpr(p->t.tr1, table, reg));
		else
			return(1+rcexpr(paint(p->t.tr1, p->t.type), table,reg));
	}
	p1 = 0;
/*
 * typo fix, original code.
 *	if (opdope[p->t.op]&BINARY) {
 *		if (p->t.op==LOGAND || p->t.op==LOGOR
 *		 || p->t.op==QUEST || p->t.op==COLON || p->t.op==SEQNC)
 *			return(0);
 *		}
 *		p1 = sdelay(&p->t.tr2);
 *	if (p1==0)
 *		p1 = sdelay(&p->t.tr1);
 */
	if (opdope[p->t.op]&BINARY) {
		if (p->t.op==LOGAND || p->t.op==LOGOR
		 || p->t.op==QUEST || p->t.op==COLON || p->t.op==SEQNC)
			return(0);
		p1 = sdelay(&p->t.tr2);
	}
	if (p1==0)
		p1 = sdelay(&p->t.tr1);
	if (p1) {
		r = rcexpr(optim(p), table, reg);
		*treep = p1;
		return(r+1);
	}
	return(0);
}

union tree *
sdelay(ap)
union tree **ap;
{
	register union tree *p, *p1;

	if ((p = *ap)==TNULL)
		return(TNULL);
	if ((p->t.op==INCAFT||p->t.op==DECAFT) && p->t.tr1->t.op==NAME) {
		*ap = paint(ncopy(p->t.tr1), p->t.type);
		return(p);
	}
	if (p->t.op==STAR || p->t.op==PLUS)
		if ( (p1=sdelay(&p->t.tr1)) )
			return(p1);
	if (p->t.op==PLUS)
		return(sdelay(&p->t.tr2));
	return(0);
}

/*
 * Propagate possible implicit type-changing operation
 */
union tree *
paint(tp, type)
register union tree *tp;
register int type;
{

	if (tp->t.type==type)
		return(tp);
	if (tp->t.type==CHAR && type==INT)
		return(tp);
	if (tp->t.type==CHAR || tp->t.type==UNCHAR) {
		return(optim(tnode(LOAD, type, tp, TNULL)));
	}
	tp->t.type = type;
	if (tp->t.op==AMPER && type&XTYPE) {
		tp->t.tr1 = paint(tp->t.tr1, decref(type));
	} else if (tp->t.op==STAR) {
		tp->t.tr1 = paint(tp->t.tr1, incref(type));
	} else if (tp->t.op==ASSIGN) {
		paint(tp->t.tr1, type);
		paint(tp->t.tr2, type);
	}
	return(tp);
}

/*
 * Copy a tree node for a register variable.
 * Used by sdelay because if *reg-- is turned
 * into *reg; reg-- the *reg will in turn
 * be changed to some offset class, accidentally
 * modifying the reg--.
 */
union tree *
ncopy(p)
register union tree *p;
{
	register union tree *q;

	q = getblk(sizeof(struct xtname));
	q->n.op = p->n.op;
	q->n.type = p->n.type;
	q->n.class = p->n.class;
	q->n.regno = p->n.regno;
	q->n.offset = p->n.offset;
	if (q->n.class==EXTERN || q->n.class==XOFFS)
		q->x.name = p->x.name;
	else
		q->n.nloc = p->n.nloc;
	return(q);
}

/*
 * If the tree can be immediately loaded into a register,
 * produce code to do so and return success.
 */
int
chkleaf(tree, table, reg)
register union tree *tree;
struct table *table;
int reg;
{
	struct tnode lbuf;

	if (tree->t.op != STAR && dcalc(tree, nreg-reg) > 12) {
		return(-1);
	}
	lbuf.op = LOAD;
	lbuf.type = tree->t.type;
	lbuf.degree = tree->t.degree;
	lbuf.tr1 = tree;
	lbuf.tr2 = NULL;
//if (table == regtab)
//dumptree((union tree *)&lbuf, 0, 'L');
	return(rcexpr((union tree *)&lbuf, table, reg));
}

/*
 * Compile a function argument.
 * If the stack is currently empty, put it in (sp)
 * rather than -(sp); this will save a pop.
 * Return the number of bytes pushed,
 * for future popping.
 */
int
comarg(tree, flagp)
register union tree *tree;
int *flagp;
{
	register int retval;
	int size;

#if 0
	if (tree->t.op==STRASG) {
		size = tree->F.mask;
		tree = tree->t.tr1;
		tree = strfunc(tree);
		if (size <= SZPTR) {
			paint(tree, INT);
			goto normal;
		}
		if (size <= SZLONG) {
			paint(tree, LONG);
			goto normal;
		}
		// TODO: silently convert to pointer?
		// or convert to memcpy(...)?
		error("Unimplemented structure assignment");
		return(0);
	}

normal:
#endif
	if (nstack || isfloat(tree) || tree->t.type==LONG || tree->t.type==UNLONG) {
		rcexpr(tree, sptab, RSTART);
		retval = arlength(tree->t.type);
	} else {
		(*flagp)++;
		rcexpr(tree, lsptab, RSTART);
		retval = SZPTR;
	}
	return(retval);
}

#if 0
union tree *
strfunc(tp)
register union tree *tp;
{
	if (tp->t.op != CALL)
		return(tp);
	paint(tp, STRUCT+PTR);
	return(tnode(STAR, STRUCT, tp, TNULL));
}
#endif

/*
 * Compile an initializing expression
 */
void
doinit(type, tree)
register int type;
register union tree *tree;
{
	double fval;
	long lval;

	if (type==CHAR || type==UNCHAR) {
		printf(".byte ");
		if (tree->t.type&XTYPE)
			goto illinit;
		type = INT;
	}
	if (type&XTYPE)
		type = INT;
	switch (type) {
	case INT:
	case UNSIGN:
		if (tree->t.op==FTOI) {
			if (tree->t.tr1->t.op!=FCON && tree->t.tr1->t.op!=SFCON)
				goto illinit;
			// TODO: plug in to shared constants
			tree = tree->t.tr1;
			tree->c.value = tree->f.fvalue;
			tree->t.op = CON;
		} else if (tree->t.op==LTOI) {
			if (tree->t.tr1->t.op!=LCON)
				goto illinit;
			// TODO: plug in to shared constants
			tree = tree->t.tr1;
			lval = tree->l.lvalue;
			tree->t.op = CON;
			tree->c.value = lval;
		}
		if (tree->t.op == CON)
			printf(".bin 0x%x#4\n", tree->c.value);
		else if (tree->t.op==AMPER) {
			printf("\t.word\t");
			pname(tree, 0, 2); // force "real var"
			putchar('\n');
		} else
			goto illinit;
		return;
	case DOUBLE:
	case FLOAT:
		if (tree->t.op==ITOF) {
			if (tree->t.tr1->t.op==CON) {
				fval = tree->t.tr1->c.value;
			} else
				goto illinit;
		} else if (tree->t.op==FCON || tree->t.op==SFCON)
			fval = tree->f.fvalue;
		else if (tree->t.op==LTOF) {
			if (tree->t.tr1->t.op!=LCON)
				goto illinit;
			fval = tree->t.tr1->l.lvalue;
		} else
			goto illinit;
		printf(".float %.12e\n", fval);
		return;
	case UNLONG:
	case LONG:
		if (tree->t.op==FTOL) {
			tree = tree->t.tr1;
			if (tree->t.op==SFCON)
				tree->t.op = FCON;
			if (tree->t.op!= FCON)
				goto illinit;
			lval = tree->f.fvalue;
		} else if (tree->t.op==ITOL) {
			if (tree->t.tr1->t.op != CON)
				goto illinit;
			if (uns(tree->t.tr1))
				lval = (unsigned)tree->t.tr1->c.value;
			else
				lval = tree->t.tr1->c.value;
		} else if (tree->t.op==LCON)
			lval = tree->l.lvalue;
		else
			goto illinit;
		printf(".bin 0x%lx#8\n", tree->l.lvalue);
		return;
	}
illinit:
	error("Illegal initialization");
}

void
movreg(r0, r1, tree)
int r0, r1;
union tree *tree;
{
	register char *s;
	char c;

	if (r0 == r1) {
		return;
	}
	printf("lca\tx%d,x%d\n", r0, r1);
}
