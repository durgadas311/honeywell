/*
 * C compiler part 2 -- expression optimizer
 */
#include "c1.h"
#include <sys/param.h>		/* for MAX */

union tree *
optim(tree)
register union tree *tree;
{
	register int op, dope;
	int d1, d2;
	union tree *t;

	if (tree==NULL)
		return(NULL);
	if ((op = tree->t.op)==0)
		return(tree);
	if (op==NAME && tree->n.class==AUTO) {
		tree->n.class = OFFS;
		tree->n.regno = BPREG;
		tree->n.offset = tree->n.nloc;
	}
	dope = opdope[op];
	if ((dope&LEAF) != 0) {
		return(tree);
	}
	if ((dope&BINARY) == 0) {
		union tree *u = unoptim(tree);
		return(u);
	}

	/* is known to be binary */
	if (tree->t.type==CHAR)
		tree->t.type = INT;
	switch(op) {
	case ASAND:
		// TODO: what is needed here?
		break;

	/*
	 * On the PDP-11, int->ptr via multiplication
	 * Longs are just truncated.
	 */
	case LTOP:
		tree->t.op = ITOP;
		tree->t.tr1 = unoptim(tnode(LTOI,INT,tree->t.tr1, TNULL));
	case ITOP:
		tree->t.op = TIMES;
		break;

	case DECAFT:
	case DECBEF:
	case ASMINUS:
	case MINUS:
		if ((t = isconstant(tree->t.tr2)) && (!uns(t) || tree->t.type!=LONG)
		 && (t->t.type!=INT || t->c.value!=0100000)) {
			tree->t.op += PLUS-MINUS;
			if (t->t.type==DOUBLE)
				/* PDP-11 FP representation */
				t->c.value ^= 0100000;
			else
				t->c.value = -t->c.value;
		}

		/* On the TI990 addresses of static variables are handled as
		 * constants, and generate immediate instructions. However,
		 * there is no 'si' instruction, only 'ai'. As we cannot negate
		 * the address at compile time (it only becomes known at final
		 * link time), we use a special MINSTAT operator for this corner
		 * case.
		 * If the address is scaled, there is a ITOL in between; ignore this.
		 */
		t = tree->t.tr2;
		if (t->t.op==ITOL)
			t = t->t.tr1;
		if (t->t.op==AMPER && t->t.tr1->t.op==NAME) {
			t = t->t.tr1;
			if (t->n.class==EXTERN || t->n.class==STATIC)
				tree->t.op = MINSTAT;
		}
		break;
	}

	op = tree->t.op;
	dope = opdope[op];
	if (dope&LVALUE && tree->t.tr1->t.op==FSEL)
		return(lvfield(tree));
	if ((dope&COMMUTE)!=0) {
		d1 = tree->t.type;
		tree = acommute(tree);
		if (tree->t.op == op)
			tree->t.type = d1;
		return(tree);
	}

	tree->t.tr1 = optim(tree->t.tr1);
	tree->t.tr2 = optim(tree->t.tr2);
	if (tree->t.type == LONG || tree->t.type==UNLONG) {
		t = lconst(tree->t.op, tree->t.tr1, tree->t.tr2);
		if (t)
			return(t);
	}
	if ((dope&RELAT) != 0) {
		/* Keep the most complex side on the left */
		if ((d1=degree(tree->t.tr1)) < (d2=degree(tree->t.tr2))
		 || (d1==d2 && tree->t.tr1->t.op==NAME && tree->t.tr2->t.op!=NAME)) {
			t = tree->t.tr1;
			tree->t.tr1 = tree->t.tr2;
			tree->t.tr2 = t;
			tree->t.op = maprel[op-EQUAL];
		}
		if (tree->t.tr1->t.type==CHAR && tree->t.tr2->t.op==CON
		 && (dcalc(tree->t.tr1, 0) <= 12 || tree->t.tr1->t.op==STAR)
		 && tree->t.tr2->c.value <= 127 && tree->t.tr2->c.value >= 0)
			tree->t.tr2->t.type = CHAR;
	}
	d1 = MAX(degree(tree->t.tr1), islong(tree->t.type));
	d2 = MAX(degree(tree->t.tr2), 0);
	switch (op) {

	/*
	 * In assignment to fields, treat all-zero and all-1 specially.
	 */
	case FSELA:
		if (tree->t.tr2->t.op==CON && tree->t.tr2->c.value==0) {
			tree->t.op = ASAND;
			tree->t.tr2->c.value = ~tree->F.mask;
			return(optim(tree));
		}
		if (tree->t.tr2->t.op==CON && tree->F.mask==tree->t.tr2->c.value) {
			tree->t.op = ASOR;
			return(optim(tree));
		}

	case LTIMES:
	case LDIV:
	case LMOD:
	case LASTIMES:
	case LASDIV:
	case LASMOD:
	case ULASMOD:
	case ULTIMES:
	case ULDIV:
	case ULMOD:
	case ULASTIMES:
	case ULASDIV:
		tree->t.degree = 10;
		break;

	case CALL:
		tree->t.degree = 10;
		break;

	case QUEST:
	case COLON:
		tree->t.degree = MAX(d1, d2);
		break;

	case PTOI:
	case DIVIDE:
	case ASDIV:
	case ASTIMES:
		if (tree->t.tr2->t.op==CON && tree->t.tr2->c.value==1) {
			if (op==PTOI)
				return(optim(tnode(LTOI,INT,paint(tree->t.tr1,LONG), TNULL)));
			return(paint(tree->t.tr1, tree->t.type));
		}
	case MOD:
	case ASMOD:
		if ((uns(tree->t.tr1) || tree->t.op==PTOI) && ispow2(tree))
			return(pow2(tree));
		if ((op==MOD||op==ASMOD) && tree->t.type==DOUBLE) {
			error("Floating %% not defined");
			tree->t.type = INT;
		}

		d1 += 2 + regpanic;
		d2 += 2 + regpanic;
		panicposs++;
		if (tree->t.type==LONG || tree->t.type==UNLONG)
			return(hardlongs(tree));

		/* Separate out unsigned division and modulo. If the divisor is
		 * known to be positive, there is no difference between 'div' and
		 * 'divs', and the unsigned 'div' compiles to better code.
		 */
		if ((op==MOD || op==DIVIDE || op==ASMOD || op==ASDIV)
		 && (uns(tree->t.tr1) || uns(tree->t.tr2)
		 || (tree->t.tr2->t.op==CON && tree->t.tr2->c.value>=1))) {
			if (op>=ASDIV) {
				tree->t.op += ASUDIV - ASDIV;
			} else
				tree->t.op += UDIV - DIVIDE;
		}
		/* If we're still signed, recognise that signed divide is a
		 * runtime subroutine.
		 */
		if (op==MOD || op==DIVIDE || op==ASMOD || op==ASDIV)
			d1 = d2 = 10;
		goto constant;

	case ASPLUS:
	case ASMINUS:
		if (tree->t.tr2->t.op==CON && tree->t.tr2->c.value==0)
			return(tree->t.tr1);
		goto def;

	case RSHIFT:
	case ASRSH:
		if (uns(tree->t.tr1))
			tree->t.op = (tree->t.op==RSHIFT) ? URSH : ASURSH;
		/* fall through */

	case LSHIFT:
	case ASLSH:
		if (tree->t.tr2->t.op==CON && tree->t.tr2->c.value==0)
			return(paint(tree->t.tr1, tree->t.type));

	constant:
		if (tree->t.tr1->t.op==CON && tree->t.tr2->t.op==CON) {
			pconst(op, &tree->t.tr1->c.value, tree->t.tr2->c.value, tree->t.type);
			return(tree->t.tr1);
		}


	def:
	default:
		if (dope&RELAT) {
			if (tree->t.tr1->t.type==LONG || tree->t.tr1->t.type==UNLONG)	/* long relations are a mess */
				d1 = 10;
			if (opdope[tree->t.tr1->t.op]&RELAT && tree->t.tr2->t.op==CON
			 && tree->t.tr2->c.value==0) {
				tree = tree->t.tr1;
				switch(op) {
				case GREATEQ:
					return(cone);
				case LESS:
					return(czero);
				case LESSEQ:
				case EQUAL:
					tree->t.op = notrel[tree->t.op-EQUAL];
				}
				return(tree);
			}
		}
		tree->t.degree = d1==d2? d1+islong(tree->t.type): MAX(d1, d2);
		break;
	}
	return(tree);
}

union tree *
unoptim(tree)
register union tree *tree;
{
	register union tree *subtre, *p;

	if (tree==NULL)
		return(NULL);
    again:
	if (tree->t.op==AMPER && tree->t.tr1->t.op==STAR) {
		subtre = tree->t.tr1->t.tr1;
		subtre->t.type = tree->t.type;
		return(optim(subtre));
	}
	subtre = tree->t.tr1 = optim(tree->t.tr1);
	switch (tree->t.op) {

	case INCAFT:
	case DECAFT:
		if (tree->t.type!=subtre->t.type)
			paint(subtre, tree->t.type);
		break;

	case ITOL:
		if (subtre->t.op==CON && subtre->t.type==INT && subtre->c.value<0) {
			subtre = getblk(sizeof(struct lconst));
			subtre->l.op = LCON;
			subtre->l.type = LONG;
			subtre->l.lvalue = tree->t.tr1->c.value;
			return(subtre);
		}
		break;
	case FTOI:
		if (uns(tree)) {
			tree->t.op = FTOL;
			tree->t.type = LONG;
			tree = tnode(LTOI, UNSIGN, tree, TNULL);
		}
		break;

	case LTOF:
		if (subtre->t.op==LCON) {
			tree = getblk(sizeof(struct ftconst));
			tree->f.op = FCON;
			tree->f.type = DOUBLE;
			tree->f.fvalue = subtre->l.lvalue;
			return(optim(tree));
		}
		if (subtre->t.type==UNLONG)
			tree->t.op = ULTOF;
		break;

	case ITOF:
		if (subtre->t.op==CON) {
			tree = getblk(sizeof(struct ftconst));
			tree->f.op = FCON;
			tree->f.type = DOUBLE;
			if (uns(subtre))
				tree->f.fvalue = (unsigned)subtre->c.value;
			else
				tree->f.fvalue = subtre->c.value;
			return(optim(tree));
		}
		if (uns(subtre)) {
			tree->t.tr1 = tnode(ITOL, LONG, subtre, TNULL);
			tree->t.op = LTOF;
			return(optim(tree));
		}
		break;
	case ITOC:
		/*
		 * Sign-extend PDP-11 characters
		 */
		if (subtre->t.op==CON) {
			char c;
			c = subtre->c.value;
			subtre->c.value = c;
			subtre->c.type = tree->t.type;
			return(subtre);
		} else if (subtre->t.op==NAME && tree->t.type==INT) {
			subtre->t.type = CHAR;
			return(subtre);
		}
		break;

	case LTOI:
		switch (subtre->t.op) {

		case LCON:
			// TODO: plug in to shared constants
			subtre->t.op = CON;
			subtre->t.type = tree->t.type;
			subtre->c.value = subtre->l.lvalue;
			return(subtre);

		case NAME:
			subtre->n.offset += SZINT;
			subtre->t.type = tree->t.type;
			return(subtre);

		case STAR:
			subtre->t.type = tree->t.type;
			subtre->t.tr1->t.type = tree->t.type+PTR;
			subtre->t.tr1 = tnode(PLUS, tree->t.type, subtre->t.tr1,
							tconst(SZINT, INT, 0));
			return(optim(subtre));

		case ITOL:
			return(paint(subtre->t.tr1, tree->t.type));

		case PLUS:
		case MINUS:
		case AND:
		case OR:
		case EXOR:
			subtre->t.tr2 = tnode(LTOI, tree->t.type, subtre->t.tr2, TNULL);
		case NEG:
		case COMPL:
			subtre->t.tr1 = tnode(LTOI, tree->t.type, subtre->t.tr1, TNULL);
			subtre->t.type = tree->t.type;
			return(optim(subtre));
		}
		break;

	case FSEL:
		tree->t.op = AND;
		tree->t.tr1 = tree->t.tr2->t.tr1;
		tree->t.tr2->t.tr1 = subtre;
		tree->t.tr2->t.op = RSHIFT;
		tree->t.tr1->c.value = (1 << tree->t.tr1->c.value) - 1;
		return(optim(tree));

	case FSELR:
		tree->t.op = LSHIFT;
		tree->t.type = UNSIGN;
		tree->t.tr1 = tree->t.tr2;
		tree->t.tr1->t.op = AND;
		tree->t.tr2 = tree->t.tr2->t.tr2;
		tree->t.tr1->t.tr2 = subtre;
		tree->t.tr1->t.tr1->c.value = (1 << tree->t.tr1->t.tr1->c.value) -1;
		return(optim(tree));

	case AMPER:
		if (subtre->t.op==STAR)
			return(subtre->t.tr1);
		if (subtre->t.op==NAME && subtre->n.class == OFFS) {
			int t = tree->t.type;
			tree->t.degree = 0;
			tree = tconst(subtre->n.offset, INT, 0);
			p = tnode(PLUS, t, subtre, tree);
			subtre->t.type = t;
#if 0
			tree->t.op = CON;
			tree->t.type = INT;
			tree->c.value = subtre->n.offset;
#endif
			subtre->n.class = REG;
			subtre->n.nloc = subtre->n.regno;
			subtre->n.offset = 0;
			return(optim(p));
		}
		if (subtre->t.op==LOAD) {
			tree->t.tr1 = subtre->t.tr1;
			goto again;
		}
		break;

	case STAR:
		if (subtre->t.op==AMPER) {
			subtre->t.tr1->t.type = tree->t.type;
			return(subtre->t.tr1);
		}
		if (tree->t.type==STRUCT)
			break;
		if (subtre->t.op==NAME && subtre->n.class==REG) {
			// never entered on H200
			subtre->t.type = tree->t.type;
			subtre->n.class = OFFS;
			subtre->n.regno = subtre->n.nloc;
			return(subtre);
		}
		p = subtre->t.tr1;
		if ((subtre->t.op==INCAFT)
		 && tree->t.type!=LONG && tree->t.type!=UNLONG
		 && p->t.op==NAME && p->n.class==REG && p->t.type==subtre->t.type) {
			// won't be enetered on H200, so no AUTOI
			p->t.type = tree->t.type;
			p->t.op   = AUTOI;
			return(p);
		}
		if (subtre->t.op==PLUS && p->t.op==NAME && p->n.class==REG) {
			// never entered on H200?
			if (subtre->t.tr2->t.op==CON) {
				p->n.offset += subtre->t.tr2->c.value;
				p->n.class = OFFS;
				p->t.type = tree->t.type;
				p->n.regno = p->n.nloc;
				return(p);
			}
			if (subtre->t.tr2->t.op==AMPER) {
				subtre = subtre->t.tr2->t.tr1;
				subtre->n.class += XOFFS-EXTERN;
				subtre->n.regno = p->n.nloc;
				subtre->t.type = tree->t.type;
				return(subtre);
			}
		}
		if (subtre->t.op==MINUS && p->t.op==NAME && p->n.class==REG
		 && subtre->t.tr2->t.op==CON) {
			// never entered on H200
			p->n.offset -= subtre->t.tr2->c.value;
			p->n.class = OFFS;
			p->t.type = tree->t.type;
			p->n.regno = p->n.nloc;
			return(p);
		}
		break;

	case EXCLA:
		if ((opdope[subtre->t.op]&RELAT)==0)
			break;
		tree = subtre;
		tree->t.op = notrel[tree->t.op-EQUAL];
		break;

	case COMPL:
		if (tree->t.type==CHAR)
			tree->t.type = INT;
		if (tree->t.op == subtre->t.op)
			return(paint(subtre->t.tr1, tree->t.type));
		if (subtre->t.op==CON) {
			subtre->c.value = ~subtre->c.value;
			return(paint(subtre, tree->t.type));
		}
		if (subtre->t.op==LCON) {
			subtre->l.lvalue = ~subtre->l.lvalue;
			return(subtre);
		}
		if (subtre->t.op==ITOL) {
			if (subtre->t.tr1->t.op==CON) {
				tree = getblk(sizeof(struct lconst));
				tree->t.op = LCON;
				tree->t.type = LONG;
				if (uns(subtre->t.tr1))
					tree->l.lvalue = ~(long)(unsigned)subtre->t.tr1->c.value;
				else
					tree->l.lvalue = ~subtre->t.tr1->c.value;
				return(tree);
			}
			if (uns(subtre->t.tr1))
				break;
			subtre->t.op = tree->t.op;
			subtre->t.type = subtre->t.tr1->t.type;
			tree->t.op = ITOL;
			tree->t.type = LONG;
			goto again;
		}

	case NEG:
		if (tree->t.type==CHAR)
			tree->t.type = INT;
		if (tree->t.op==subtre->t.op)
			return(paint(subtre->t.tr1, tree->t.type));
		if (subtre->t.op==CON) {
			subtre->c.value = -subtre->c.value;
			return(paint(subtre, tree->t.type));
		}
		if (subtre->t.op==LCON) {
			subtre->l.lvalue = -subtre->l.lvalue;
			return(subtre);
		}
		if (subtre->t.op==ITOL && subtre->t.tr1->t.op==CON) {
			tree = getblk(sizeof(struct lconst));
			tree->t.op = LCON;
			tree->t.type = LONG;
			if (uns(subtre->t.tr1))
				tree->l.lvalue = -(long)(unsigned)subtre->t.tr1->c.value;
			else
				tree->l.lvalue = -subtre->t.tr1->c.value;
			return(tree);
		}
#ifndef __ti990__
		/*
		 * PDP-11 FP negation
		 */
		if (subtre->t.op==SFCON) {
			subtre->c.value ^= 0100000;
			subtre->f.fvalue = -subtre->f.fvalue;
			return(subtre);
		}
		if (subtre->t.op==FCON) {
			subtre->f.fvalue = -subtre->f.fvalue;
			return(subtre);
		}
#endif
	}
	if ((opdope[tree->t.op]&LEAF)==0)
		tree->t.degree = MAX(islong(tree->t.type), degree(subtre));
	return(tree);
}

/*
 * Deal with assignments to partial-word fields.
 * The game is that select(x) += y turns into
 * select(x += select(y)) where the shifts and masks
 * are chosen properly.  The outer select
 * is discarded where the value doesn't matter.
 * Sadly, overflow is undetected on += and the like.
 * Pure assignment is handled specially.
 */

union tree *
lvfield(t)
register union tree *t;
{
	register union tree *t1, *t2;

	switch (t->t.op) {

	case ASSIGN:
		t2 = getblk(sizeof(struct fasgn));
		t2->t.op = FSELA;
		t2->t.type = UNSIGN;
		t1 = t->t.tr1->t.tr2;
		t2->F.mask = ((1<<t1->t.tr1->c.value)-1)<<t1->t.tr2->c.value;
		t2->t.tr1 = t->t.tr1;
		t2->t.tr2 = t->t.tr2;
		t = t2;

	case ASPLUS:
	case ASMINUS:
	case ASOR:
	case ASXOR:
	case INCBEF:
	case INCAFT:
	case DECBEF:
	case DECAFT:
		t1 = t->t.tr1;
		t1->t.op = FSELR;
		t->t.tr1 = t1->t.tr1;
		t1->t.tr1 = t->t.tr2;
		t->t.tr2 = t1;
		t1 = t1->t.tr2;
		t1 = tnode(COMMA, INT, tconst(t1->t.tr1->c.value, INT, 0),
			tconst(t1->t.tr2->c.value, INT, 0));
		return(optim(tnode(FSELT, UNSIGN, t, t1)));

	}
	error("Unimplemented field operator");
	return(t);
}

#define	LSTSIZ	20
struct acl {
	int nextl;
	int nextn;
	union tree *nlist[LSTSIZ];
	union tree *llist[LSTSIZ+1];
};

union tree *
acommute(tree)
register union tree *tree;
{
	struct acl acl;
	int d, i, op, flt, d1, type;
	register union tree *t1, **t2;
	union tree *t;

	acl.nextl = 0;
	acl.nextn = 0;
	op = tree->t.op;
	type = tree->t.type;
	flt = isfloat(tree);
	insert(op, tree, &acl);
	acl.nextl--;
	t2 = &acl.llist[acl.nextl];
	if (!flt) {
		/* put constants together */
		for (i=acl.nextl; i>0; i--) {
			d = t2[-1]->t.type==UNSIGN||t2[0]->t.type==UNSIGN?UNSIGN:INT;
			if (t2[0]->t.op==CON && t2[-1]->t.op==CON) {
				acl.nextl--;
				t2--;
				pconst(op, &t2[0]->c.value, t2[1]->c.value, d);
				t2[0]->t.type = d;
			} else if ( (t = lconst(op, t2[-1], t2[0])) ) {
				acl.nextl--;
				t2--;
				t2[0] = t;
			}
		}
	}
	if (op==PLUS || op==OR) {
		/* toss out "+0" */
		if (acl.nextl>0 && ( ((t1 = isconstant(*t2)) && t1->c.value==0)
		 || ((*t2)->t.op==LCON && (*t2)->l.lvalue==0))) {
			acl.nextl--;
			t2--;
		}
		if (acl.nextl <= 0) {
			if ((*t2)->t.type==CHAR || (*t2)->t.type==UNCHAR)
				*t2 = tnode(LOAD, tree->t.type, *t2, TNULL);
			(*t2)->t.type = tree->t.type;
			return(*t2);
		}
		/* subsume constant in "&x+c" */
#if 0	// can't make this optimization? e.g. "lca 40+.buf,0(x1)"
		if (op==PLUS && t2[0]->t.op==CON && t2[-1]->t.op==AMPER) {
			t2--;
fprintf(stderr, "PLUS CON AMPER offset += %d\n", t2[1]->c.value);
			t2[0]->t.tr1->n.offset += t2[1]->c.value;
			acl.nextl--;
		}
#endif
	} else if (op==TIMES || op==AND) {
		t1 = acl.llist[acl.nextl];
		if (t1->t.op==CON) {
			if (t1->c.value==0) {
				for (i=0; i<acl.nextl; i++)
					if (sideeffects(acl.llist[i]))
						break;
				if (i==acl.nextl)
					return(t1);
			}
			if (op==TIMES && t1->c.value==1 && acl.nextl>0)
				if (--acl.nextl <= 0) {
					t1 = acl.llist[0];
					if (uns(tree))
						paint(t1, tree->t.type);
					return(t1);
				}
		}
	}
	if (op==PLUS && !flt)
		distrib(&acl);
	tree = *(t2 = &acl.llist[0]);
	d = MAX(degree(tree), islong(tree->t.type));
	if (op==TIMES && !flt) {
		d += regpanic+1;
		panicposs++;
	}
	for (i=0; i<acl.nextl; i++) {
		t1 = acl.nlist[i];
		t1->t.tr2 = t = *++t2;
		d1 = degree(t);
		/*
		 * PDP-11 strangeness:
		 * rt. op of ^ must be in a register.
		 */
		if (op==EXOR && dcalc(t, 0)<=12) {
			t1->t.tr2 = t = optim(tnode(LOAD, t->t.type, t, TNULL));
			d1 = t->t.degree;
		}
		t1->t.degree = d = d==d1? d+islong(t1->t.type): MAX(d, d1);
		t1->t.tr1 = tree;
		tree = t1;
		if (tree->t.type==LONG || tree->t.type==UNLONG) {
			if (tree->t.op==TIMES)
				tree = hardlongs(tree);
			else if (tree->t.op==PLUS && (t = isconstant(tree->t.tr1))
			       && t->c.value < 0 && !uns(t)) {
				tree->t.op = MINUS;
				t->c.value = - t->c.value;
				t = tree->t.tr1;
				tree->t.tr1 = tree->t.tr2;
				tree->t.tr2 = t;
			}
		}
	}
	if (tree->t.op==TIMES && ispow2(tree))
		tree->t.degree = MAX(degree(tree->t.tr1), islong(tree->t.type));
	paint(tree, type);
	return(tree);
}

int
sideeffects(tp)
register union tree *tp;
{
	register int dope;

	if (tp==NULL)
		return(0);
	dope = opdope[tp->t.op];
	if (dope&LEAF) {
		if (tp->t.op==AUTOI || tp->t.op==AUTOD)
			return(1);
		return(0);
	}
	if (dope&ASSGOP)
		return(1);
	switch(tp->t.op) {
	case CALL:
	case FSELA:
	case STRASG:
		return(1);
	}
	if (sideeffects(tp->t.tr1))
		return(1);
	if (dope&BINARY)
		return(sideeffects(tp->t.tr2));
	return(0);
}

void
distrib(list)
struct acl *list;
{
/*
 * Find a list member of the form c1c2*x such
 * that c1c2 divides no other such constant, is divided by
 * at least one other (say in the form c1*y), and which has
 * fewest divisors. Reduce this pair to c1*(y+c2*x)
 * and iterate until no reductions occur.
 */
	register union tree **p1, **p2;
	union tree *t;
	int ndmaj, ndmin;
	union tree **dividend = 0, **divisor = 0;
	union tree **maxnod = 0, **mindiv = 0;

    loop:
	maxnod = &list->llist[list->nextl];
	ndmaj = 1000;
	dividend = 0;
	for (p1 = list->llist; p1 <= maxnod; p1++) {
		if ((*p1)->t.op!=TIMES || (*p1)->t.tr2->t.op!=CON)
			continue;
		ndmin = 0;
		for (p2 = list->llist; p2 <= maxnod; p2++) {
			if (p1==p2 || (*p2)->t.op!=TIMES || (*p2)->t.tr2->t.op!=CON)
				continue;
			if ((*p1)->t.tr2->c.value == (*p2)->t.tr2->c.value) {
				(*p2)->t.tr2 = (*p1)->t.tr1;
				(*p2)->t.op = PLUS;
				(*p1)->t.tr1 = (*p2);
				*p1 = optim(*p1);
				squash(p2, maxnod);
				list->nextl--;
				goto loop;
			}
			if (((*p2)->t.tr2->c.value % (*p1)->t.tr2->c.value) == 0)
				goto contmaj;
			if (((*p1)->t.tr2->c.value % (*p2)->t.tr2->c.value) == 0) {
				ndmin++;
				mindiv = p2;
			}
		}
		if (ndmin > 0 && ndmin < ndmaj) {
			ndmaj = ndmin;
			dividend = p1;
			divisor = mindiv;
		}
    contmaj:;
	}
	if (dividend==0)
		return;
	t = list->nlist[--list->nextn];
	p1 = dividend;
	p2 = divisor;
	t->t.op = PLUS;
	t->t.type = (*p1)->t.type;
	t->t.tr1 = (*p1);
	t->t.tr2 = (*p2)->t.tr1;
	(*p1)->t.tr2->c.value /= (*p2)->t.tr2->c.value;
	(*p2)->t.tr1 = t;
	t = optim(*p2);
	if (p1 < p2) {
		*p1 = t;
		squash(p2, maxnod);
	} else {
		*p2 = t;
		squash(p1, maxnod);
	}
	list->nextl--;
	goto loop;
}

void
squash(p, maxp)
union tree **p, **maxp;
{
	register union tree **np;

	for (np = p; np < maxp; np++)
		*np = *(np+1);
}

void
pconst(op, vp, v, type)
int op, type;
register int *vp, v;
{
	switch (op) {

	case PTOI:
		(*vp) /= (unsigned)v;
		return;

	case PLUS:
		*vp += v;
		return;

	case TIMES:
		*vp *= v;
		return;

	case AND:
		*vp &= v;
		return;

	case OR:
		*vp |= v;
		return;

	case EXOR:
		*vp ^= v;
		return;

	case UDIV:
	case UMOD:
		type = UNSIGN;
	case DIVIDE:
	case MOD:
		if (type==UNSIGN && v!=0 && v<=1) {
			if (op==UDIV || op==DIVIDE) {
				if (v==1)
					return;
				*vp = *(unsigned *)vp >= (unsigned)v;
				return;
			} else {
				if (v==1) {
					*vp = 0;
					return;
				}
				if (*(unsigned *)vp >= (unsigned)v)
					*vp -= v;
				return;
			}
		}
		if (v==0)
			werror("divide check");
		else
			if (type==INT)
				if (op==DIVIDE || op==UDIV)
					*vp /= v;
				else
					*vp %= v;
			else
				if (op==DIVIDE || op==UDIV)
					*(unsigned *)vp /= (unsigned)v;
				else
					*(unsigned *)vp %= (unsigned)v;
			return;

	case URSH:
		type = UNSIGN;

	case RSHIFT:
	rshift:
		if (v<0) {
			v = -v;
			goto lshift;
		}
		if (type==INT)
			*vp >>= v;
		else
			*(unsigned *)vp >>= (unsigned)v;
		return;

	case LSHIFT:
	lshift:
		if (v<0) {
			v = -v;
			goto rshift;
		}
		if (type==INT)
			*vp <<= v;
		else
			*(unsigned *)vp <<= (unsigned)v;
		return;

	}
	error("C error: const");
}

union tree *
lconst(op, lp, rp)
int op;
register union tree *lp, *rp;
{
	long l, r;

	if (lp->t.op==LCON)
		l = lp->l.lvalue;
	else if (lp->t.op==ITOL && lp->t.tr1->t.op==CON) {
		if (lp->t.tr1->t.type==INT)
			l = lp->t.tr1->c.value;
		else
			l = (unsigned)lp->t.tr1->c.value;
	} else
		return(0);
	if (rp->t.op==LCON)
		r = rp->l.lvalue;
	else if (rp->t.op==ITOL && rp->t.tr1->t.op==CON) {
		if (rp->t.tr1->t.type==INT)
			r = rp->t.tr1->c.value;
		else
			r = (unsigned)rp->t.tr1->c.value;
	} else
		return(0);
	switch (op) {

	case PLUS:
		l += r;
		break;

	case MINUS:
		l -= r;
		break;

	case TIMES:
	case LTIMES:
		l *= r;
		break;

	case DIVIDE:
	case LDIV:
		if (r==0)
			error("Divide check");
		else
			l /= r;
		break;

	case MOD:
	case LMOD:
		if (r==0)
			error("Divide check");
		else
			l %= r;
		break;

	case AND:
		l &= r;
		break;

	case OR:
		l |= r;
		break;

	case EXOR:
		l ^= r;
		break;

	case LSHIFT:
		l <<= r;
		break;

	case RSHIFT:
		l >>= r;
		break;

	default:
		return(0);
	}
	if (lp->t.op==LCON) {
		lp->l.lvalue = l;
		return(lp);
	}
	lp = getblk(sizeof(struct lconst));
	lp->t.op = LCON;
	lp->t.type = LONG;
	lp->l.lvalue = l;
	return(lp);
}

void
insert(op, tree, list)
int op;
register union tree *tree;
register struct acl *list;
{
	register int d;
	int d1, i;
	union tree *t;

ins:
	if (tree->t.op != op)
		tree = optim(tree);
	if (tree->t.op == op && list->nextn < LSTSIZ-2) {
		list->nlist[list->nextn++] = tree;
		insert(op, tree->t.tr1, list);
		insert(op, tree->t.tr2, list);
		return;
	}
	if (!isfloat(tree)) {
		/* c1*(x+c2) -> c1*x+c1*c2 */
		if ((tree->t.op==TIMES||tree->t.op==LSHIFT)
		  && tree->t.tr2->t.op==CON && tree->t.tr2->c.value>0
		  && tree->t.tr1->t.op==PLUS && tree->t.tr1->t.tr2->t.op==CON) {
			d = tree->t.tr2->c.value;
			if (tree->t.op==TIMES)
				tree->t.tr2->c.value *= tree->t.tr1->t.tr2->c.value;
			else
				tree->t.tr2->c.value = tree->t.tr1->t.tr2->c.value << d;
			tree->t.tr1->t.tr2->c.value = d;
			tree->t.tr1->t.op = tree->t.op;
			tree->t.op = PLUS;
			tree = optim(tree);
			if (op==PLUS)
				goto ins;
		}
	}
	d = degree(tree);
	for (i=0; i<list->nextl; i++) {
		if ((d1=degree(list->llist[i]))<d) {
			t = list->llist[i];
			list->llist[i] = tree;
			tree = t;
			d = d1;
		}
	}
	list->llist[list->nextl++] = tree;
}

union tree *
tnode(op, type, tr1, tr2)
int op, type;
union tree *tr1, *tr2;
{
	register union tree *p;

	p = getblk(sizeof(struct tnode));
	p->t.op = op;
	p->t.type = type;
	p->t.degree = 0;
	p->t.tr1 = tr1;
	p->t.tr2 = tr2;
	return(p);
}

#define NSHARED	500
static struct consts {
	int	op;
	int	type;
	int	value;
	int	label;
} shared[NSHARED];
static int nconsts = 0;

static int masks[VOID+1] = {
[CHAR] = 077,
[UNCHAR] = 077,
[INT] = MAXUINT,
[UNSIGN] = MAXUINT,
};

static int sizes[VOID+1] = {
[CHAR] = SZCHAR,
[UNCHAR] = SZCHAR,
[INT] = SZINT,
[UNSIGN] = SZINT,
};

union tree *
tconst(val, type, stc)
int val, type, stc;
{
	return tconst0(CON, val, type, stc);
}

union tree *
tconst0(op, val, type, stc)
int op, val, type, stc;
{
	register union tree *p;

	if (stc) {
		p = malloc(sizeof(struct tconst));
	} else {
		p = getblk(sizeof(struct tconst));
	}
	p->c.op = op;
	p->c.type = type;
	p->c.value = val;
	return(p);
}

void prlab(int lab, int flag) {
	printf("%c%d", flag ? 'P' : 'L', lab);
}

void sprconlab(char *buf, int val, int op, int type) {
	int x;
	// TODO: any other type-collapsing?
	if (type == UNSIGN || (type & XTYPE) == PTR) {
		type = INT;
	}
	int msk = masks[type];

	if (op == CCON) {
		sprintf(buf, "@T%02X", val & 0x7f);
		return;
	}
	// must be 'CON'?
	if (type == INT) {
		if (val >= 0 && val <= 99999) {
			sprintf(buf, "@P%d", val);
		} else if (val < 0 && val >= -99999) {
			sprintf(buf, "@N%d", -val);
		} else {
			sprintf(buf, "@%06X", val & msk);
		}
		return;
	}
	// TODO: is this code reached?
	int lab = -1;
	for (x = 0; x < nconsts; ++x) {
		if (shared[x].op == op &&
				shared[x].type == type &&
		    		(shared[x].value & msk) == (val & msk)) {
			lab = shared[x].label;
			break;
		}
	}
	if (lab < 0) {
		lab = isn++;
		if (nconsts < NSHARED) {
			shared[nconsts].op = op;
			shared[nconsts].type = type;
			shared[nconsts].value = val;
			shared[nconsts].label = lab;
			++nconsts;
		}
	}
	sprintf(buf, "L%d", lab);
}

void prconlab(int val, int op, int type) {
	static char buf[16];
	sprconlab(buf, val, op, type);
	printf("%s", buf);
}

void prcons() {
	int x;
	// assume .data already...
	// TODO: Are these used anymore?
	for (x = 0; x < nconsts; ++x) {
		// fprintf(stderr, "mkconst: %d (0%o) L%d\n",
		// 	shared[x].value, shared[x].type, shared[x].label);
		int c = shared[x].value;
		if (shared[x].op == CCON && c >= ' ' && c <= '~') {
			if (c == '\\' || c == '\'') {
				printf(	"L%d:\t.bin\t'\\%c'#%d\n",
					shared[x].label, c,
					sizes[shared[x].type]);
			} else {
				printf(	"L%d:\t.bin\t'%c'#%d\n",
					shared[x].label, c,
					sizes[shared[x].type]);
			}
		} else {
			printf(	"L%d:\t.bin\t0x%x#%d\n",
				shared[x].label, c,
				sizes[shared[x].type]);
		}
	}
	nconsts = 0;
}

static char *Tblock_base = NULL;
static int Tblock_len = 65536;
static int Tblock_use = 0;

union tree *
resetblk() {
	Tblock_use = 0;
	return getblk(0);
}

union tree *
getblk(size)
int size;
{
	register union tree *p;

	if (size&01)
		size++;
	if (Tblock_base == NULL) {
		Tblock_base = (char *)malloc(Tblock_len);
		if (Tblock_base == NULL) {
			error("Out of memory-- c1");
			exit(1);
		}
	}
	if (Tblock_use + size > Tblock_len) {
		error("Out of space-- c1");
		exit(1);
	}
	p = (union tree *)(Tblock_base + Tblock_use);
	Tblock_use += size;
	return(p);
}

int
islong(t)
int t;
{
	if (t==LONG || t==UNLONG)
		return(2);
	return(1);
}

union tree *
isconstant(t)
register union tree *t;
{
	if (t->t.op==CON || t->t.op==SFCON)
		return(t);
	if (t->t.op==ITOL && t->t.tr1->t.op==CON)
		return(t->t.tr1);
	return(NULL);
}

union tree *
hardlongs(t)
register union tree *t;
{
	switch(t->t.op) {

	case TIMES:
	case DIVIDE:
	case MOD:
		if (t->t.type == UNLONG)
			t->t.op += ULTIMES-TIMES;
		else
			t->t.op += LTIMES-TIMES;
		break;

	case ASTIMES:
	case ASDIV:
	case ASMOD:
		if (t->t.type == UNLONG)
			t->t.op += ULASTIMES-ASTIMES;
		else
			t->t.op += LASTIMES-ASTIMES;
		t->t.tr1 = tnode(AMPER, LONG+PTR, t->t.tr1, TNULL);
		break;

	default:
		return(t);
	}
	return(optim(t));
}

/*
 * Is tree of unsigned type?
 */
int
uns(tp)
union tree *tp;
{
	register int t;

	t = tp->t.type;
	if (t==UNSIGN || t==UNCHAR || t==UNLONG || t&XTYPE)
		return(1);
	return(0);
}
