#include <stdio.h>
#include "as.h"

SEGMNT text, data, bss, comm;
int hsize;

SYMBOL *curlab;		/* current label */
SEGMNT *curseg;		/* current segment */
SYMBOL *curcomm;	/* current COMN or STRUC name */
int	currel;		/* current relocatability */
int	ifcount;	/* count of open .if's */

#include <setjmp.h>
extern jmp_buf err_jmp;
extern void sym_reset();

static int pnc;

void do_pseudo(int op) {
	int t, count;
	char *s;
	int c, b;
	EXPR reg;

	switch (op) {

	case PIF:
		t = expr();
		if (t != RABS) {
			cerror(errv);
		}
		if (res.val!=0) {
			ifcount++;
			break;
		}
		putline(0);
		count = 1;
		while (count > 0) {
			t = tok(0);
			if (t == EOL && !get_line() ) {
				cerror(erri);
				count = 0;
			}
			if (t != IDENT || symlook(0) == 0) continue;
			switch (cursym->value & OP_MSK) {
			case PIF:	count++; break;
			case PENDIF:	count--; break;
			}
		}
		break;

	case PENDIF:
		if (--ifcount < 0) cerror(erri);
		break;

	case PLINE:
		t = expr();
		snprintf(symbuf, 8, "~~%d", res.val);
		if (!symlook(2)) {
			cursym->value = curseg->loc;
			cursym->type = STEXT;
		}
		putline(0);
		// TODO: adjust concept of current line?
		// line = t;
		break;

	case PERR:
	case PWARN:
		t = tok(0);
		if (t != STRING) {
			cerror(errv);
		}
		putline(0);
		serror((char *)conbuf);
		if (op == PERR) {
			++errcnt;
		} else {
			nexttoken = 0;
			longjmp(err_jmp, 0);
		}
		break;

	case PDATA:
		curseg = &data;
		currel = RDATA;
		nexttoken = EOL;
		break;

	case PTEXT:
		curseg = &text;
		currel = RTEXT;
		nexttoken = EOL;
		break;

	case PBSS:
		curseg = &bss;
		currel = RBSS;
		nexttoken = EOL;
		break;

	case PSTRING:	// [punc] "ascii-string"
		pnc = punct[0]; // TODO: what default?
		check_punc(&pnc);
		t = scanstr(pnc);
		break;

	case PWORD:	// [punc] addr-expr
		pnc = punct[0]; // TODO: what default?
		check_punc(&pnc);
		t = parse_addr(token(), &reg, 0);
		putaddr(reg.val, reg.rel, pnc);
		break;

	case PFLOAT:	// [punc] fp-const
		pnc = punct[0]; // TODO: what default?
		check_punc(&pnc);
		t = scanfp(pnc);
		break;

	case PDEC:	// [punc] bcd-digits
		pnc = punct[0]; // TODO: what default?
		check_punc(&pnc);
		t = scanbcd(pnc);
		break;

	case PBIN:	// [punc] arb-len-number
		pnc = punct[0]; // TODO: what default?
		check_punc(&pnc);
		t = scan_bin(pnc);
		break;

	case PBYTE:	// [punc] char-expr...
		// punctuation in listing is difficult...
		c = 0;
		do {
			t = expr();
			if (pass_gen) {
				// 'res.val' is *unsigned*
				b = res.val;
				if (t != RABS || b < -127 || b > 255) {
					cerror(errv);
				}
			}
			if (!c) {
				pnc = (res.val & 0300) << 8;
			} else {
				pnc |= (res.val & 0300);
			}
			putb(res.val, 1);
			++c;
		} while ((t = token()) == COMMA);
		nexttoken = t;
		break;

	case PHEAP:
		t = expr();
		if (t != RABS || res.val < 0) { // TODO: maximum?
			cerror(errv);
		}
		hsize += res.val;
		break;

	case PSPACE:
		t = expr();
		if (t != RABS || res.val < 0) { // TODO: maximum?
			cerror(errv);
		}
		if (curseg == &text || curseg == &data) {
			b = res.val;
			for (c = 0; c < b; ++c) {
				putb(0, 0);
			}
		} else {
			curseg->loc += res.val;
		}
		break;

	case PADMODE:
		t = expr();
		if (t != RABS || res.val < 2 || res.val > 4) {
			cerror(errv);
		}
		admode = res.val;
		break;

	case PCOMM:
		if (tok(1) != IDENT) {
			cerror(errx);
		}
		curlab = cursym;
		cursym->type |= SEXT;

		if (tok(1) != COMMA) {
			cerror(errx);
		}
		expr();
		cursym->value = res.val;
		break;

	case PGLOBL:
		do {
			if ((t = tok(1)) != IDENT) {
				break;
			} else {
				cursym->type |= SEXT;
			}
		} while ((t = token()) == COMMA);
		nexttoken = t;
		break;

	}
}

static void do_expr_or_assign() {
	int t,t2;

	/* first check for assignment */
	sscan();
	t  = token();
	t2 = token();

	/* if not, backtack and assemble an expression */
	if (t2 != EQU) {
		rscan();
		expr();
		putaddr(res.val, res.rel, 0);
		return;
	}

	switch (t) {

	case DOT:
		switch (t = expr()) {

			case RTEXT:
				curseg = &text;
				break;

			case RDATA:
				curseg = &data;
				break;

			default:
				if (t == currel)
					break;
				cerror(errr);
				t = RDATA;
		}
		currel = t;
		org(res.val);
		break;

	case IDENT:
		curlab = cursym;
		expr();
		deflab(res.rel, res.val);
		break;

	default:
		xerror(errx);
	}
}

#define before(t) \
	(curseg == &text && !(curlab->type & SREV) || \
	curseg != &text && (curlab->type & SREV) || \
	t == EOL || t == COMMENT)

#define after (curlab && (curseg == &text && (curlab->type & SREV) || \
		    curseg != &text && !(curlab->type & SREV)))

int assemble()
{
	register int t, op;
	register int optype;
	int start_loc;
	int nlab, nlabn;

	/*
	 * Initialize for assembly pass
	 */
	nextfile();
	seginit();
	nlabinit();

	setjmp(err_jmp);
	for (; get_line();putline(pnc)) {
		pnc = 0;
		nlab = 0;
		start_loc = curseg->loc; // TODO: what if curseg changes?

		/* list line address */
		if (pass_lst)
			lstloc();

next_stmt:
		/* skip comments & EOL marker */
		if ((t = tok(0)) == COMMENT || t == EOL)
			continue;

		/* save statement label symbol to be defined below */
		curlab = NULL;
		// TODO: allow 'curlab' to be valid for this?
		//	label:
		//		op foo,bar

		while (t == LABEL || t == NLABEL) {
			if (t == LABEL) {
				// value might change later...
				symlook(2);
				curlab = cursym;
				t = tok(0);
				if (before(t)) {
					deflab(currel, curseg->loc);
				}
			} else {
				if (!symrev) {
					defnlab(conbuf, currel, curseg->loc);
				} else {
					nlab = 1;
					nlabn = conbuf;
				}
				t = tok(0);
			}
		}

		switch (t) {

		case EOL:
		case COMMENT:
			// 'before' handled this case
			continue;

		case SEMI:
			// TODO: define deferred label here, also?
			goto next_stmt;

		// TODO: eliminate all assembling of "loose" items
		case STRING:
			putstr(strsiz);
			goto next_stmt;

		case IDENT:
			if (symlook(0)) {
				optype = RABS;
				op     = cursym->value;
				break;
			};
			symlook(2);

		default:
			nexttoken = t;
			do_expr_or_assign();
			goto next_stmt;
		}

		/* call opcode routine to parse the operands */
		if (op & P_OP) {
			do_pseudo(op & OP_MSK);
		} else {
			pnc = WM << 8;
			do_machine(op);
		}
		// done with "statement"
		int l = curseg->loc - 1;
		if (l < start_loc) l = start_loc;
		if (nlab) {
			defnlab(nlabn, currel, l);
		} else if (after) {
			deflab(currel, l);
			curlab->len = start_loc - curseg->loc; // negative
		} else if (curlab) {
			curlab->len = curseg->loc - start_loc; // positive
		}
		goto next_stmt;

	}
	sym_reset();
	return(errcnt);
}
