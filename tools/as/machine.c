/*
 *		Assembler Machine Instruction Processing
 */

#include "as.h"

int admode = 4;	// TODO: make this changeable? ".admode X" ?

/* evaluate an expression which is expected to return 1..15 */
static int regexpr()
{
	int r;

	r = expr();
	if (pass && (r != RABS || res.val > 15 || res.val == 0)) {
		cerror(errv);
	}
	return(res.val);
}

static void skip_comma()
{
	int t;

	while ((t=token())==SPACE) ;
	if (t!=COMMA)
		cerror(errs);
}

static int nextparm() {
	int t;
	while ((t = token()) == SPACE);
	if (t == COMMENT) t = EOL;
	if (t != EOL && t != COMMA) {
		cerror(errx);
	}
	if (t == COMMA) {
		t = token();
	}
	return t;
}

/* ident, ident(x1), (ident) */
int parse_addr(int t, EXPR *reg) {
	int nt;
	int xr;
	int rel;
	uint32_t val;
	while (t == SPACE) t = token();
	if (t == LPAREN) {
		// indirect addressing
		expr();
		if (token() != RPAREN) {
			cerror(errs);
		}
		rel = res.rel;
		val = res.val;
		switch (admode) {
		case 3:
			val |= 0700000;
			break;
		case 4:
			val |= 040000000;
			break;
		default:
			cerror(errx); // TODO: pick better error
		}
	} else {
		nexttoken = t;  // unget
		expr();
		rel = res.rel;
		val = res.val;
		nt = token();
		if (nt != LPAREN) {
			// direct addressing
			nexttoken = nt; // unget
		} else {
			// indexed addressing
			nt = token();
			if (nt != IDENT || !(cursym->type & (SIDX|SIDY))) {
				cerror(errx);
			}
			xr = cursym->value >> 2;
			if (token() != RPAREN) {
				cerror(errs);
			}
			if (cursym->type & SIDY) {
				xr |= 0b10000;
			}
			switch (admode) {
			case 3:
				if (xr > 6) {
					cerror(errx); // TODO: pick better error
				}
				val |= (xr << 15);
				break;
			case 4:
				val |= (xr << 19);
				break;
			default:
				cerror(errx); // TODO: pick better error
			}
		}
	}
	reg->val = val;
	reg->rel = rel;
	nt = nextparm();
	return nt;
}

static int parse_var(int t) {
	if (t != CON) {
		cerror(errv); // TODO: pick better error
	}
	if (conbuf < 0 || conbuf > 077) {
		cerror(errv); // TODO: pick better error
	}
	putb(conbuf, 1);
	t = nextparm(); // see what's next
	return t;
}

void do_machine(op)
	int op;
{
	int opd = 0;
	int t;
	EXPR aar, bar, car;

	// must accumulate entire instruction before changing '.'
	while ((t = token()) == SPACE);
	if (t == COMMENT) t = EOL;
	if (op & OP_A) {
		if (t != EOL) {
			t = parse_addr(t, &aar);
			opd |= OP_A;
		} else if (op & RQ_A) {
			cerror(errx);
		}
	}
	if (op & OP_B) {
		if (t != EOL) {
			t = parse_addr(t, &bar);
			opd |= OP_B;
		} else if (op & RQ_B) {
			cerror(errx);
		}
	}
	if (op & OP_C) {
		if (t != EOL) {
			t = parse_addr(t, &car);
			opd |= OP_C;
		} else if (op & RQ_C) {
			cerror(errx);
		}
	}

	putb((op & OP_MSK) | WM, 1);
	if (opd & OP_A) {
		putaddr(aar.val, aar.rel, 0);
	}
	if (opd & OP_B) {
		putaddr(bar.val, bar.rel, 0);
	}
	if (opd & OP_C) {
		putaddr(car.val, car.rel, 0);
	} else if (t != EOL) { // assemble variants even if not used
		while (t != EOL) {
			t = parse_var(t);
		}
	}
}
