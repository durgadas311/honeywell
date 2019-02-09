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

// indirect address tag
static uint32_t am_ind(int am) {
	switch(am) {
	case 3: return 000700000;
	case 4: return 040000000;
	}
	return 0;
}

// indexed address tag
static uint32_t am_idx(int am, int xr) {
	switch(am) {
	case 3: return (xr << 15);
	case 4: return (xr << 19);
	}
	return 0;
}

static uint32_t am_mask(int am) {
	switch(am) {
	case 2: return             0b111111111111;
	case 3: return       0b000111111111111111;
	case 4: return 0b000001111111111111111111;
	}
	return 0;
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
		val &= am_mask(admode);
		// TODO: detect error?
		val |= am_ind(admode);
	} else {
		nexttoken = t;  // unget
		expr();
		rel = res.rel;
		val = res.val;
		val &= am_mask(admode);
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
			// TODO: detect error?
			val |= am_idx(admode, xr);
		}
	}
	reg->val = val;
	reg->rel = rel;
	nt = nextparm();
	return nt;
}

static int parse_var(int t) {
	int v;
	if (t == CON) {
		v = conbuf;
	} else if (t == IDENT && cursym->type == SABS) {
		v = cursym->value;
	} else {
		cerror(errv); // TODO: pick better error
	}
	if (v < 0 || v > 077) {
		cerror(errv); // TODO: pick better error
	}
	putb(v, 1);
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
