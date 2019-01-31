#include "as.h"

struct optab {
	char   name[8];	
	int    opval;		
};

/*
 * Opcodes and pseudo-ops
 */
struct optab opcodes[] = {

	/* data-definition pseudo-ops */
	{ ".byte",	P_OP | PBYTE  }, // * general data (char, not byte)
	{ ".string",	P_OP | PSTRING}, // * ASCII converted to H200, limited escapes
	{ ".word",	P_OP | PWORD  }, // * e.g. DCA
	{ ".float",	P_OP | PFLOAT }, // * FP constants
	{ ".dec",	P_OP | PDEC   }, // * variable-length BCD fields
	{ ".bin",	P_OP | PBIN   }, // * variable-length numeric fields
	{ ".globl",	P_OP | PGLOBL },
	{ ".if",	P_OP | PIF    },
	{ ".endif",	P_OP | PENDIF },
	{ ".text",	P_OP | PTEXT  },
	{ ".data",	P_OP | PDATA  },
	{ ".bss",	P_OP | PBSS   },
	{ ".comm",	P_OP | PCOMM  },
	// * = accepts "Set II Punctuation Indicator" prefix - e.g. "A:"

	/* HW200/2000 machine ops */
	{ "a",      036 | OP_A | OP_B },
	{ "s",      037 | OP_A | OP_B },
	{ "ba",     034 | OP_A | OP_B },
	{ "bs",     035 | OP_A | OP_B },
	{ "za",     016 | OP_A | OP_B },
	{ "zs",     017 | OP_A | OP_B },
	{ "m",      026 | OP_A | OP_B },
	{ "d",      027 | OP_A | OP_B },
	{ "ext",    031 | OP_A | OP_B },
	{ "ha",     030 | OP_A | OP_B },
	{ "sst",    032 | OP_A | OP_B | OP_V },
	{ "c",      033 | OP_A | OP_B },
	{ "b",      065 | OP_A | RQ_A },
	{ "bct",    065 | OP_A | OP_V },
	{ "bcc",    054 | OP_A | OP_B | OP_V },
	{ "bce",    055 | OP_A | OP_B | OP_V },
	{ "bbe",    056 | OP_A | OP_B | OP_V },
	{ "sw",     022 | OP_A | OP_B },
	{ "si",     020 | OP_A | OP_B },
	{ "cw",     023 | OP_A | OP_B },
	{ "ci",     021 | OP_A | OP_B },
	{ "h",      045 | OP_A | OP_B | OP_V },
	{ "nop",    040 },
	{ "mcw",    014 | OP_A | OP_B },
	{ "lca",    015 | OP_A | OP_B },
	{ "scr",    024 | OP_A | OP_V },
	{ "lcr",    025 | OP_A | OP_V },
	{ "cam",    042 | OP_V },
	{ "csm",    043 | OP_A | OP_B },
	{ "exm",    010 | OP_A | OP_B },
	{ "mat",    060 | OP_A | OP_B | OP_C | RQ_A | RQ_B | RQ_C },
	{ "mit",    062 | OP_A | OP_B | OP_V | RQ_A | RQ_B | RQ_V },
	{ "lib",    077 | OP_A | OP_B | RQ_A },
	{ "sib",    076 | OP_A | OP_B | RQ_A },
	{ "tlu",    057 | OP_A | OP_B | OP_V },
	{ "mos",    013 | OP_A | OP_B | OP_V },
	{ "svi",    046 | OP_V | RQ_V },
	{ "rvi",    067 | OP_A | RQ_A | OP_V | RQ_V },
	{ "mc",     044 },
	{ "rnm",    041 | OP_A | OP_B },
	{ "mce",    074 | OP_A | OP_B },
	{ "pdt",    066 | OP_A | RQ_A | OP_V | RQ_V },
	{ "pcb",    064 | OP_A | RQ_A | OP_V | RQ_V },
	{ "iic",    000 },	// TBD
	// Variant specifies operation:
	{ "fma",    007 | OP_A | RQ_A | OP_V | RQ_V },
	{ "faa",    006 | OP_V | RQ_V },
	{ "bms",    004 | OP_V | RQ_V },
	{ "bim",    005 | OP_A | OP_B | RQ_A | RQ_B },
	{ {0}, 0 }
};

int punct[] = {
['A'-'A'] = (WM << 8) | 0,
['B'-'A'] = (IM << 8) | 0,
['C'-'A'] = (RM << 8) | 0,
['D'-'A'] = 0         | WM,
['E'-'A'] = 0         | IM,
['F'-'A'] = 0         | RM,
['G'-'A'] = (IM << 8) | IM,
['H'-'A'] = (IM << 8) | WM,
['I'-'A'] = (IM << 8) | RM,
['J'-'A'] = (WM << 8) | IM,
['K'-'A'] = (WM << 8) | WM,
['L'-'A'] = -1,
['M'-'A'] = (WM << 8) | RM,
['N'-'A'] = 0         | 0,
['O'-'A'] = -1,
['P'-'A'] = (RM << 8) | WM,
['Q'-'A'] = -1,
['S'-'A'] = (RM << 8) | IM,
['T'-'A'] = (RM << 8) | RM,
['U'-'A'] = -1,
['V'-'A'] = -1,
['W'-'A'] = -1,
['X'-'A'] = -1,
['Y'-'A'] = -1,
['Z'-'A'] = -1,
};

char hw200[128] = {
	036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036,
	036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036,
	015, 057, 055, 052, 053, 035, 017, 012, 074, 034, 054, 020, 073, 040, 033, 061,
	000, 001, 002, 003, 004, 005, 006, 007, 010, 011, 014, 032, 060, 013, 016, 037,
	072, 021, 022, 023, 024, 025, 026, 027, 030, 031, 041, 042, 043, 044, 045, 046,
	047, 050, 051, 062, 063, 064, 065, 066, 067, 070, 071, 076, 036, 036, 077, 036,
	000, 021, 022, 023, 024, 025, 026, 027, 030, 031, 041, 042, 043, 044, 045, 046,
	047, 050, 051, 062, 063, 064, 065, 066, 067, 070, 071, 036, 036, 036, 075, 036,
};

// This is different from "mcw label(x1),foo".
// This is for things like "lwa foo,x1"
struct optab predefs[] = {
	/* index register names */
	{ "x1", 	4 },
	{ "x2", 	8 },
	{ "x3", 	12 },
	{ "x4", 	16 },
	{ "x5", 	20 },
	{ "x6", 	24 },
	{ "x7", 	28 },
	{ "x8", 	32 },
	{ "x9", 	36 },
	{ "x10",	40 },
	{ "x11",	44 },
	{ "x12",	48 },
	{ "x13",	52 },
	{ "x14",	56 },
	{ "x15",	60 },
	// TODO: need to differentiate Y?
	{ "y1", 	4 },
	{ "y2", 	8 },
	{ "y3", 	12 },
	{ "y4", 	16 },
	{ "y5", 	20 },
	{ "y6", 	24 },
	{ "y7", 	28 },
	{ "y8", 	32 },
	{ "y9", 	36 },
	{ "y10",	40 },
	{ "y11",	44 },
	{ "y12",	48 },
	{ "y13",	52 },
	{ "y14",	56 },
	{ "y15",	60 },
	{ {0}, 0 }
};

void sym_init()
{
	struct optab  *op;
	SYMBOL *sym;

	for (op = opcodes; op->name[0]; op++) {
		sym = sym_enter(op->name);
		sym->type  = OPCODE;
		sym->value = op->opval;
	}
	for (op = predefs; op->name[0]; op++) {
		sym = sym_enter(op->name);
		if (op->name[0] == 'x') {
			sym->type  = SABS | SIDX;
		} else {
			sym->type  = SABS | SIDY;
		}
		sym->value = op->opval;
	}
	linksym  = 0;
	symcount = 0;
}
