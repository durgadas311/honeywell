#include "c1.h"

/*
	operators
*/
static char *ops[] = {
[EOFC] = "EOFC",
[SEMI] = "SEMI",
[LBRACE] = "LBRACE",
[RBRACE] = "RBRACE",
[LBRACK] = "LBRACK",
[RBRACK] = "RBRACK",
[LPARN] = "LPARN",
[RPARN] = "RPARN",
[COLON] = "COLON",
[COMMA] = "COMMA",
[FSEL] = "FSEL",
[FSELR] = "FSELR",
[FSELT] = "FSELT",
[FSELA] = "FSELA",
[URSH] = "URSH",
[ASURSH] = "ASURSH",
[KEYW] = "KEYW",
[NAME] = "NAME",
[CON] = "CON",
[STRING] = "STRING",
[FCON] = "FCON",
[SFCON] = "SFCON",
[LCON] = "LCON",
[CCON] = "CCON",
[AUTOI] = "AUTOI",
[AUTOD] = "AUTOD",
[NULLOP] = "NULLOP",
[INCBEF] = "INCBEF",
[DECBEF] = "DECBEF",
[INCAFT] = "INCAFT",
[DECAFT] = "DECAFT",
[EXCLA] = "EXCLA",
[AMPER] = "AMPER",
[STAR] = "STAR",
[NEG] = "NEG",
[COMPL] = "COMPL",
[DOT] = "DOT",
[PLUS] = "PLUS",
[MINUS] = "MINUS",
[TIMES] = "TIMES",
[DIVIDE] = "DIVIDE",
[MOD] = "MOD",
[RSHIFT] = "RSHIFT",
[LSHIFT] = "LSHIFT",
[AND] = "AND",
[OR] = "OR",
[EXOR] = "EXOR",
[ARROW] = "ARROW",
[ITOF] = "ITOF",
[FTOI] = "FTOI",
[LOGAND] = "LOGAND",
[LOGOR] = "LOGOR",
[FTOL] = "FTOL",
[LTOF] = "LTOF",
[ITOL] = "ITOL",
[LTOI] = "LTOI",
[ITOP] = "ITOP",
[PTOI] = "PTOI",
[LTOP] = "LTOP",
[EQUAL] = "EQUAL",
[NEQUAL] = "NEQUAL",
[LESSEQ] = "LESSEQ",
[LESS] = "LESS",
[GREATEQ] = "GREATEQ",
[GREAT] = "GREAT",
[LESSEQP] = "LESSEQP",
[LESSP] = "LESSP",
[GREATQP] = "GREATQP",
[GREATP] = "GREATP",
[ASPLUS] = "ASPLUS",
[ASMINUS] = "ASMINUS",
[ASTIMES] = "ASTIMES",
[ASDIV] = "ASDIV",
[ASMOD] = "ASMOD",
[ASRSH] = "ASRSH",
[ASLSH] = "ASLSH",
[ASAND] = "ASAND",
[ASOR] = "ASOR",
[ASXOR] = "ASXOR",
[ASSIGN] = "ASSIGN",
[LTIMES] = "LTIMES",
[LDIV] = "LDIV",
[LMOD] = "LMOD",
[LASTIMES] = "LASTIMES",
[LASDIV] = "LASDIV",
[LASMOD] = "LASMOD",
[QUEST] = "QUEST",
[MAXP] = "MAXP",
[MINP] = "MINP",
[SEQNC] = "SEQNC",
[CALL1] = "CALL1",
[CALL2] = "CALL2",
[CALL] = "CALL",
[MCALL] = "MCALL",
[JUMP] = "JUMP",
[CBRANCH] = "CBRANCH",
[INIT] = "INIT",
[SETREG] = "SETREG",
[LOAD] = "LOAD",
[PTOI1] = "PTOI1",
[ITOC] = "ITOC",
[RFORCE] = "RFORCE",
[BRANCH] = "BRANCH",
[LABEL] = "LABEL",
[NLABEL] = "NLABEL",
[RLABEL] = "RLABEL",
[STRASG] = "STRASG",
[STRSET] = "STRSET",
[UDIV] = "UDIV",
[UMOD] = "UMOD",
[ASUDIV] = "ASUDIV",
[ASUMOD] = "ASUMOD",
[ULTIMES] = "ULTIMES",
[ULDIV] = "ULDIV",
[ULMOD] = "ULMOD",
[ULASTIMES] = "ULASTIMES",
[ULASDIV] = "ULASDIV",
[ULASMOD] = "ULASMOD",
[ULTOF] = "ULTOF",
[ALABEL] = "ALABEL",
[MINSTAT] = "MINSTAT",
[SLABEL] = "SLABEL",
[BDATA] = "BDATA",
[PROG] = "PROG",
[DATA] = "DATA",
[BSS] = "BSS",
[CSPACE] = "CSPACE",
[SSPACE] = "SSPACE",
[SYMDEF] = "SYMDEF",
[SAVE] = "SAVE",
[RETRN] = "RETRN",
[EVEN] = "EVEN",
[BSTR] = "BSTR",
[PROFIL] = "PROFIL",
[SWIT] = "SWIT",
[EXPR] = "EXPR",
[SNAME] = "SNAME",
[RNAME] = "RNAME",
[ANAME] = "ANAME",
[SETSTK] = "SETSTK",
[SINIT] = "SINIT",
[GLOBAL] = "GLOBAL",
[C3BRANCH] = "C3BRANCH",
[ASSEM] = "ASSEM",
[BSTR2] = "BSTR2",
[BSTR0] = "BSTR0",
};

/*
 *	types
 */
static char *types[] = {
[INT] = "INT",
[CHAR] = "CHAR",
[FLOAT] = "FLOAT",
[DOUBLE] = "DOUBLE",
[STRUCT] = "STRUCT",
[RSTRUCT] = "RSTRUCT",
[LONG] = "LONG",
[UNSIGN] = "UNSIGN",
[UNCHAR] = "UNCHAR",
[UNLONG] = "UNLONG",
[VOID] = "VOID",
};

static char *xtypes[] = {
[PTR >> 4] = "PTR",
[FUNC >> 4] = "FUNC",
[ARRAY >> 4] = "ARRAY",
};
#if 0
#define	TYLEN	2
#define	TYPE	017
#define	XTYPE	(03<<4)
#define	PTR	020
#define	FUNC	040
#define	ARRAY	060

// degree constants
#define DALL	0077	// max degree, matches all
#define DPTR	0100	// pointer, indirection
#define DZER	4	// "0"
#define DONE	5	// "1"
#define DTWO	6	// "2"
#define DCON	8	// other constants
#define DCHR	9
#define DADR	12
#define DFIX	16	// cut-off for ???
#define DREG	20
#define DNRG	24
#endif
/*
	storage	classes
*/
static char *classes[] = {
[KEYWC] = "KEYWC",
[MOS] = "MOS",
[AUTO] = "AUTO",
[EXTERN] = "EXTERN",
[STATIC] = "STATIC",
[REG] = "REG",
[STRTAG] = "STRTAG",
[ARG] = "ARG",
[OFFS] = "OFFS",
[XOFFS] = "XOFFS",
[SOFFS] = "SOFFS",
};

static char buf[32];

void dumptree(union tree *p, int level, char tag) {
	static char *blanks = "                                ";
	int op = p->t.op;
	int type = p->t.type;
	int xtyp = (type & XTYPE) >> 4;
	type &= TYPE;
	int class;
	if (!level) {
		fprintf(stderr, "%.*s%d: %c ", level * 2, blanks, line, tag);
	} else {
		fprintf(stderr, "%.*s%c ", level * 2, blanks, tag);
	}
	
        fprintf(stderr, "%6s: %6s", ops[op], types[type]);
	if (xtyp) {
        	fprintf(stderr, "+%-5s", xtypes[xtyp]);
	}
	switch (op) {
	case NAME:
		class = p->x.class;
		fprintf(stderr, " %6s", classes[class]);
		if (class == EXTERN) {
			fprintf(stderr, " r%d +%d \"%s\"",
				p->x.regno, p->x.offset, p->x.name);
		} else {
			fprintf(stderr, " r%d +%d L%d",
				p->n.regno, p->n.offset, p->n.nloc);
		}
		break;
	case CON:
		fprintf(stderr, " = %d", p->c.value);
		break;
	case CCON:
		fprintf(stderr, " = '%c'", p->c.value);
		break;
	case EXPR:
		break;
	}
	fprintf(stderr, " {%lx %lx}\n", (unsigned long)p->t.tr1, (unsigned long)p->t.tr2);
	// opdope for LOAD not correct?
	if (op != LOAD && op != STAR && op != INIT &&
			op != CALL1 && op != CALL2 &&
			!(opdope[op] & (BINARY|LVALUE))) {
		return;
	}
	if (p->t.tr1) {
		dumptree(p->t.tr1, level + 1, 'L');
	}
	if (p->t.tr2) {
		dumptree(p->t.tr2, level + 1, 'R');
	}
}
