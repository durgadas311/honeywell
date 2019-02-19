#ifndef __AS_H__
#define __AS_H__

#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

/* tables.c */

#define OP_A	0x00100	// May have A operand
#define OP_B	0x00200	// May have B operand
#define OP_C	0x00400	// May have C operand
#define OP_V	0x00800	// May have Variant
#define RQ_A	0x01000	// Requires A operand
#define RQ_B	0x02000	// Requires B operand
#define RQ_C	0x04000	// Requires C operand
#define RQ_V	0x08000	// Requires Variant

#define OP_MSK	0x000ff
#define P_OP	0x10000	// pseudo-ops (directives)

#define WM	0100
#define IM	0200
#define RM	0300

#define NEG	0040	// BCD negative values, LSD

#define type_opc type_p
#define OPCODE   (uint16_t)(-1)

extern void sym_init();
extern int punct[];
extern char hw200[];

/* symbol.c */

struct symbol {				
	struct symbol	*next;		/* chain to next symbol with same hash code */
	struct symbol	*link;		/* chain to next symbol with next index */
	char		name[8];	/* left-justified, zero-padded */
	uint32_t	value;
	uint16_t	type;		/* relocatability type */
	uint16_t	idx;		/* symbol index in symbol table */
};
typedef struct symbol SYMBOL;

extern SYMBOL	*cursym;
extern char	symbuf[];
extern char	symrev;	// did it end in ::
extern int	symcount;
extern SYMBOL	*firstsym;
extern SYMBOL	*linksym;
extern int      admode;	// current address mode (always 4?)

extern SYMBOL	*sym_enter(char *name);
extern int	symlook(int flag);
extern void	sym_iter();

/* main.c */

/* error types */

#define erra	 0	/* previously defined absolute value required */
#define errb	 1	/* short branch out of range */
#define errc	 2	/* illegal or missing opcode */
#define errd	 3	/* illegal data in .bss segment */
#define erre	 4	/* eof or END within ifx */
#define errg	 5	/* garbage character */
#define errh	 6	/* halfword value too large */
#define erri	 7	/* improper nesting of .if and .endif */
#define errl	 8	/* missing label */
#define errm	 9	/* multiply defined symbol */
#define errn	10	/* number syntax */
#define erro	11	/* org to illegal address */
#define errp	12	/* symbol value changed between passes */
#define errq	13	/* missing quote */
#define errr	14	/* relocation error */
#define errs	15	/* missing symbol */
#define erru	16	/* undefined symbol */
#define errv	17	/* illegal value (e.g. register >15) */
#define errx	18	/* syntax error */
#define errz	19	/* division by zero */
#define errt	20	/* numeric label overflow */
#define errf	21	/* illegal map file */

extern void	serror(char *str);
extern void	cerror(int type);
extern void	xerror(int type);
extern int 	errcnt;

extern int	fd_in;			/* fd of current input file */

extern int	nextfile();

/* lexer.h */

/* lexical tokens - returned by token() */

#define	GARBAGE	0	/*  unknown character */
#define	PLUS	'+'
#define	MINUS	'-'
#define	OR	'|'
#define	AND	'&'
#define	STAR	'*'
#define	SLASH	'/'
#define	SPACE	' '	/* blanks and/or tabs */
#define	COMMA	','
#define	LPAREN	'('
#define	RPAREN	')'
#define	QUOTE	'\''
#define	DQUOTE	'"'
#define	AT	'@'
#define DOT	'.'
#define COLON	':'
#define SEMI	';'
#define EQU	'='

#define BINOP	16	/* last binary operator */
#define	EOL	17	/* end of line */
#define	CON	18	/* constant - value in conbuf */
#define	STRING	19	/* string & length in strbuf & strlen */
#define	IDENT	20	/* symbol - name in symbuf */
#define LABEL   21	/* symbol - name in symbuf, followed by a colon */
#define COMMENT 22	/* // found, skip rest of line */
#define NLABEL	23	/* numeric label, e.g. 1f: */
#define NIDENT	24	/* numeric identifier, e.g. 1f */

extern int	line;			/* line counter */
extern int	nexttoken;		/* look-ahead token */
extern int 	ucase;			/* if true, convert uppercase to lowercase symbols */
extern int 	uflag;			/* if true, make undefined symbols external */
extern char	strbuf[128];		/* returned value of last string */
extern long	conbuf;			/* returned value of last constant */
extern int	strsiz;			/* returned length of last string */

extern int get_line();
extern int token();
extern int tok(int lookup);
extern int getsym();
extern int eol();
extern void sscan();
extern void rscan();
extern void check_punc(int *pnc);
extern int scanhex(int pnc);
extern int scanoct(int pnc);
extern int scanbin(int pnc);
extern int scandec(int pnc);
extern int scan_bin(int pnc); // entry to scandec,scanbin,scanoct,scanhex
extern int scanfp(int pnc);
extern int scanbcd(int pnc);
extern int scanstr(int pnc);

/* assem.c */

#define PBYTE	1
#define PSTRING	2
#define PIF	3
#define PENDIF	4
#define PGLOBL	5
#define PTEXT	6
#define PDATA	7
#define PBSS	8
#define PCOMM	9
#define PWORD	10
#define PFLOAT	11
#define PDEC	12
#define PBIN	13
#define PSPACE	14
#define PADMODE	15
#define PERR	16
#define PWARN	17

/* symbol types */

#define	SUNDEF	  00		/* undefined */
#define	SABS	  01		/* absolute */
#define	STEXT	  02		/* text */
#define	SDATA	  03		/* data */
#define	SBSS	  04		/* bss */
#define	SSEG	  07		/* mask for segment */
#define	SEXT	 040		/* external (global) symbol */
#define	SREV	0100		/* use opposite end of item */
#define	SIDX	0200		/* index register X... */
#define	SIDY	0400		/* index register Y... */

/* segment relocation bits -- used in a.out relocation sections */

#define	RUNDEF	(-1)		/* undefined -- internal only */
#define	RABS	 00		/* absolute */
#define	RTEXT	 02		/* text = PURE */
#define	RDATA	 04		/* data = IMPUR */
#define	RBSS	 06		/* bss */
#define	REXT	010		/* external reference -- bits 15-4 contain symbol number */
#define	RSEG	017		/* Mask for segment */

/* table for numeric labels, e.g. 1f */
struct nlabel {
	char num;
	char seg;
	int loc;
};

extern struct nlabel nlabtab[];

/* segment information */

#define	OBSIZE	512		/* size of output buffers (must be even) */

extern struct segment {
	uint32_t loc;		/* location counter */
	uint32_t maxloc;	/* highest address so far */
	/* buffers below, only used for .text and .data segments */
	int nchar;		/* number of chars in buffers */
	int tseek;		/* seek address for text buffer */
	int rseek;		/* seek address for relocation bits buffer */
	char tbuf[OBSIZE];	/* text buffer */
	char rbuf[OBSIZE];	/* relocation bits buffer */
} text, data, bss, comm;

typedef struct segment SEGMNT;

extern SEGMNT *curseg;		/* current segment */
extern SYMBOL *curcomm;		/* current COMN or STRUC name */
extern SYMBOL *curlab;		/* current label */

extern int     currel;		/* current relocatability */

extern int	pass;		/* pass number */
extern int	pass_gen;	/* generate code in this pass ?    */
extern int	pass_lst;	/* generate listing in this pass ? */

extern int  assemble();
extern void do_pseudo(int op);
extern void do_machine(int op);

/* list.c */

extern void putline(int pnc);
extern void lstinit();
extern void lstloc();
extern void lstb(int val, int rel);
extern void lstaddr(uint32_t val, int rel);

/* output.c */

extern int ofile;	/* fd of output file */

extern void seginit();
extern void outhdr();
extern void symout();
extern void relocate_0407();
extern void nlabinit();

extern int  reloc(uint32_t val, int rel);
extern void putaddr(uint32_t val, int rel, int pnc);
extern void putb(int val, int lst);
extern void putstr(int len);
extern void putstrhw(char *val, int pnc);
extern void org(int loc);
extern void deflab(int rel, uint32_t val);
extern void align(int boundary);
extern void oflush(SEGMNT *sp);
extern void defnlab(uint32_t num, char seg, int loc);
extern struct nlabel *getnlab(uint32_t num);

/* expr.c */

struct expr {
	int rel;		/* relocatability of expression */
	uint32_t val;		/* value of expression */
};

typedef struct expr EXPR;

extern EXPR res;

extern int expr();

extern int parse_addr(int t, EXPR *reg, int simple);


#endif // __AS_H__
