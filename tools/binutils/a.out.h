/*
 * Structure of a.out file.
 *
 * This file is part of BKUNIX project, which is distributed
 * under the terms of the GNU General Public License (GPL).
 * See the accompanying file "COPYING" for more details.
 */
#ifndef _AOUT_H_
#define _AOUT_H_ 1

#include <stdint.h>

struct exec {
	uint32_t	a_magic;	/* magic number */
	uint32_t	a_text; 	/* size of text segment */
	uint32_t	a_data; 	/* size of initialized data */
	uint32_t	a_bss;  	/* size of unitialized data */
	uint32_t	a_syms; 	/* size of symbol table */
	uint32_t	a_entry; 	/* entry point */
	uint32_t	a_unused;	/* not used */
	uint32_t	a_flag; 	/* relocation info stripped */
};

#define	A_FMAGIC	020007		/* normal */
#define	A_NMAGIC	020010		/* read-only text */
#define	A_IMAGIC	020011		/* separated I&D */
#define	N_BADMAG(x)	((x).a_magic != A_FMAGIC && \
			(x).a_magic != A_NMAGIC && \
			(x).a_magic != A_IMAGIC)

#define A_NRELFLG	01		/* non-relocatable flag */

#define	N_TXTOFF(x)	(sizeof(struct exec))
#define	N_RELOFF(x)	(N_TXTOFF(x) + (x).a_text + (x).a_data)
#define	N_SYMOFF(x)	(N_RELOFF(x) + (((x).a_flag & A_NRELFLG) ? 0 : \
			((x).a_text + (x).a_data)))

/*
 * relocation types
 */
#define A_RMASK		016		/* bit mask */
#define A_RABS		000		/* absolute */
#define A_RTEXT		002		/* reference to text */
#define A_RDATA		004		/* reference to data */
#define A_RBSS		006		/* reference to bss */
#define	A_REXT		010		/* external symbol */
#define A_RINDEX(r)	((r) >> 4)	/* external symbol index */
#define A_RPUTINDEX(r)	((r) << 4)

// Routines for the .reloc bytes
static inline uint32_t am_relmsk(int am) {
	switch (am) {
	case 2:	return 0b0111111111111111;
	case 3:	return 0b001111111111111111111111;
	case 4:	return 0b00011111111111111111111111111111;
	}
	return 0;
}
// NOTE! this is used on the hi byte only!
static inline uint32_t am_reltag(int am) {
	// first (hi) byte must be non-zero to flag a reloc
	switch (am) {
	case 2:	return 0b10000000;
	case 3:	return 0b01000000;
	case 4:	return 0b00100000;
	}
	return 0;
}

/*
 * symbol table entry
 */
struct nlist {
	char    	n_name[8];	/* symbol name */
	uint16_t     	n_type;    	/* type flag */
	uint16_t	n_pad1;
	uint32_t	n_value;	/* value */
};
/*
 * n_value is stored in files as "memory image" - i.e. 6-bit chars:
 *
 *    0b00xxxxxx00xxxxxx00xxxxxx00xxxxxx
 *
 * Address size is 19 bits, plus 5 bits for addressing mode.
 * Stored big-endian.
 */

/*
 * values for type flag
 */
#define	N_UNDF		000		/* undefined */
#define	N_ABS		001		/* absolute */
#define	N_TEXT		002		/* text symbol */
#define	N_DATA		003		/* data symbol */
#define	N_BSS		004		/* bss symbol */
#define	N_COMM		005		/* for ld internal use only */
#define	N_TYPE		037
#define	N_REG		024		/* register name */
#define	N_FN		037		/* file name symbol */
#define	N_EXT		040		/* external bit, or'ed in */

#endif /* _AOUT_H_ */
