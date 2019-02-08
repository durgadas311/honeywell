/*
 * C compiler-- basic constants for all
 */

#define	LTYPE	long	/* change to int if no long consts */
#define	MAXINT	037777777	/* Largest positive short integer */
#define	MAXUINT	077777777	/* largest unsigned integer */
#define	HSHSIZ	300	/* # entries in hash table for names */
#define	CMSIZ	40	/* size of expression stack */
#define	SSIZE	40	/* size of other expression stack */
#define	SWSIZ	300	/* size of switch table */
#define	NMEMS	128	/* Number of members in a structure */
#define	NBPW	24	/* bits per word, object machine */
#define	NBPC	6	/* bits per character, object machine */
#define	NCPW	4	/* chars per word, object machine */
#define	LNCPW	2	/* chars per word, compiler's machine */
#define	LNBPW	16	/* bits per word, compiler's machine */
/* dlf change
#define	STAUTO	(-8)	 offset of first auto variable */
int	STAUTO;
#define	STARG	8	/* offset of first argument */
			// forces alloc of return value, even if not used.
#define	DCLSLOP	512	/* Amount trees lie above declaration stuff */

// Basic call frame layout:
//         |   ...   |
//         +---------+
// 12(x2)  |  param2 |
//         +---------+
// 8(x2)   |  param1 |
//         +---------+
//         |(ret val)|
//         +---------+
// x2 ->   |(ret adr)| a.k.a. "bp"
//         +---------+
//         |(prev bp)|
//         +---------+
// -8(x2)  | (auto1) |
//         +---------+
// -12(x2) | (auto2) |
//         +---------+
//         |   ...   |
//         +---------+
//         | (autoN) |
//         +---------+
// x1 ->   |         | a.k.a. "sp"
//         +---------+
//         |   ...   |
//
// The above diagram shows the function stack after setup.
// On entry to function, 'x1' is at location 'x2' above, and
// 'x2' points to caller's frame.

/*
 * # bytes in primitive types
 */
#define	SZCHAR	1
#define	SZINT	4
#define	SZPTR	4
#define	SZFLOAT	8
#define	SZLONG	8
#define	SZDOUB	8

