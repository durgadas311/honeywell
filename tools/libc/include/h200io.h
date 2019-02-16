#ifndef _H200IO_H_
#define _H200IO_H_

/* flag bits to control print()/lprint() */
#define PR_NL	001	/* newline after printing */
#define PR_SPB	002	/* space before string */
#define PR_SPA	004	/* space after string */

/* flag bits to control setpnc() */
#define SP_WM	001	/* set word mark */
#define SP_IM	002	/* set item mark */
#define SP_CLR	004	/* clear all first */

/* H200/2000 Console Output */
void print();	/* (char *str, int flag) - PR_* flags above */
void putc();	/* (int ch) */
void putnl();	/* (void) */
void puto();	/* (int num) - print num in octal, zero-sup */
void putx();	/* (int num) - print num in hex, zero-sup */
void putd();	/* (int num) - print num in decimal, zero-sup */
/* variations for LinePrinter output */
void lprint();	/* (char *str, int flag) - PR_* flags above */
void lputc();	/* (int ch) */
void lputnl();	/* (void) */
void lputo();	/* (int num) - print num in octal, zero-sup */
void lputx();	/* (int num) - print num in hex, zero-sup */
void lputd();	/* (int num) - print num in decimal, zero-sup */

/* H200/2000 memory punctuation */
void setpnc();	/* (void *ptr, int flag) */
int isrm();	/* (void *) */
int isim();	/* (void *) */
int iswm();	/* (void *) */

#endif /* _H200IO_H_ */