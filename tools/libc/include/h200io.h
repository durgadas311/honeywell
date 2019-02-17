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

/* SENSE switch detection */
int sense();	/* (int sw) */
/* These may be ORed, but only for */
/* SW_1..SW_4 or SW_5..SW_8, producing AND */
#define SW_1	001
#define SW_2	002
#define SW_3	004
#define SW_4	010
#define SW_5	021
#define SW_6	022
#define SW_7	024
#define SW_8	030

/* H200/2000 Punchcard I/O */
int cread();	/* (void *buf, int op) */
#define PC_HOL	027
#define PC_SPC	026

/* H200/2000 Mag Tape I/O */
int tread();	/* (int lun, void *buf) */
/* these should be ioctls? */
int tbsp();	/* (int lun) */
int tfwd();	/* (int lun) */
int trew();	/* (int lun) */
int tunl();	/* (int lun) */

/* H200/2000 Disk I/O */
struct dsk_adr {
	char lun;
	int cyl;
	int trk;
	int rec;
	int sw;	/* dtell only */ 
	int dl;	/* dtell only */ 
};
int dread();	/* (void *buf) */
int dwrite();	/* (void *buf) */
void dset();	/* (struct dsk_adr *adr) */
void dtell();	/* (struct dsk_adr *adr) */
int drest();	/* (int unit) */
int dseek();	/* (int unit, int cyl) */

#endif /* _H200IO_H_ */
