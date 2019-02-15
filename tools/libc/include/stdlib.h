#ifndef _STDLIB_H_
#define _STDLIB_H_

void div();	/* (int num, int den, int *res) */

char *strcat();	/* (char *d, char *s) */
char *strcpy();	/* (char *d, char *s) */
int strlen();	/* (char *s) */
int strcmp();	/* (char *s1, char *s2) */

int memcpy();	/* (char *d, char *s, int n) */
int memcmp();	/* (char *s1, char *s2, int n) */
int memset();	/* (char *s, int c, int n) */

#endif /* _STDLIB_H_ */
