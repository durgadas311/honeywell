#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <setjmp.h>

#include "as.h"

int	nifiles;	/* number of input files    */
char	**ifiles;	/* names of input files     */
int	cfile = -1;	/* index of current file    */
int	fd_in;		/* fd of current input file */

int	pass	 = 0;	/* pass number */
int	pass_gen = 0;	/* generate code in this pass ?    */
int	pass_lst = 0;	/* generate listing in this pass ? */
int	textlen  = 0;	/* length of text segment in current run */
int	lflag	 = 0;	/* listing */

char *ofname = "a.out";

char *usage = "Usage: as [-o obj-file] [-u] [-l] [-D sym=val] input-file [...]\n";

jmp_buf err_jmp;

int main(argc, argv)
	int argc; char **argv;
{
	int x;
	char *s;
	uint32_t v;
	SYMBOL *sym;
	extern int optind;
	extern char *optarg;

	sym_init();
	/* Get flags & files */
	while ((x = getopt(argc, argv, "D:lo:u")) != EOF) {
		switch (x) {
		case 'u':
			uflag++;
			break;
		case 'l':
			lflag++;
			break;
		case 'o':
			ofname = optarg;
			break;
		case 'D':
			s = strchr(optarg, '=');
			if (!s) {
				v = 1;
			} else {
				*s++ = '\0';
				v = strtoul(s, NULL, 0);
			}
			sym = sym_enter(optarg);
			sym->value = v;
			sym->type = SABS;
			sym->len = 0;
			break;
		default:
			goto error;
		}
	}
	if ((nifiles = argc - optind) < 1) {
error:
		fprintf(stderr, "%s", usage);
		exit(1);
	}
	ifiles = &argv[optind];

	/* symbol table generation pass */
	assemble();
	if (errcnt)
		exit(errcnt > 0);

	/*
	 * Code generation pass(es)
	 *	- more than one pass may be required in cases
	 *	  where short jump instructions must be changed
	 *    a reverse jump over a long distance branch.
	 *	  Since each pass can only make instructions longer, the
	 *	  process is guaranteed to terminate eventually.
	 */
	do {
		pass++;
		textlen = text.loc;
		cfile   = -1;
		assemble();
		if (errcnt)
			exit(errcnt > 0);
	} while ( textlen!=text.loc);

	if ((ofile = open(ofname, O_WRONLY|O_CREAT|O_TRUNC, 0666)) < 0) {
		fprintf(stderr, "%s: can't create\n", ofname);
		exit(1);
	}
	outhdr();

	/* listing & output pass */
	if (1) {
		pass_gen = 1;
		if (lflag) pass_lst = 1;
		cfile   = -1;
		assemble();
	}
	oflush(&text);
	oflush(&data);

	/* relocate symbols, append to file & finish */
	symout();
	close(ofile);

	return 0;
}

/*
 * Open next input file
 */

int nextfile()
{
	if (++cfile >= nifiles) {
		return(0);
	}

	if (fd_in)
		close(fd_in);

	fd_in = open(ifiles[cfile], O_RDONLY);
	if (fd_in < 0) {
		fprintf(stderr, "%s: can't open\n", ifiles[cfile]);
		exit(1);
	}
	line = 0;
	return(1);
}


/*
 * Print error message and continue
 */

static char *errtab [] = {
	"Previously defined absolute value required",
	"Short branch out of range",
	"Illegal or missing opcode",
	"Illegal data in .bss segement",
	"OEF or END within .if",
	"Garbage character",
	"Word value too large",
	"Improper nesting of .if and .endif",
	"Missing label",
	"Multiply defined symbol",
	"Number syntax",
	"Cannot move location counter backwards",
	"Symbol value changed between passes",
	"Missing quote",
	"Relocation error",
	"Missing symbol",
	"Undefined symbol",
	"Illegal value (e.g. register >15)",
	"Syntax error",
	"Division by zero",
	"Too many numeric labels",
	"Illegal map file",
};

int errcnt = 0;

void serror(str)
char *str;
{
	if (nifiles > 1)
		fprintf(stderr, "%s: ", ifiles[cfile]);
	fprintf(stderr, "%d: %s\n", line, str);
}

void cerror(type)
int type;
{
	serror(errtab[type]);
	errcnt++;
}

/*
 * Print error message and abort current line
 */
void xerror(type)
int type;
{
	cerror(type);
	nexttoken = 0;
	longjmp(err_jmp, 0);
}
