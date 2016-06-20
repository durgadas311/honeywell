#include <stdio.h>
#include <string.h>

static char buf[4096]; /* lines (cards) really should be <= 80... */

int main(int argc, char **argv) {
	int x;
	extern optarg;
	int intvl = 10;
	int lineno;
	char num[8];

	while ((x = getopt(argc, argv, "i:")) != EOF) {
		switch(x) {
		case 'i':
			intvl = strtoul(optarg, NULL, 0);
			break;
		}
	}
	lineno = 0;
	while (fgets(buf, sizeof(buf), stdin) != NULL) {
		lineno += intvl;
		if (lineno > 99999) {
			lineno = 99999;
		}
		sprintf(num, "%05d", lineno);
		memcpy(buf, num, 5);
		fputs(buf, stdout);
	}
	return 0;
}
