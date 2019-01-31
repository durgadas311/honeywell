/*
 * Structure of archive file.
 *
 * This file is part of BKUNIX project, which is distributed
 * under the terms of the GNU General Public License (GPL).
 * See the accompanying file "COPYING" for more details.
 */
#ifndef _AR_H_
#define _AR_H_ 1

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdint.h>

#define	ARCMAGIC 0172040

struct ar_file {
	uint32_t ar_magic;
};

// This is repeated for each module
struct ar_hdr {
	char ar_name[14];		/* name */
	time_t ar_date;			/* modification time */
	uid_t ar_uid;			/* user id */
	gid_t ar_gid;			/* group id */
	mode_t ar_mode;			/* octal file permissions */
	off_t ar_size;			/* size in bytes */
};

#endif /* _AR_H_ */
