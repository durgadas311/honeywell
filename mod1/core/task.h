#ifndef __TASK_H__
#define __TASK_H__

/* task state (flags) */
#define TF_DONE		0	/* exited, gone */
#define TF_RUN		1	/* run */
#define TF_EMERG	2	/* run, NOW */

/* task exit/return code */
#define TR_OK		0
#define TR_EINTR	001	/* xxxx01 = fatal II "signal" */
#define TR_ENOSYS	2
/* 
#define TR_		3
#define TR_		4
#define TR_		6
#define TR_		7
#define TR_		8
#define TR_		10...
 */
/* TR_EINTR: multiple signals could be set */
#define TR_SEGV		041	/* 1xxx01 = adr viol */
#define TR_ILL		021	/* x1xx01 = opcode viol */
#define TR_ITO		011	/* xx1x01 = instr timeout */
#define TR_FPE		005	/* xxx101 = FPE */

struct task {
	int	flags;	/* task state */
	void	*sr;	/* task SR on intr */
	int	eiaar;
	int	eibar;
	int	iiaar;
	int	iibar;
	char	eivar[5];
	char	iivar[5];
	char	brr[2];	/* base page (4K) of task */
	char	ibr[2]; /* num pages in task */
	char	time[8]; /* accum CPU time (ATR) */
	char	id;	/* ID char for logs */
	char	ret;	/* exit code for task */
};

extern struct task *task;

#endif /* __TASK_H__ */
