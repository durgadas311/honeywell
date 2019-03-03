#ifndef __TASK_H__
#define __TASK_H__

struct task {
	int	sr;	
	int	eiaar;
	int	eibar;
	int	iiaar;
	int	iibar;
	char	eivar[5];
	char	iivar[5];
	char	brr[2];
	char	ibr[2];
	char	time[10];
};

extern struct task *task;
#endif /* __TASK_H__ */
