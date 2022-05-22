// Example showing stack/frame usage
// Program initialization must ensure that 'x1' contains a word mark.
	.globl	_end
	.admode	4
	.heap	128

// On entry:          In body:
//       +-------------+
// (+4)  | param1      | (+12)
//       +-------------+
// x1 -> | * return *  | (+8)
//       +-------------+
// (-4)  | * level+1 * | (+4)
//       +-------------+
// (-8)  |             | <- x1
//       +-------------+

	.text
// void main() {
//	int level = 0;
//	printf("main begin\n");
//	recurs(level);
//	printf("main end\n");
// }
main:
	cam	060
	lca	_stk,x1		// setup stack
	bs	_c4,x1		// alloc local variable 'level'
	lca	x1,x2		// init frame pointer
//
//       +-------------+
// (+8)  |     ---     | (+8)
//       +-------------+
// (+4)  |     ---     | (+4)
//       +-------------+
// x1 -> |   * level * | <- x2 (frame ptr)
//       +-------------+
// (-4)  |             | (-4)
//       +-------------+
	lca	_c0,0(x2)	// init level=0
	pdt	_m1,012,07,0	// print function name
	pcb	.,012,07,010
	pdt	_m2,012,07,01	// print start message
	pcb	.,012,07,010
	// callee can alter 'level' unless we copy it
	lca	0(x2),-4(x1)	// push copy of 'level' on stack
	bs	_c4,x1		// (adjust for push)
//       +-------------+
// (+12) |     ---     | (+8)
//       +-------------+
// (+8)  |     ---     | (+4)
//       +-------------+
// (+4)  |   * level * | <- x2 (frame ptr)
//       +-------------+
// x1 -> |     arg     | (-4)
//       +-------------+
	b	recurs		// call recurs(level)
	ba	_c4,x1		// pop/discard 'level' copy
	pdt	_m1,012,07,0	// print function name
	pcb	.,012,07,010
	pdt	_m3,012,07,01	// print end message
	pcb	.,012,07,010
	.admode	3
	cam	040
	b	(139)		// return to monitor
	h	.
	.admode	4

// void recurs(int l) {
//	int lev = l + 1;
//	printf("recurse level %d enter\n", lev);
//	if (lev != 5) recurs(lev);
//	printf("recurse level %d exit\n", lev);
// }
recurs:
//			 <- x2
//       +-------------+
// x1 -> |     arg     |
//       +-------------+
// (-4)  |     ---     |
//       +-------------+
// (-8)  |     ---     |
//       +-------------+
// (-12) |     ---     |
//       +-------------+
///////////////////////
// function preamble:
	scr	-4(x1),070	// save return address
	lca	x2,-8(x1)	// save frame pointer
	lca	x1,x2		// set new frame pointer
	bs	_c12,x1		// adjust stack for function
//       +-------------+
// (+12) |     arg     | <- x2
//       +-------------+
// (+8)  |   return    | (-4)
//       +-------------+
// (+4)  |     x2      | (-8)
//       +-------------+
// x1 -> |     lev     | (-12)
//       +-------------+
///////////////////////
// function body:
	lca	4(x2),-12(x2)	// copy param to local var
	ba	_c1,-12(x2)	// increment level
	exm	-12(x2),_r1+14,001 // put level (digit) in message
	pdt	_r1,012,07,0	// print...
	pcb	.,012,07,010	//
	pdt	_r2,012,07,01	// print rest of message
	pcb	.,012,07,010	//
	bce	1f,-12(x2),05	// stop recursion at 5
	// callee can modify level unless we copy
	lca	-12(x2),-4(x1)	// push 'lev' on stack
	bs	_c4,x1		// (adjust for push)
	b	recurs		// call recurs(lev)
	ba	_c4,x1		// discard func param
1:	exm	-12(x2),_r1+14,001 // put level in message
	pdt	_r1,012,07,0	// print...
	pcb	.,012,07,010	//
	pdt	_r3,012,07,01	// print rest of message
	pcb	.,012,07,010	//
///////////////////////
// function postamble:
	lca	x2,x1		// restore stack pointer
	lca	-8(x1),x2	// restore frame pointer
	lcr	-4(x1),077	// "return": load direct to SR
	nop // terminate prev instr - WM required at least

	.data
// main() data
@main:	.bin	4#1	// local stack variables
_stk:	.word	_end-@
_m1::	.string	f:"main_"
_m2::	.string f:" begin_"
_m3::	.string f:" end_"

// recurs() data
@recurs: .bin	8#1	// recurs() frame size
.recurs: .bin	4#1	// recurs() arglist size
_r1::	.string	f:"recurse level X_"
_r2::	.string f:" enter_"
_r3::	.string f:" exit_"

// general constants
_c0:	.bin	0#4	// init for params, addrs
_c1:	.bin	1#1
_c4:	.bin	4#1
_c8:	.bin	8#1
_c12:	.bin	12#1
