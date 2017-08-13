// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

// set() -----> Selector(ProgStart) -----> is()
//                 |
//                 |
//        change() +----> SelectorContact
//                         [C(ProgStart)]
//                         [N(ProgStart)]
//                         [T(ProgStart)]

// An array of this class represents all available selectors,
// but each selector has an infinit number of contacts.
public class Selector extends ProgStart {
	boolean next;
	SelectorComm comm;
	SelectorNorm norm;
	SelectorTran tran;

	public Selector(int w) {
		super(false);
		comm = new SelectorComm(this, w);
		norm = new SelectorNorm(this, w);
		tran = new SelectorTran(this, w);
		comm.setNT(norm, tran);
		norm.setC(comm);
		tran.setC(comm);
		next = false;
	}

	public SelectorComm C() { return comm; }
	public SelectorNorm N() { return norm; }
	public SelectorTran T() { return tran; }

	@Override
	public void set(boolean b) {
		// TODO: does change require handling?
		// (notification of contacts?)
		next = b;
	}

	public void change() {
		super.set(next);
	}

	public String dump() {
		return String.format("%d", bool ? 1 : 0);
	}
}
