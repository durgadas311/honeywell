// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class Counter extends ProgExit {
	int width;
	int sum;
	ProgEntry[] ents;
	ProgStart plus = null;
	ProgStart minus = null;
	ProgExit credit = null;
	Counter cyo = null;
	int mod;

	// max number digits is 8.
	static final int[] pow = new int[]{
		1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
	};

	public Counter(int wid) {
		super();
		width = wid;
		ents = new ProgEntry[width];
		sum = 0;
		mod = pow[width];
	}

	public void setPlus(ProgStart pl) {
		plus = pl;
	}

	public void setMinus(ProgStart mi) {
		minus = mi;
	}

	public void setCredit(ProgExit cr) {
		credit = cr;
	}

	public void setCarry(Counter ct) {
		cyo = ct;
	}

	public void setEntry(int dig, ProgEntry ent) {
		if (dig < 0 || dig >= width) {
			return;
		}
		ent.setNext(ents[dig]);
		ents[dig] = ent;
	}

	public void processExits() {
		int v = sum;
		sum = 0;
		if (v < 0) {
			v = -v;
			if (credit != null) {
				credit.processExits();
			}
		}
		for (int x = ents.length; x > 0;) {
			--x;
			char d = (char)((v % 10) + '0');
			v /= 10;
			if (ents[x] != null) {
				ents[x].putCol(d);
			}
		}
		if (_next != null) {
			_next.processExits();
		}
	}

	private void add(int n) {
		sum += n;
		if (sum >= mod) {
			if (cyo != null) {
				cyo.carry();
			}
			sum -= mod;
		}
	}

	private void sub(int n) {
		sum -= n;
		if (sum < 0) {
			if (cyo != null) {
				cyo.borrow();
			}
			sum += mod;
		}
	}

	private void carry() {
		add(1);
	}

	private void borrow() {
		sub(1);
	}

	public void accum(int d, int dig) {
		boolean add = (plus != null && plus.is());
		boolean sub = (minus != null && minus.is());
		if (!add && !sub) {
			return;
		}
		if (dig < 0 || dig >= width || d == 0) {
			return;
		}
		dig = width - dig - 1;
		int f = (d * pow[dig]);
		if (add) {
			add(f);
		} else {
			sub(f);
		}
	}
}
