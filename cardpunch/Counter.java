// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class Counter extends ProgStart {

	class Carry extends ProgStart {
		Counter that;
		public Carry(Counter it) {
			super(true);
			that = it;
		}
		@Override
		public void set(boolean b) {
			super.set(b);
			if (!b) return;
			that.accum(1, width - 1);
		}
	}

	class Entry extends ProgItem {
		Counter that;
		public Entry(int w, Counter it) {
			super(w);
			exit = false;
			that = it;
		}
		@Override
		public ProgStart get(int p) {
			if (ents[p] == null) {
				ents[p] = new CounterEntry(that, p);
			}
			return ents[p];
		}
	}

	class Exit extends ProgItem {
		public Exit(int w) {
			super(w);
		}
		public void processExit(int p, char d) {
			// Do not use get() here...
			if (ents[p] != null) {
				ents[p].putCol(d);
			}
		}
	}

	int width;
	int sum;
	Entry ents;
	Exit exts;
	ProgStart plus = null;
	ProgStart minus = null;
	ProgStart credit;
	ProgStart cyo;
	int mod;

	// max number digits is 8.
	static final int[] pow = new int[]{
		1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
	};

	public Counter(int wid) {
		super(true);
		width = wid;
		ents = new Entry(width, this);
		exts = new Exit(width);
		sum = 0;
		mod = pow[width];
		credit = new ProgStart(true);
		cyo = new ProgStart(true);
	}

	public void setPlus(ProgStart pl) {
		plus = pl;
	}

	public void setMinus(ProgStart mi) {
		minus = mi;
	}

	// Credit Symbol Exit - allow multiple connections...
	// TODO: this should emit a character, not impulse...
	public void setCredit(ProgStart cr) {
		credit.addWatcher(cr);
	}

	// Carry Exit - allow multiple connections...
	public void setCarry(Counter ct) {
		setCarry(new Carry(ct));
	}
	public void setCarry(ProgStart es) {
		cyo.addWatcher(es);
	}

	public ProgItem E() { return ents; }
	public ProgItem X() { return exts; }

	// TOTAL ENTRY (print & reset)
	@Override
	public void set(boolean b) {
		super.set(b);	// n/a ?
		if (!b) return;
		int v = sum;
		sum = 0;
		// TODO: if (supp.is()) return;
		if (v < 0) {
			v = -v;
			credit.set(true);
		}
		for (int x = width; x > 0;) {
			--x;
			char d = (char)((v % 10) + '0');
			v /= 10;
			exts.processExit(x, d);
		}
	}

	private void add(int n) {
		sum += n;
		if (sum >= mod) {
			cyo.set(true);
			sum -= mod;
		}
	}

	private void sub(int n) {
		sum -= n;
		if (sum < 0) {
			cyo.set(true);
			sum += mod;
		}
	}

	// Called by CounterEntry.putCol() and Carry
	// 'dig' is 0-based but '0' is MSD.
	public void accum(int val, int dig) {
		boolean add = (plus != null && plus.is());
		boolean sub = (minus != null && minus.is());
		if (!add && !sub) {
			return;
		}
		if (dig < 0 || dig >= width || val == 0) {
			return;
		}
		dig = width - dig - 1;	// '0' is now LSD
		int f = (val * pow[dig]);
		if (add) {
			add(f);
		} else {
			sub(f);
		}
	}
}
