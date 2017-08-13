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
		public void processExit(int x, int p, char c) {
			// Do not use get() here...
			if (ents[x] != null) {
				ents[x].putCol(p, c);
			}
		}
	}

	int width;
	int sum;
	Entry ents;
	Exit exts;
	SingleEntry plus;
	SingleEntry minus;
	ProgItem credit;
	ProgItem cyo;
	ProgItem cyi;
	SingleEntry supp;
	SingleEntry total;
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
		plus = new SingleEntry();
		minus = new SingleEntry();
		sum = 0;
		mod = pow[width];
		credit = new SingleExit(new SpecialPrint('\u00a9'));
		cyo = new ProgItem(1);
		cyi = new SingleEntry(new Carry(this));
		supp = new SingleEntry();
		total = new SingleEntry(this);
	}

	public ProgItem E() { return ents; }
	public ProgItem X() { return exts; }
	public ProgItem PLUS() { return plus; }
	public ProgItem MINUS() { return minus; }
	public ProgItem CR() { return credit; }
	public ProgItem CI() { return cyo; }
	public ProgItem C() { return cyi; }
	public ProgItem SUPP() { return supp; }
	public ProgItem TOTAL() { return total; }

	// TOTAL ENTRY (print & reset)
	@Override
	public void set(boolean b) {
		if (b) return;	// Print on trailing edge of cycle
		int v = sum;
		sum = 0;
		if (supp.is(0)) {
			return;
		}
		if (v < 0) {
			v = -v;
			credit.set(0, true);
		}
		for (int x = width; x > 0;) {
			--x;
			int n = v % 10;
			char d = (char)(n + '0');
			int p = (0x0200 >> n);
			v /= 10;
			exts.processExit(x, p, d);
		}
		super.set(b); // now trigger watchers (printing)...
	}

	private void add(int n) {
		sum += n;
		if (sum >= mod) {
			cyo.set(0, true);
			sum -= mod;
		}
	}

	private void sub(int n) {
		sum -= n;
		if (sum < 0) {
			cyo.set(0, true);
			sum += mod;
		}
	}

	// Called by CounterEntry.putCol() and Carry
	// 'dig' is 0-based but '0' is MSD.
	public void accum(int val, int dig) {
		boolean add = (plus != null && plus.is(0));
		boolean sub = (minus != null && minus.is(0));
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
