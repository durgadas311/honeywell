// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class Counter extends ProgExit {
	int width;
	int sum;
	ProgEntry[] ents;
	ProgStart plus;
	ProgStart minus;
	int mod;

	public Counter(int wid) {
		super();
		width = wid;
		ents = new ProgEntry[wid];
		sum = 0;
		minus = null;
		plus = null;
		mod = (int)Math.pow(10, width);
	}

	public void setPlus(ProgStart pl) {
		plus = pl;
	}

	public void setMinus(ProgStart mi) {
		minus = mi;
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
		boolean neg = false;
		if (v < 0) {
			neg = true;
			v = -v;
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
		int f = (d * (int)Math.pow(10, dig));
		// TODO: carry in...
		if (add) {
			sum += f;
		} else {
			sum -= f;
		}
		// TODO: carry out...
		sum %= mod;
	}
}
