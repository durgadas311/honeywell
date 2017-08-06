// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class Counter extends ProgExit {
	int width;
	int sum;
	ProgEntry[] ents;
	boolean sub;
	int mod;

	public Counter(int wid) {
		super();
		width = wid;
		ents = new ProgEntry[wid];
		sum = 0;
		sub = false;
		mod = (int)Math.pow(10, width);
	}

	public void setMinus(boolean mi) {
		sub = mi;
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
		if (dig < 0 || dig >= width || d == 0) {
			return;
		}
		dig = width - dig - 1;
		int f = (d * (int)Math.pow(10, dig));
		if (sub) {
			sum -= f;
		} else {
			sum += f;
		}
		sum %= mod;
	}
}
