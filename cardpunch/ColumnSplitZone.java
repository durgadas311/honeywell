// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

class ColumnSplitZone extends ProgItem {
	ColumnSplitComm comm;
	int mask;

	class T extends ProgStart {
		int pos;
		int pun;

		public T(int p) {
			super(false);
			pos = p;
			pun = 0;
		}
		@Override
		void trigger(int p, char c) {
			// 'c' no longer valid...
			super.trigger(p & mask, ' ');
		}
		@Override
		public void putCol(int p, char c) {
			pun = p & mask;
		}
		public int getPun() {
			int p = pun;
			pun = 0;
			return p;
		}
	}

	public ColumnSplitZone(int w, int m) {
		super(w);
		mask = m;
	}

	public void setC(ColumnSplitComm c) {
		comm = c;
	}

	@Override
	public ProgStart get(int p) {
		if (ents[p] == null) {
			ents[p] = new T(p);
		}
		return ents[p];
	}

	// TODO: require some sort of hint to direction?
	@Override
	public void linkEntry(int id, int p, ProgStart es) {
		ProgStart ps = get(p);
		// TODO: is this legal?
		ps.addWatcher(es);
		es.addWatcher(ps);
	}

	public void setExit(boolean xt) {
		exit = xt;
	}

	public void trigger(int x, int p, char c) {
		if (ents[x] != null) {
			ents[x].trigger(p, c);
		}
	}

	public int getPun(int x) {
		if (ents[x] != null) {
			return ((T)ents[x]).getPun();
		} else {
			return 0;
		}
	}
}
