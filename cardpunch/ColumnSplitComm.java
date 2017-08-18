// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

class ColumnSplitComm extends ProgItem {
	ColumnSplitZone zone;
	ColumnSplitDigit dig;

	class C extends ProgStart {
		int pos;
		int pun;

		public C(int p) {
			super(false);
			pos = p;
			pun = 0;
		}
		@Override
		public void putCol(int p, char c) {
			dig.trigger(pos, p & 0x03ff, ' ');
			zone.trigger(pos, p & 0x0c00, ' ');
		}
		@Override
		void trigger(int p, char c) {
			pun |= p;
		}
		@Override
		public void set(boolean b) {
			super.trigger(pun, ' ');
			pun = 0;
		}
	}

	public ColumnSplitComm(int w) {
		super(w);
		// 'exit' is n/a... how to...
	}

	public void setXD(ColumnSplitZone n, ColumnSplitDigit t) {
		zone = n;
		dig = t;
	}

	@Override
	public ProgStart get(int p) {
		if (ents[p] == null) {
			ents[p] = new C(p);
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
	public void commit() {
		for (ProgStart ent : ents) {
			if (ent != null) {
				ent.set(true);
			}
		}
	}
}
