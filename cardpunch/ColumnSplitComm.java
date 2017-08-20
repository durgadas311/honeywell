// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

class ColumnSplitComm extends ProgItem {
	ColumnSplitZone zone;
	ColumnSplitZone r;
	ColumnSplitDigit dig;

	class C extends ProgStart {
		int pos;

		public C(int p) {
			super(false);
			pos = p;
		}
		@Override
		public void putCol(int p, char c) {
			dig.trigger(pos, p, c);
			zone.trigger(pos, p, c);
			if (r != null) {
				r.trigger(pos, p, c);
			}
		}
		@Override
		void trigger(int p, char c) {
			// do not trigger... yet...
		}
		@Override
		public void set(boolean b) {
			// "commit"
			int pun = zone.getPun(pos) | dig.getPun(pos);
			if (r != null) {
				pun |= r.getPun(pos);
			}
			super.trigger(pun, ' ');
		}
	}

	public ColumnSplitComm(int w) {
		super(w);
		// 'exit' is n/a... how to...
	}

	public void setXD(ColumnSplitZone r, ColumnSplitZone n, ColumnSplitDigit t) {
		zone = n;
		dig = t;
		this.r = r;
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
