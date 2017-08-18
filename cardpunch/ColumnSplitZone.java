// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

class ColumnSplitZone extends ProgItem {
	ColumnSplitComm comm;

	class T extends ProgStart {
		int pos;

		public T(int p) {
			super(false);
			pos = p;
		}
		@Override
		public void putCol(int p, char c) {
			comm.trigger(pos, p & 0x0c00, ' ');
		}
	}

	public ColumnSplitZone(int w) {
		super(w);
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
}
