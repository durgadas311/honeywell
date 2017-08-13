// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

class SelectorTran extends ProgItem {
	ProgStart that;
	SelectorComm comm;

	class T extends ProgStart {
		int pos;

		public T(int p) {
			super(false);
			pos = p;
		}
		@Override
		public void set(boolean b) {
			super.set(b);
			if (that.is()) {
				comm.trigger(pos, b);
			}
		}
		@Override
		public void putCol(int p, char c) {
			if (that.is()) {
				comm.trigger(pos, p, c);
			}
		}
		@Override
		public boolean is() {
			if (that.is()) { return comm.is(pos); }
			return false;
		}
	}

	public SelectorTran(ProgStart it, int w) {
		super(w);
		// 'exit' is n/a... how to...
		that = it;
	}

	public void setC(SelectorComm c) {
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

	public void trigger(int x, boolean b) {
		if (ents[x] != null) {
			ents[x].trigger(b);
		}
	}

	public void trigger(int x, int p, char c) {
		if (ents[x] != null) {
			ents[x].trigger(p, c);
		}
	}
}
