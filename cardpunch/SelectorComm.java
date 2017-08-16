// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

class SelectorComm extends ProgItem {
	ProgStart that;
	SelectorNorm norm;
	SelectorTran tran;

	class C extends ProgStart {
		int pos;

		public C(int p) {
			super(false);
			pos = p;
		}
		@Override
		public void set(boolean b) {
			super.set(b);
			if (that.is()) {
				tran.trigger(pos, b);
			} else {
				norm.trigger(pos, b);
			}
		}
		@Override
		public void putCol(int p, char c) {
			if (that.is()) {
				tran.trigger(pos, p, c);
			} else {
				norm.trigger(pos, p, c);
			}
		}
		@Override
		public boolean is() {
			if (that.is()) {
				return tran.is(pos);
			} else {
				return norm.is(pos);
			}
		}
	}

	public SelectorComm(ProgStart it, int w) {
		super(w);
		// 'exit' is n/a... how to...
		that = it;
	}

	public void setNT(SelectorNorm n, SelectorTran t) {
		norm = n;
		tran = t;
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

	public void trigger(int p, boolean b) {
		if (ents[p] != null) {
			ents[p].trigger(b);
		}
	}

	public void trigger(int x, int p, char c) {
		if (ents[x] != null) {
			ents[x].trigger(p, c);
		}
	}

	public void resize(int w) {
		if (w <= ents.length) {
			return;
		}
		ProgStart[] ne = Arrays.copyOf(ents, w);
		ents = ne;
	}
}
