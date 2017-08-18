// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class Comparator {
	int width;
	class EntryItem extends ProgItem {
		public EntryItem(int w) {
			super(w);
			exit = false;
		}
		@Override
		public ProgStart get(int p) {
			if (ents[p] == null) {
				ents[p] = new ComparingEntry();
			}
			return ents[p];
		}
		@Override
		public void linkEntry(int id, int p, ProgStart es) {
			ComparingExit xt = (ComparingExit)exits.get(id);
			xt.expand(p);
			super.linkEntry(id, p, es);
		}
	}
	class ExitItem extends ProgItem {
		public ExitItem(int w) {
			super(w);
		}
		@Override
		public ProgStart get(int p) {
			if (ents[p] == null) {
				ents[p] = new ComparingExit(p);
			}
			return ents[p];
		}
		@Override
		public void linkEntry(int id, int p, ProgStart es) {
			// should only get called once, id == p.
			ComparingExit xt = (ComparingExit)get(id);
			//xt.expand(p);
			if (p == id) {
				xt.addWatcher(es);
			}
		}

		// Compares for NOT-EQUAL
		private void compare(ComparingExit xit) {
			int n = xit.width();
			int c = xit.position();
			while (n > 0) {
				ComparingEntry e1 = (ComparingEntry)ents1.get(c);
				ComparingEntry e2 = (ComparingEntry)ents2.get(c);
				if (!e1.compare(e2)) {
					xit.set(true);
					return;
				}
				++c;
				--n;
			}
		}

		public void processExits() {
			ComparingExit xt;
			for (int x = 0; x < ents.length; ++x) {
				xt = (ComparingExit)ents[x];
				if (xt != null) {
					compare(xt);
				}
			}
		}
	}

	EntryItem ents1;
	EntryItem ents2;
	ExitItem exits;

	public Comparator(int wid) {
		width = wid;
		ents1 = new EntryItem(wid);
		ents2 = new EntryItem(wid);
		exits = new ExitItem(wid);
	}

	public void reset() {
		ents1.reset();
		ents2.reset();
		exits.reset();
	}

	public ProgItem A() { return ents1; }
	public ProgItem B() { return ents2; }
	public ProgItem X() { return exits; }

	public void processExits() {
		exits.processExits();
	}
}
