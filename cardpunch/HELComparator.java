// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

public class HELComparator {
	int width;
	class EntryItem extends ProgItem {
		public EntryItem(int w) {
			super(w);
			exit = false;
		}
		@Override
		public ProgStart get(int p) {
			if (ents[p] == null) {
				ents[p] = new HELComparingEntry();
			}
			return ents[p];
		}
	}
	class ExitItem extends ProgItem {
		public ExitItem() {
			super(3);	// HI, EQ, LO (0, 1, 2)
			ents[0] = new ProgStart(false);
			ents[1] = new ProgStart(false);
			ents[2] = new ProgStart(false);
		}
		@Override
		public void reset() {
			ents[0].reset();
			ents[1].reset();
			ents[2].reset();
		}
		@Override
		public void linkEntry(int id, int p, ProgStart es) {
			// should only get called once, id == p.
			if (p == id) {
				es.addWatcher(es);
			}
		}

		// HIGH-EQUAL-LOW compare...
		private int compare(boolean mode) {
			// fixed-width comparator
			int n = width;
			int c = 0;
			int cmp = 0;
			while (n > 0) {
				// TODO: don't use get()!
				HELComparingEntry e1 = (HELComparingEntry)ents1.get(c);
				HELComparingEntry e2 = (HELComparingEntry)ents2.get(c);
				cmp = e1.compare(mode, e2);
				if (cmp != 0) {
					break;
				}
				++c;
				--n;
			}
			return cmp;
		}

		public void processExits(boolean mode) {
			ents[0].set(false);
			ents[1].set(false);
			ents[2].set(false);
			int cmp = compare(mode);
			switch (cmp) {
			case -1:
				ents[2].set(true);
				break;
			case 0:
				ents[1].set(true);
				break;
			case 1:
				ents[0].set(true);
				break;
			}
		}
	}

	EntryItem ents1;
	EntryItem ents2;
	ExitItem exits;

	public HELComparator(int wid) {
		width = wid;
		ents1 = new EntryItem(wid);
		ents2 = new EntryItem(wid);
		exits = new ExitItem();
	}

	public void reset() {
		ents1.reset();
		ents2.reset();
		exits.reset();
	}

	public ProgItem A() { return ents1; }
	public ProgItem B() { return ents2; }
	public ProgItem X() { return exits; }

	public void processExits(boolean mode) {
		exits.processExits(mode);
	}
}
