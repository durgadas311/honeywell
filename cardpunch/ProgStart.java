// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

// "is()" and "set()" represent opposite ends of this "widget".
// Watchers are part of the "is()" end. Watchers should never
// include this instance or the instance that calls "set()".

public class ProgStart {
	boolean bool;
	boolean oneShot;
	Vector<ProgStart> watchers;

	public ProgStart(boolean os) {
		oneShot = os;
		bool = false;
		watchers = new Vector<ProgStart>();
	}

	public void addWatcher(ProgStart ps) {
		// TODO: reject "ps == this"?
		// TODO: reject duplicates?
		watchers.add(ps);
	}

	public void reset() {
		watchers.clear();
		bool = false;
	}

	boolean get() { return bool; }

	void trigger(boolean b) {
		for (ProgStart ps : watchers) {
			ps.set(b);
		}
	}

	void trigger(char c) {
		for (ProgStart ps : watchers) {
			ps.putCol(c);
		}
	}

	// These may be overridden...
	// This is the interface.
	public boolean is() { return get(); }

	public void set(boolean b) {
		if (!oneShot && bool == b) {
			return;
		}
		bool = b;
		trigger(bool);
	}

	// Unless extended, this class has nothing to do with char.
	// Note, overriding putCol() requires calling trigger(c).
	public void putCol(char c) {
		trigger(c);
	}
}
