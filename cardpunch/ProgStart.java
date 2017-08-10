// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;

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

	public boolean is() { return bool; }

	public void set(boolean b) {
		bool = b;
		for (ProgStart ps : watchers) {
			ps.set(bool);
		}
		if (bool && oneShot) {
			bool = false;
		}
	}
}
