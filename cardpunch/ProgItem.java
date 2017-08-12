// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

public class ProgItem {
	ProgStart[] ents;
	boolean exit;	// are we input or output?

	public ProgItem(int w) {
		ents = new ProgStart[w];
		exit = true;
	}

	// Reset for new program panel
	public void reset() {
		Arrays.fill(ents, null);
	}

	public ProgStart get(int p) {
		if (ents[p] == null) {
			ents[p] = new ProgStart(true);
		}
		return ents[p];
	}

	public boolean isExit() { return exit; }

	// Typically overridden, this default works for simple cases.
	// Must be overridden for ENTRY items.
	public void linkEntry(int id, int p, ProgStart es) {
		if (p < 0 || p >= ents.length) {
			return;
		}
		if (exit) {
			get(p).addWatcher(es);
		} else {
			es.addWatcher(get(p));
		}
	}
}
