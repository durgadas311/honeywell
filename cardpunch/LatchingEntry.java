// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class LatchingEntry extends SingleEntry {
	public LatchingEntry() {
		super(new LatchingStart());
	}

	@Override
	public void reset() {
		ents[0].reset();
	}

	@Override
	public void set(int x, boolean b) {
		if (b) {
			ents[x].set(b);
		} else {
			((LatchingStart)ents[x]).clear();
		}
	}
}
