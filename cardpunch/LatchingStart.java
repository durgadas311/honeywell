// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// This is not intended to have watchers...
public class LatchingStart extends ProgStart {
	public LatchingStart() {
		super(false);
	}
	@Override
	public void set(boolean b) {
		if (!b) return;
		super.set(b);
	}
	public void clear() {
		super.set(false);
	}
}
