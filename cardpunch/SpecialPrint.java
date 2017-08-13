// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

class SpecialPrint extends ProgStart {
	char _char;
	public SpecialPrint(char ch) {
		super(true);
		_char = ch;
	}
	@Override
	public void set(boolean b) {
		super.set(b);	// n/a ?
		if (!b) return;	// generate character on leading edge of cycle
		// TODO: enforce odd/even columns? Numeric?
		// TODO: generate some punch value?
		trigger(0, _char);
	}
}
