// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

class SpecialPrint extends ProgStart {
	char _char;
	int _punch;
	public SpecialPrint(int pu, char ch) {
		super(true);
		_char = ch;
		_punch = pu;
	}
	@Override
	public void set(boolean b) {
		super.set(b);	// n/a ?
		if (!b) return;	// generate character on leading edge of cycle
		// TODO: enforce odd/even columns? Numeric?
		// TODO: generate some punch value?
		trigger(_punch, _char);
	}
}
