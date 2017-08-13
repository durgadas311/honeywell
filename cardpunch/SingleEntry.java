// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

class SingleEntry extends ProgItem {
	public SingleEntry() {
		super(1);
		exit = false;
		ents[0] = new ProgStart(false);
	}

	@Override
	public void reset() {
		ents[0].reset();
	}

	public SingleEntry(ProgStart ps) {
		super(1);
		exit = false;
		ents[0] = ps;
	}
}
