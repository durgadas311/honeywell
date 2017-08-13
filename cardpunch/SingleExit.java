// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

class SingleExit extends ProgItem {
	public SingleExit() {
		super(1);
		ents[0] = new ProgStart(false);
	}

	@Override
	public void reset() {
		ents[0].reset();
	}

	public SingleExit(ProgStart ps) {
		super(1);
		ents[0] = ps;
	}
}
