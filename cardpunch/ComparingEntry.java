// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ComparingEntry extends ProgStart {
	int punch;

	public ComparingEntry() {
		super(true);
	}

	@Override
	public void putCol(int p, char c) {
		punch = p;
	}

	public boolean compare(ComparingEntry ent) {
		return punch == ent.punch;
	}
}
