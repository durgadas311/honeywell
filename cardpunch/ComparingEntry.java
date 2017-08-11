// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ComparingEntry extends ProgStart {
	char punch;

	public ComparingEntry() {
		super(true);
	}

	@Override
	public void putCol(char c) {
		punch = c;
	}

	public boolean compare(ComparingEntry ent) {
		return punch == ent.punch;
	}
}
