// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ComparingEntry extends ProgEntry {
	char punch;

	public ComparingEntry(int pos) {
		super(pos);
	}

	public void putCol(char c) {
		punch = c;
	}

	public boolean compare(ComparingEntry ent) {
		return punch == ent.punch;
	}
}
