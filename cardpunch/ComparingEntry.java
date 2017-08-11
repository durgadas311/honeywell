// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ComparingEntry implements ProgEntry {
	int col;
	char punch;

	public ComparingEntry(int pos) {
		col = pos;
	}

	public void putCol(char c) {
		punch = c;
	}

	public boolean compare(ComparingEntry ent) {
		return punch == ent.punch;
	}
}
