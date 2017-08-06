// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ComparingEntry extends ProgEntry {
	char punch;

	public ComparingEntry(int pos, ProgEntry next) {
		super(pos, next);
	}

	public void putCol(char c) {
		punch = c;
		if (_next != null) {
			_next.putCol(c);
		}
	}

	public boolean compare(ComparingEntry ent) {
		return punch == ent.punch;
	}
}
