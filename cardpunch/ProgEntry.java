// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public abstract class ProgEntry {
	int _col;

	public ProgEntry(int col) {
		_col = col;
	}

	abstract void putCol(char c);
}
