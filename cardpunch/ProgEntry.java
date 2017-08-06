// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public abstract class ProgEntry {
	int _col;
	ProgEntry _next;

	public ProgEntry(int col, ProgEntry next) {
		_col = col;
		_next = next;
	}

	abstract void putCol(char c);

	// TODO: reject duplicates
	public void setNext(ProgEntry ent) { _next = ent; }
}
