// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class CounterEntry extends ProgEntry {
	Counter _ctr;

	public CounterEntry(Counter ctr, int col) {
		super(col, null);
		_ctr = ctr;
	}

	public CounterEntry(Counter ctr, int col, ProgEntry next) {
		super(col, next);
		_ctr = ctr;
	}

	public void putCol(char c) {
		if (c >= '0' && c <= '9') {
			_ctr.accum(c - '0', _col);
		}
		if (_next != null) {
			_next.putCol(c);
		}
	}
}

