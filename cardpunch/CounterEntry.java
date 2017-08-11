// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class CounterEntry extends ProgStart {
	int _col;
	Counter _ctr;

	public CounterEntry(Counter ctr, int col) {
		super(true);
		_col = col;
		_ctr = ctr;
	}

	@Override
	public void putCol(char c) {
		if (c >= '0' && c <= '9') {
			_ctr.accum(c - '0', _col);
		}
	}
}

