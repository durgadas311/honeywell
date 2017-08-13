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
	public void putCol(int p, char c) {
		if (Character.isDigit(c)) {
			_ctr.accum(c - '0', _col);
		}
	}
}

