// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class PrintEntry extends ProgEntry {
	char[] _line;

	public PrintEntry(char[] line, int col) {
		super(col, null);
		_line = line;
	}

	public PrintEntry(char[] line, int col, ProgEntry next) {
		super(col, next);
		_line = line;
	}

	public void putCol(char c) {
		// TODO: Alphameric vs. Numeric
		_line[_col] = c;
		if (_next != null) {
			_next.putCol(c);
		}
	}
}
