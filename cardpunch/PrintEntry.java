// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class PrintEntry implements ProgEntry {
	int _col;
	char[] _line;

	public PrintEntry(char[] line, int col) {
		_col = col;
		_line = line;
	}

	public void putCol(char c) {
		// TODO: Alphameric vs. Numeric
		_line[_col] = c;
	}
}
