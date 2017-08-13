// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class PrintEntry extends ProgStart {
	int _col;
	char[] _line;

	public PrintEntry(char[] line, int col) {
		super(true);
		_col = col;
		_line = line;
	}

	@Override
	public void putCol(int p, char c) {
		// TODO: Alphameric vs. Numeric
		_line[_col] = c;
	}
}
