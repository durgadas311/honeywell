// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class PunchingEntry extends ProgStart {
	int _col;
	short[] _card;

	public PunchingEntry(short[] card, int col) {
		super(true);
		_col = col;
		_card = card; // punch buffer, not actual card
	}

	@Override
	public void putCol(int p, char c) {
		_card[_col] = (short)(p & 0x0fff);
	}
}
