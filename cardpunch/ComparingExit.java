// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ComparingExit extends ProgStart {
	int _pos;
	int _wid;

	public ComparingExit(int pos) {
		super(true);
		_pos = pos;
		_wid = 1;
	}

	public int position() { return _pos; }
	public int width() { return _wid; }
	public void expand(int p) {
		if (p >= _pos + _wid) {
			_wid = (p - _pos) + 1;
		}
	}
}
