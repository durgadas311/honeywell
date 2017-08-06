// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class ComparingExit {
	int _pos;
	int _wid;
	ProgStart _start;

	public ComparingExit(int pos, int wid, ProgStart srt) {
		super();
		_pos = pos;
		_wid = wid;
		_start = srt;
	}

	public int position() { return _pos; }
	public int width() { return _wid; }
	public void start() {
		_start.set(true);
	}
}
