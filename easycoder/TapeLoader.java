// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class TapeLoader extends BRTLoader implements Loader {
	private OutputStream targ;

	public TapeLoader(OutputStream targ, CharConverter cvt) {
		super(cvt, 250);
		this.targ = targ;
	}

	void writeRec(byte[] rec, int len) {
		try {
			targ.write(rec, 0, len);
			targ.write(0300); // tape record mark
		} catch (Exception ee) {}
	}
}
