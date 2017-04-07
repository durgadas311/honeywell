// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class TapeLoader extends BRTLoader implements Loader {
	private OutputStream targ = null;
	private RandomAccessFile rwf = null;

	public TapeLoader(OutputStream f, CharConverter cvt) {
		super(cvt, 250);
		targ = f;
	}

	public TapeLoader(RandomAccessFile f, CharConverter cvt) {
		super(cvt, 250);
		rwf = f;
	}

	void writeRec(byte[] rec, int len) {
		try {
			if (targ != null) {
				targ.write(rec, 0, len);
				targ.write(0300); // tape record mark
			} else {
				rwf.write(rec, 0, len);
				rwf.write(0300); // tape record mark
			}
		} catch (Exception ee) {}
	}
}
