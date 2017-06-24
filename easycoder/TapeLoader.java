// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class TapeLoader extends BRTLoader {
	private OutputStream targ = null;
	private RandomAccessFile rwf = null;

	public TapeLoader(OutputStream f, CharConverter cvt, long vis, int rev) {
		super(cvt, vis, rev, 250);
		targ = f;
	}

	public TapeLoader(RandomAccessFile f, CharConverter cvt, long vis, int rev) {
		super(cvt, vis, rev, 250);
		rwf = f;
	}

	boolean writeRec(byte[] rec, int len) {
		try {
			if (targ != null) {
				targ.write(rec, 0, len);
				targ.write(0300); // tape record mark
			} else {
				rwf.write(rec, 0, len);
				rwf.write(0300); // tape record mark
			}
		} catch (Exception ee) {
			error = 00501;
			return false;
		}
		return true;
	}

	boolean endSeg() {
		return true;
	}

	boolean beginSeg(String rev, String prg, String seg, long vis) {
		return true;
	}
}
