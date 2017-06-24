// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class PeriphLoader extends BRTLoader {
	private SequentialRecordIO targ;

	public PeriphLoader(SequentialRecordIO targ, CharConverter cvt,
				long vis, int rev, int reclen) {
		super(cvt, vis, rev, reclen);
		this.targ = targ;
	}

	boolean writeRec(byte[] rec, int len) {
		return targ.appendRecord(rec, 0, len);
	}

	boolean endSeg() {
		return true;
	}

	boolean beginSeg(String rev, String prg, String seg, long vis) {
		return true;
	}
}
