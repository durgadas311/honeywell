// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class PeriphLoader extends BRTLoader implements Loader {
	private SequentialRecordIO targ;

	public PeriphLoader(SequentialRecordIO targ, CharConverter cvt, int reclen) {
		super(cvt, reclen);
		this.targ = targ;
	}

	void writeRec(byte[] rec, int len) {
		targ.appendRecord(rec, 0, len);
	}
}
