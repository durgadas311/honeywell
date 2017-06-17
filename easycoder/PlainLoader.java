// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;
import java.util.Arrays;

// Simple contiguous stream of BRT control strings. No formal records.
// May include 77 codes but in this case they are no-oop.
//
public class PlainLoader extends BRTDataField implements Loader {
	private OutputStream targ;

	public PlainLoader(OutputStream targ, CharConverter cvt, int reclen) {
		super(cvt, reclen);
		this.targ = targ;
	}

	void finRec(boolean last) {
		endRec(last);
		try {
			targ.write(record, 0, reccnt);
		} catch (Exception ee) {
			ee.printStackTrace();
		}
		dirty = false;
	}

	void initRec() {
		reccnt = 0;
	}

	public void begin(int adr, String prg, String seg) {
		dist = -1;
		dirty = false;
	}

	public void segment(String prg, String seg) {
		if (dirty) {
			System.err.format("WARNING: SEG after code\n");
		}
	}
}
