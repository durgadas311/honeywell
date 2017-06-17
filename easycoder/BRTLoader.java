// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class BRTLoader extends BRTDataField implements Loader {
	private int seq;

	public BRTLoader(CharConverter cvt, int reclen) {
		super(cvt, reclen);
		seq = 0;
	}

	abstract void writeRec(byte[] rec, int len);
	abstract void endSeg();
	abstract void beginSeg(String rev, String prg, String seg, long vis);

	void finRec(boolean last) {
		endRec(last);
		++seq;
		putLen(reccnt);
		if (last) {
			record[0] &= ~07;
			record[0] |= (byte)04;
		}
		writeRec(record, reccnt);
		dirty = false;
		if (last) {
			endSeg();
		}
	}

	private void putLen(int len) {
		record[1] = (byte)((len >> 12) & 0x3f);
		record[2] = (byte)((len >> 6) & 0x3f);
		record[3] = (byte)((len >> 0) & 0x3f);
	}

	private void putSeq(int seq) {
		record[4] = (byte)((seq >> 6) & 0x3f);
		record[5] = (byte)((seq >> 0) & 0x3f);
	}

	void initRec() {
		reccnt = 0;
		record[0] = (byte)041;
		putLen(0);	// updated later...
		putSeq(seq);
		reccnt = 7;
		record[6] = (byte)reccnt;
	}

	// Strings must have already been truncated/padded to exact field length.
	private void putStr(String str) {
		for (byte c : str.getBytes()) {
			record[reccnt++] = cvt.asciiToHw(c);
		}
	}

	private void initSeg(String rev, String prg, String seg, long vis) {
		beginSeg(rev, prg, seg, vis);
		if (seq > 0) {
			record[0] = (byte)054;
		} else {
			record[0] = (byte)050;
		}
		putLen(0);	// updated later...
		putSeq(seq);	//
		reccnt = 7;
		putStr(rev);
		putStr(prg);
		putStr(seg);
		putAdr((int)(vis >> 18));
		putAdr((int)(vis));
		// assert reccnt == 24...
		reccnt = 24;
		record[6] = (byte)reccnt;
		seq = 1;
	}

	public void begin(int adr, String prg, String seg, String rev, long vis) {
		initSeg(rev, prg, seg, vis);
		// setAdr(adr); // let first setCode do this...
		dist = -1;
		dirty = false;
	}

	public void segment(String prg, String seg, String rev, long vis) {
		if (dirty) {
			System.err.format("WARNING: SEG after code\n");
		}
		// only 'seg' should be different...
		initSeg(rev, prg, seg, vis);
	}
}
