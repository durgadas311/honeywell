// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class BRTLoader extends BRTDataField implements Loader {
	private int seq;
	private long vis;
	private int rev;

	public BRTLoader(CharConverter cvt, long vis, int rev, int reclen) {
		super(cvt, reclen);
		this.vis = vis;
		this.rev = rev % 1000;
		seq = 0;
	}

	abstract boolean writeRec(byte[] rec, int len);
	abstract boolean endSeg();
	abstract boolean beginSeg(String rev, String prg, String seg, long vis);

	boolean finRec(boolean last) {
		endRec(last);
		++seq;
		putLen(reccnt);
		if (last) {
			record[0] &= ~07;
			record[0] |= (byte)04;
		}
		if (!writeRec(record, reccnt)) {
			return false;
		}
		dirty = false;
		if (last && !endSeg()) {
			return false;
		}
		return true;
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
		record[0] = (byte)041; // modified to 044 at end if last
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

	private boolean initSeg(String prg, String seg, long vis) {
		String rev = String.format("%03d", this.rev);
		if (!beginSeg(rev, prg, seg, vis)) {
			return false;
		}
		record[0] = (byte)050; // modified to 054 at end if last
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
		return true;
	}

	public boolean begin(int adr, String prg, String seg) {
		if (!initSeg(prg, seg, vis)) {
			return false;
		}
		// setAdr(adr); // let first setCode do this...
		dist = -1;
		dirty = false;
		return true;
	}

	public boolean segment(String prg, String seg) {
		if (dirty) {
			System.err.format("WARNING: SEG after code\n");
		}
		// only 'seg' should be different...
		return initSeg(prg, seg, vis);
	}
}
