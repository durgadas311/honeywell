// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class BRTDataField implements Loader {
	protected int dist = -1;
	protected int reclen;
	protected int reccnt;
	protected byte[] record;
	protected CharConverter cvt;
	protected boolean dirty = false;
	protected int error = 0;

	public BRTDataField(CharConverter cvt, int reclen) {
		this.cvt = cvt;
		this.reclen = reclen;
		reccnt = 0;
		record = new byte[reclen + 6];
	}

	abstract void initRec();
	abstract boolean finRec(boolean last);

	protected void putAdr(int adr) {
		record[reccnt++] = (byte)((adr >> 12) & 077);
		record[reccnt++] = (byte)((adr >> 6) & 077);
		record[reccnt++] = (byte)((adr >> 0) & 077);
	}

	protected void endRec(boolean last) {
		if (!last) {
			record[reccnt++] = 077;	// read next record
		}
	}

	// Data always follows...
	private boolean mkSpace(int len) {
		dirty = true;
		if (reccnt + len >= reclen) {
			if (!finRec(false)) {
				return false;
			}
			initRec();
		}
		return true;
	}

	private boolean setAdr(int adr, int cc) {
		if (!mkSpace(4)) {
			return false;
		}
		if (cc == 060) dist = adr;
		if (adr > 0777777) cc |= 010;
		record[reccnt++] = (byte)cc;
		putAdr(adr);
		return true;
	}

	private boolean kludge(int adr, byte[] code) {
		byte[] b1 = new byte[1];
		byte[] b2 = new byte[code.length - 1];
		b1[0] = code[0];
		System.arraycopy(code, 1, b2, 0, b2.length);
		if (!setCode(adr, b1) || !setCode(adr + 1, b2)) {
			return false;
		}
		return true;
	}

	// Only called for lengths <= 15
	private boolean setCode(int adr, byte[] code, byte ctrl, int start, int end) {
		int len = (end - start);
		ctrl |= len;
		if (!mkSpace(len + 1)) {
			return false;
		}
		record[reccnt++] = ctrl;
		for (int y = start; y < end; ++y) {
			record[reccnt++] = (byte)(code[y] & 077);
		}
		dist += (end - start);
		return true;
	}

	// TODO: reloc should be 0...
	public boolean setCode(int adr, byte[] code) {
		int len = code.length;
		byte ctrl = (byte)0;
		// TODO: how is RM handled? Is RM ever at start of field?
		// 1-char segments use the post-punctuation method...
		if (len > 1) {
			if ((code[0] & 0300) == 0300) {
				// Must handle special case that doesn't fit BRT...
				return kludge(adr, code);
			} else if ((code[0] & 0100) != 0) {
				ctrl |= 0020;
			} else if ((code[0] & 0200) != 0) {
				ctrl |= 0040;
			}
		}
		if (dist != adr && !setAdr(adr, 060)) {
			return false;
		}
		int n = 0;
		while (len - n > 15) {
			if (!setCode(adr, code, ctrl, n, n + 15)) {
				return false;
			}
			n += 15;
			adr += 15;
			ctrl = 0;
		}
		if (!setCode(adr, code, ctrl, n, len)) {
			return false;
		}
		if ((code[len - 1] & 0100) != 0) {
			if (!mkSpace(1)) {
				return false;
			}
			record[reccnt++] = (byte)063;
		}
		if ((code[len - 1] & 0200) != 0) {
			if (!mkSpace(1)) {
				return false;
			}
			record[reccnt++] = (byte)064;
		}
		return true;
	}

	// either (start > 0777777 && end > 0777777)
	//     or (start <= 0777777 && end <= 0777777)
	// TODO: if spans boundary, split into two CLEARs.
	public boolean clear(int start, int end, byte fill) {
		if (!mkSpace(8) || !setAdr(start, 062)) {
			return false;
		}
		putAdr(end);
		record[reccnt++] = fill;
		return true;
	}

	public boolean range(int start, int end) {
		if (!setAdr(start, 060) || !setAdr(end, 060)) {
			return false;
		}
		return true;
	}

	public boolean exec(int start) {
		return end(start);
	}

	public boolean end(int start) {
		setAdr(start, 061);
		return finRec(true);
	}

	public int getError() { return error; }
}
