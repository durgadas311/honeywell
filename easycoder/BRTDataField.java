// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public abstract class BRTDataField implements Loader {
	protected int dist = -1;
	protected int reclen;
	protected int reccnt;
	protected byte[] record;
	protected CharConverter cvt;
	protected boolean dirty = false;

	public BRTDataField(CharConverter cvt, int reclen) {
		this.cvt = cvt;
		this.reclen = reclen;
		reccnt = 0;
		record = new byte[reclen + 6];
	}

	abstract void initRec();
	abstract void finRec(boolean last);

	protected void putAdr(int adr) {
		record[reccnt++] = (byte)((adr >> 12) & 0x3f);
		record[reccnt++] = (byte)((adr >> 6) & 0x3f);
		record[reccnt++] = (byte)((adr >> 0) & 0x3f);
	}

	protected void endRec(boolean last) {
		if (!last) {
			record[reccnt++] = 077;	// read next record
		}
	}

	// Data always follows...
	private void mkSpace(int len) {
		dirty = true;
		if (reccnt + len >= reclen) {
			finRec(false);
			initRec();
		}
	}

	private void setAdr(int adr) {
		mkSpace(4);
		record[reccnt++] = 060;
		putAdr(adr);
		dist = adr;
	}

	private void kludge(int adr, byte[] code) {
		byte[] b1 = new byte[1];
		byte[] b2 = new byte[code.length - 1];
		b1[0] = code[0];
		System.arraycopy(code, 1, b2, 0, b2.length);
		setCode(adr, b1);
		setCode(adr + 1, b2);
	}

	// Only called for lengths <= 15
	private void setCode(int adr, byte[] code, byte ctrl, int start, int end) {
		int len = (end - start);
		ctrl |= len;
		mkSpace(len + 1);
		record[reccnt++] = ctrl;
		for (int y = start; y < end; ++y) {
			record[reccnt++] = (byte)(code[y] & 0x3f);
		}
		dist += (end - start);
	}

	// TODO: reloc should be 0...
	public void setCode(int adr, byte[] code) {
		int len = code.length;
		byte ctrl = (byte)0;
		// TODO: how is RM handled? Is RM ever at start of field?
		// 1-char segments use the post-punctuation method...
		if (len > 1) {
			if ((code[0] & 0300) == 0300) {
				// Must handle special case that doesn't fit BRT...
				kludge(adr, code);
				return;
			} else if ((code[0] & 0100) != 0) {
				ctrl |= 0020;
			} else if ((code[0] & 0200) != 0) {
				ctrl |= 0040;
			}
		}
		if (dist != adr) {
			setAdr(adr);
		}
		int n = 0;
		while (len - n > 15) {
			setCode(adr, code, ctrl, n, n + 15);
			n += 15;
			adr += 15;
			ctrl = 0;
		}
		setCode(adr, code, ctrl, n, len);
		if ((code[len - 1] & 0100) != 0) {
			mkSpace(1);
			record[reccnt++] = (byte)063;
		}
		if ((code[len - 1] & 0200) != 0) {
			mkSpace(1);
			record[reccnt++] = (byte)064;
		}
	}

	public void clear(int start, int end, byte fill) {
		mkSpace(8);
		record[reccnt++] = (byte)061;
		putAdr(start);
		putAdr(end);
		record[reccnt++] = fill;
	}

	public void range(int start, int end) {
		setAdr(start);
		setAdr(end);
	}

	public void exec(int start) {
		end(start);
	}

	public void end(int start) {
		mkSpace(4);
		record[reccnt++] = (byte)061;
		putAdr(start);
		finRec(true);
	}
}
