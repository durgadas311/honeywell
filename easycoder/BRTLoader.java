// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class BRTLoader implements Loader {
	private int dist = -1;
	private int reclen;
	private int reccnt;
	private OutputStream targ;

	public BRTLoader(OutputStream targ, int reclen) {
		this.targ = targ;
		this.reclen = reclen;
		reccnt = 0;
	}

	private void putAdr(int adr) throws Exception {
		targ.write((adr >> 12) & 0x3f);
		targ.write((adr >> 6) & 0x3f);
		targ.write((adr >> 0) & 0x3f);
	}

	private void mkSpace(int len) throws Exception {
		if (reccnt + len >= reclen) {
			targ.write(077);
			targ.write(0301); // tape record mark
			reccnt = 0;
		}
		reccnt += len;
	}

	private void setAdr(int adr) throws Exception {
		mkSpace(4);
		targ.write(060);
		putAdr(adr);
		dist = adr;
	}

	public void begin(int adr) {
		try {
			setAdr(adr);
		} catch (Exception ee) {}
	}

	// TODO: reloc should be 0...
	public void setCode(int adr, byte[] code) {
		int len = code.length;
		byte ctrl = (byte)len;
		// TODO: how is RM handled? Is RM ever at start of field?
		if ((code[0] & 0100) != 0) {
			ctrl |= 0020;
		} else if ((code[0] & 0200) != 0) {
			ctrl |= 0040;
		}
		try {
			if (dist != adr) {
				setAdr(adr);
			}
			mkSpace(len + 1);
			targ.write(ctrl);
			for (int y = 0; y < len; ++y) {
				targ.write(code[y] & 0x3f);
			}
			dist += len;
			if ((code[len - 1] & 0100) != 0) {
				mkSpace(1);
				targ.write(063);
			}
			if ((code[len - 1] & 0200) != 0) {
				mkSpace(1);
				targ.write(064);
			}
		} catch (Exception ee) {}
	}

	public void clear(int start, int end, byte fill) {
		try {
			mkSpace(8);
			targ.write(061);
			putAdr(start);
			putAdr(end);
			targ.write(fill);
		} catch (Exception ee) {}
	}

	public void end(int start) {
		try {
			mkSpace(4);
			targ.write(061);
			putAdr(start);
			targ.write(0301); //
			targ.write(0301); // tape file mark
		} catch (Exception ee) {}
	}
}
