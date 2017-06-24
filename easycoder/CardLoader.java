// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.io.*;

public class CardLoader extends BRTLoader {
	private OutputStream targ = null;
	private RandomAccessFile rwf = null;
	private byte[] card;
	private int csq;

	public CardLoader(OutputStream f, CharConverter cvt, long vis, int rev) {
		super(cvt, vis, rev, 80);
		targ = f;
		card = new byte[2*80];
		csq = 1;
	}

	public CardLoader(RandomAccessFile f, CharConverter cvt, long vis, int rev) {
		super(cvt, vis, rev, 80);
		rwf = f;
		card = new byte[2*80];
		csq = 1;
	}

	boolean writeRec(byte[] rec, int len) {
		int x;
		rec[1] = (byte)((csq / 100) % 10);
		rec[2] = (byte)((csq / 10) % 10);
		rec[3] = (byte)((csq / 1) % 10);
		rec[4] = 015;	// blank
		rec[5] = 015;	// blank
		++csq;
		Arrays.fill(card, (byte)0);
		// For convenience, "print" the header columns...
		int hdr = rec[6];
		for (x = 0; x < len; ++x) {
			int p = cvt.hwToPun(rec[x], true);
			if (x < hdr) {
				p |= 0x1000;
			}
			card[x * 2] = (byte)p;
			card[x * 2 + 1] = (byte)(p >> 8);
		}
		try {
			if (targ != null) {
				targ.write(card);
			} else {
				rwf.write(card);
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
