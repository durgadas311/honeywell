// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.io.*;

public class CardLoader extends BRTLoader implements Loader {
	private OutputStream targ;
	private byte[] card;
	private int csq;

	public CardLoader(OutputStream targ, CharConverter cvt) {
		super(cvt, 80);
		this.targ = targ;
		card = new byte[2*80];
		csq = 1;
	}

	void writeRec(byte[] rec, int len) {
		int x;
		rec[1] = (byte)((csq / 100) % 10);
		rec[2] = (byte)((csq / 10) % 10);
		rec[3] = (byte)((csq / 1) % 10);
		++csq;
		Arrays.fill(card, (byte)0);
		for (x = 0; x < len; ++x) {
			int p = cvt.hwToPun(rec[x], true);
			card[x * 2] = (byte)p;
			card[x * 2 + 1] = (byte)(p >> 8);
		}
		try {
			targ.write(card);
		} catch (Exception ee) {}
	}
}
