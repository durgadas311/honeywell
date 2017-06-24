// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class CardInputStream extends InputStream {
	private BufferedReader input;
	private CharConverter cvt;

	// Allows reading "punch card data" from the same input stream
	// used for compiling the program. Converts ASCII lines into
	// punch cards. An instance of this class is installed in
	// the punch card reader peripheral, before executing the
	// program.
	public CardInputStream(BufferedReader in, CharConverter cvt) {
		input = in;
		this.cvt = cvt;
	}

	// This is the onlu read routine to be used
	public int read(byte[] card) {
		if (input == null || card.length != 160) {
			return -1;
		}
		int n = getCard(card);
		if (n < 0) {
			return -1;
		}
		return 160;
	}

	public int available() { return 160; } // TODO:
	public int read() { return -1; }
	public int read(byte[] b, int s, int n) { return -1; }
	public boolean markSupported() { return false; }
	public void mark(int limit) {}
	public void reset() {}
	public long skip(long sk) { return -1; }

	public void close() {
		try {
			input.close();
		} catch (Exception ee) {}
		input = null;
	}

	private int getCard(byte[] card) {
		String line = null;
		try {
			line = input.readLine();
		} catch (Exception ee) {}
		if (line == null) {
			return -1;
		}
		int n = line.length();
		int x;
		for (x = 0; x < 80 && x < n; ++x) {
			char c = Character.toUpperCase(line.charAt(x));
			int ix = CharConverter.hwAsciiSup.indexOf(c);
			if (ix >= 0) {
				c = CharConverter.hwAsciiRep.charAt(ix);
			}
			byte a = cvt.asciiToHw((byte)(c & 0x7f));
			// TODO: support alternate codes
			int p = cvt.hwToPun(a, false);
			putCol(card, x, p);
		}
		while (x < 80) {
			// no punches == 0 == "blank"
			putCol(card, x++, 0);
		}
		return 160;
	}

	// This must use same endianess as P_CardReaderPunch, et al.
	// (PunchCard, CardLoader, ?)
	private void putCol(byte[] card, int ix, int p) {
		card[ix * 2] = (byte)(p & 0x0ff);
		card[ix * 2 + 1] = (byte)((p >> 8) & 0x0ff);
	}
}
