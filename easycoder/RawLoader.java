// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class RawLoader implements Loader {
	OutputStream out;
	Assembler asm;
	int curadr;
	String last = null;
	int line = 1;
	int reclen = 250;
	int cnt = 0;

	public RawLoader(OutputStream out, Assembler asm) {
		this.out = out;
		this.asm = asm;	// could be null
	}

	private void oneChar() {
		if (++cnt < reclen) {
			return;
		}
		cnt = 0;
		try {
			out.write(0301);
		} catch (Exception ee) {}
	}

	private void makeSW(int adr) {
		String sym = asm.lookupAdr(adr);
		if (sym == null) {
			System.err.format("Unresolved address %07o\n", adr);
		} else {
			int d = adr - asm.lookupSym(sym);
			if (last == null) {
				last = String.format("%s+%d", sym, d);
			} else {
				System.err.format("%05d         SW    %s,%s+%d\n",
					line++, last, sym, d);
				last = null;
			}
		}
	}

	public void begin(int adr, String prg, String seg, String rev, int vis) {
		curadr = adr;
	}

	public void setCode(int adr, byte[] code) {
		if (asm != null) {
			// Very simplistic, assume every block starts with WM...
			// Since this is intended for bootstrap programs,
			// that is probably realistic.
			makeSW(adr);
		}
		try {
			while (curadr < adr) {
				oneChar();
				out.write((byte)0);
				++curadr;
			}
			for (int y = 0; y < code.length; ++y) {
				oneChar();
				out.write(code[y] & 077);
			}
			curadr += code.length;
		} catch (Exception ee) {}
	}

	public void clear(int start, int end, byte fill) {
		try {
			for (int y = start; y <= end; ++y) {
				oneChar();
				out.write(fill);
			}
		} catch (Exception ee) {}
	}

	public void end(int start) {
		if (last != null) {
			System.err.format("%05d         SW    %s\n", line++, last);
			last = null;
		}
		try {
			if (cnt > 0) {
				out.write(0301);
			}
			out.write(0301);
		} catch (Exception ee) {}
	}
}
