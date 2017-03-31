// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.io.*;

public class RawLoader implements Loader {
	OutputStream out;
	Assembler asm;
	int curadr;
	String lastw = null;
	String lasti = null;
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
			if (lastw == null) {
				lastw = String.format("%s+%d", sym, d);
			} else {
				System.err.format("%05d         SW    %s,%s+%d\n",
					line++, lastw, sym, d);
				lastw = null;
			}
		}
	}

	private void makeSI(int adr) {
		String sym = asm.lookupAdr(adr);
		if (sym == null) {
			System.err.format("Unresolved address %07o\n", adr);
		} else {
			int d = adr - asm.lookupSym(sym);
			if (lasti == null) {
				lasti = String.format("%s+%d", sym, d);
			} else {
				System.err.format("%05d         SI    %s,%s+%d\n",
					line++, lasti, sym, d);
				lasti = null;
			}
		}
	}

	public void begin(int adr, String prg, String seg, String rev, int vis) {
		curadr = adr;
	}

	public void setCode(int adr, byte[] code) {
		if (asm != null) {
			// Very simplistic, examines only punc at start of block
			// Since this is intended for bootstrap programs,
			// that is probably realistic.
			if ((code[0] & 0100) != 0) {
				makeSW(adr);
			}
			if ((code[0] & 0200) != 0) {
				makeSI(adr);
			}
			int n = code.length - 1;
			if (n > 0) {
				if ((code[n] & 0100) != 0) {
					makeSW(adr + n);
				}
				if ((code[n] & 0200) != 0) {
					makeSI(adr + n);
				}
			}
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
		if (lastw != null) {
			System.err.format("%05d         SW    %s\n", line++, lastw);
			lastw = null;
		}
		if (lasti != null) {
			System.err.format("%05d         SI    %s\n", line++, lasti);
			lasti = null;
		}
		try {
			if (cnt > 0) {
				out.write(0301);
			}
			out.write(0301);
		} catch (Exception ee) {}
	}
}
