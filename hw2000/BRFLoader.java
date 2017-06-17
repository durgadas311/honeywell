// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class BRFLoader extends BRTLoader {
	private DiskFile targ;	// assumed to be Partitioned Sequential
	private CoreMemory buf;
	private CoreMemory mmb;

	// Caller opens, and closes, the file.
	// But member open/close must be handled here...
	public BRFLoader(DiskFile targ, CharConverter cvt) {
		super(cvt, targ.itemLen());
		this.targ = targ;
		buf = new BufferMemory(record);
		mmb = new BufferMemory(14);
		targ.endMemb();
	}

	void writeRec(byte[] rec, int len) {
		// TODO: how to report errors?
		buf.zero(reccnt, reclen - reccnt);
		boolean ok = targ.putItem(buf, 0);
		if (!ok) {
System.err.format("MSPUT: %s\n", FileVolSupport.getError(targ.getError()));
		}
	}

	void endSeg() {
		boolean ok = targ.endMemb();
		if (!ok) {
System.err.format("ENDM: %s\n", FileVolSupport.getError(targ.getError()));
		}
	}

	void beginSeg(String rev, String prg, String seg, long vis) {
		String m = String.format("%-6s%-2s      ", prg, seg);
		int x = 0;
		for (byte c : m.getBytes()) {
			mmb.writeChar(x++, cvt.asciiToHw(c));
		}

		// TODO: handle error - also handle duplicates
		boolean ok = targ.setMemb(mmb, 0, DiskFile.OUT);
		if (!ok) {
System.err.format("SETM: %s\n", FileVolSupport.getError(targ.getError()));
		}
	}
}
