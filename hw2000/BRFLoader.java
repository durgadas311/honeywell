// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// Our only real option is to replace any existing member of the same name,
// since we don't know if the new size might overrun existing area.
// So, we always attempt to delete any member of the same name.
public class BRFLoader extends BRTLoader {
	private DiskFile targ;	// assumed to be Partitioned Sequential
	private CoreMemory buf;
	private CoreMemory mmb;

	// Caller opens, and closes, the file.
	// But member open/close must be handled here...
	public BRFLoader(DiskFile targ, CharConverter cvt, long vis, int rev) {
		super(cvt, vis, rev, targ.itemLen());
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
		String m = String.format("%-6s%-2s", prg, seg);
		int x = 0;
		// We know 'mmb' is 0-based
		for (byte c : m.getBytes()) {
			mmb.writeChar(x++, cvt.asciiToHw(c));
		}
		for (int y = 13; y >= x; --y) {
			mmb.writeChar(y, (byte)vis);
			vis >>= 6;
		}
		boolean ok;
		// TODO: handle errors
		ok = targ.alterMemb(mmb, 0, PartitionedSeqFile._DEL_, null, 0);
		if (!ok && targ.getError() != 00213) {
System.err.format("MALTER: %s\n", FileVolSupport.getError(targ.getError()));
		}
		ok = targ.setMemb(mmb, 0, DiskFile.OUT);
		if (!ok) {
System.err.format("SETM: %s\n", FileVolSupport.getError(targ.getError()));
		}
	}
}
