// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class BRFLoader extends BRTDataField implements Loader {
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

	void finRec(boolean last) {
		endRec(last);
		// TODO: how to report errors?
		buf.zero(reccnt, reclen - reccnt);
		boolean ok = targ.putItem(buf, 0);
		if (!ok) {
System.err.format("MSPUT: %s\n", FileVolSupport.getError(targ.getError()));
		}
		dirty = false;
		if (last) {
			ok = targ.endMemb();
			if (!ok) {
System.err.format("ENDM: %s\n", FileVolSupport.getError(targ.getError()));
			}
		}
	}

	void initRec() {
		reccnt = 0;
	}

	private void initSeg(String rev, String prg, String seg, int vis) {
		String m = String.format("%-6s%-2s%06o", prg, seg, vis);
		int x = 0;
		for (byte c : m.getBytes()) {
			mmb.writeChar(x++, cvt.asciiToHw(c));
		}
		// TODO: handle error
		boolean ok = targ.setMemb(mmb, 0, DiskFile.OUT);
		if (!ok) {
System.err.format("SETM: %s\n", FileVolSupport.getError(targ.getError()));
		}
	}

	public void begin(int adr, String prg, String seg, String rev, int vis) {
		initSeg(rev, prg, seg, vis);
		dist = -1;
		dirty = false;
	}

	public void segment(String prg, String seg, String rev, int vis) {
		if (dirty) {
			// TODO: need to return error
			System.err.format("WARNING: SEG after code\n");
		}
		// only 'seg' should be different...
		initSeg(rev, prg, seg, vis);
	}
}
