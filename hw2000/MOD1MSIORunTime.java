// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;
import java.util.Vector;

public class MOD1MSIORunTime implements HW2000Trap {
	static final String name = "MOD1MSIO";
	private int base = 0;
	private int vbuf = 0;
	private int[] parms;
	private int nparms;

	private HW2000 sys;

	class MCA {
		public int adr;		// address of MCA in program
		public byte[] name;	// file name
		public int mode;	// 1=IN, 2=OUT, 3=IN/OUT, 4=UPDATE
		public int result;	// address of result char
		public int prot;	// protection
		public int deliv;	// item delivery mode
		public int buf1;	// address of block buffer 1
		public int itm1;	// address of item buffer 1
		public int xitDir;	// directory exit routine
		public int xitIdx;	// index exit routine
		public int xitMmb;	// every member exit routine
		public int xitDat;	// data exit routine
		public int xitDev;	// device exit routine
		public int apdAdr;	// address of DSA for APD
		public int cadAdr;	// actual disk address current I/O
		public int ricAdr;	// disk address last item retrieved
		public int vnmAdr;	// volume name
		public int vsnAdr;	// volume serial number
		public int[] devtab;
		public int dd;	// current disk unit
		public int pp;	// current disk pack (always 0)
		public DiskVolume vol;
		public DiskFile file;

		public MCA() {
			name = new byte[10];
			file = null;
		}
	}

	Map<Integer, MCA> mcas;

	private int exitSR;	// original action call return
	private MCA xitMCA;	// current MCA or null if not in EXIT
	private int xitOp;	// current operation if in EXIT
	private int xitRes;	// address of current EXIT result/recovery code
	private int xitErr;	// original error code if in EXIT
	private int xitAct;	// user action code if in EXIT

	P_Console cons;

	public MOD1MSIORunTime(HW2000 sys) {
		this.sys = sys;
		parms = new int[6];
		mcas = new HashMap<Integer, MCA>();
		cons = (P_Console)sys.pdc.getPeriph(PeriphDecode.P_CO);
		reinit();
	}

	public void reinit() {
		endProg(); // in case previous run was unclean
		sys.SR += name.length();
		base = getAdr();
		vbuf = getAdr();
		setupCA();
	}

	public String getName() { return name; }
	static public String name() { return name; }

	public boolean doTrap() {
		if (!(sys.SR < 190 || sys.SR == base || sys.SR == base + 1)) {
			return false;
		}
		if (sys.SR < 190) {
			doSupervisor();
			return true;
		}
		int op;
		nparms = 0;
		if (sys.fp != null) {
			sys.fp.setActive(true);
		}
		try {
			if (sys.SR == base + 1) {
				// Return from EXIT callback...
				sys.SR = exitSR;
				if (xitRes > 0) {
					xitAct = sys.readChar(xitRes);
				}
				op = xitOp;
			} else {
				xitMCA = null; // cancel any prior EXIT...
				// Need parameters to decode call
				sys.SR = sys.BAR;
				// RM single char is end, rest are addresses.
				while ((sys.rawReadMem(sys.SR) & 0300) != 0300) {
					if (nparms >= parms.length) {
						// halt? panic?
						sys.halt = true;
						sys.SR = sys.BAR; // guess
						System.err.format("runaway params %07o\n",
								sys.SR);
						return true;
					}
					parms[nparms++] = getAdr();
				}
				op = sys.rawReadMem(sys.SR++) & 077;
				exitSR = sys.SR;
				xitOp = op;
			}
			switch (op) {
			case 4:	msopen(); break; // MSOPEN
			case 5:	msclos(); break; // MSCLOS
			case 6:	msget(); break; // MSGET
			case 7:	msrep(); break; // MSREP
			case 8:	msput(); break; // MSPUT
			case 9:	setm(); break; // SETM
			case 10: endm(); break; // ENDM
			case 11: malter(); break; // MALTER
			case 12: msrel(); break; // MSREL
			default:
				System.err.format("invalid op %02o\n", op);
				sys.halt = true;
				exit();	// too draconian?
				break;
			}
		} finally {
			if (sys.fp != null) {
				sys.fp.setActive(false);
			}
		}
		return true;
	}

	public void done() {
		// TODO: any cleanup required?
	}

	private void exit() {
		// System.err.format("exit %07o\n", sys.SR);
		sys.removeTrap(this);
	}

	private void doSupervisor() {
		if (sys.SR == 130) { // return for segment load
			// Not supported, just STOP
		} else if (sys.SR == 131) { // return for program exit
			// Nothing special to do, just STOP
			setupCA();
		} else if (sys.SR == 86) {
			// emergency exit... just STOP
			setupCA();
		} else {
			// error. report issue...
			// TODO: trigger exception?
			System.err.format("Invalid supervisor call from %07o\n", sys.BAR);
		}
		endProg();
		sys.SR = sys.BAR;
		sys.halt = true;
	}

	private void setupCA() {
		_putAdr(3, 139, 131); // 3-char prog exit - B (139)...
		_putAdr(4, 164, 131); // 4-char prog exit - B (164)...
		_putAdr(4, 168, 130); // 4-char segm load - B (168)...
		_putAdr(3, 187, 0777777); // memory limit
		//sys.rawWriteChar(155, ?); // Operator mode: panel or console...
		// other initialization?
	}

	// TODO: exit may be error or informative.
	// Need to handle no-exit case appropriately.
	private boolean setupExit(int xiterr) {
		int xit = 0;
		switch ((xiterr >> 6) & 077) {
		case 0:	
			// can't exit - what to do?
			System.err.format("Untrapped error %d\n", xiterr);
			sys.halt = true;
			return false;
		case 1:
			xit = xitMCA.xitDir;
			break;
		case 2:
			xit = xitMCA.xitIdx;
			break;
		case 3:
			xit = xitMCA.xitMmb;
			break;
		case 4:
			xit = xitMCA.xitDat;
			break;
		case 5:
			xit = xitMCA.xitDev;
			break;
		}
		if (xit == 0) {
			return false;
		}
		xitErr = xiterr;
		xitRes = xit - 1;
		putChar(xitRes, xitErr);
		sys.BAR = base + 1;
		sys.SR = xit;
		return true;
	}

	// Fatal conditions, unless handled by program
	private void handleError(int err) {
		if (setupExit(err)) {
			return;
		}
		haltErr(err);
	}

	private void haltErr(int err) {
		// TODO: consult console mode...
		cons.output(FileVolSupport.errmsg.get(err) + '\n');
		sys.AAR = err;
		sys.halt = true;
	}

	private void endProg() {
		xitMCA = null;
		for (MCA mca : mcas.values()) {
			if (mca.file != null) {
				mca.file.close();
				mca.file = null;
			}
		}
		mcas.clear();
		// TODO: any other cleanup?
	}

	// Doesn't check IM...
	private int getAdr() {
		int a = fetchAdr(sys.SR);
		sys.SR += sys.am_na;
		return a;
	}

	private void putAdr(int loc, int val) {
		_putAdr(sys.am_na, loc, val);
	}

	// Put an address based on specified address mode (NOT current CPU mode)
	// 'loc' points to *left* character of address field.
	private void _putAdr(int am, int loc, int val) {
		int a = val;
		for (int n = am - 1; n >= 0; --n) {
			sys.rawWriteChar(loc + n, (byte)a);
			a >>= 6;
		}
	}

	// This is a clone of HW2000.fetchAddr() - keep in sync!
	// 'p' points to *left* character of address field.
	private int fetchAdr(int p) {
		int a = 0;
		for (int n = 0; n < sys.am_na; ++n) {
			a = (a << 6) | (sys.rawReadMem(p++) & 077);
		}
		int x = (a >> sys.am_shift);
		a &= sys.am_mask; // TODO: need? | (ref & ~am_mask);
		if (x == 0) {
			return a;
		}
		if (sys.am_na == 3 && x == 0x07 || x == 0x10) {
			return fetchAdr(a);
		}
		int ix = ((x & 0x0f) * 4) - sys.am_na + 1;
		if (x > 0x10) {
			if (!sys.CTL.isRELOC()) {
				ix += (sys.IBR << 12);
			}
		} else if (sys.am_na == 4) {
			// all set for X1-X15?
		} else {
			if (!sys.CTL.isRELOC()) {
				ix += (sys.SR & ~0x07fff);
			}
		}
		if (sys.CTL.isRELOC()) {
			ix += (sys.BRR << 12);
		}
		a += fetchAdr(ix);
		a &= sys.am_mask; // need ref?
		return a;
	}

	// Works backward until WM...
	private int getInt(int a) {
		int i = 0;
		int b;
		// TODO: re-use routines from instructions?
		for (b = a; b >= 0; --b) {
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
		while (b <= a) {
			i = (i << 6) | (sys.rawReadMem(b++) & 077);
		}
		return i;
	}

	// Works backward until WM...
	private int getStrPtr(int a) {
		int b;
		for (b = a; b >= 0; --b) {
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
		// TODO: detect error?
		return b;
	}

	// DPCCTTRRII... ('P' always 0)
	// Works backward until WM...
	private void putDskAdr(int a, int d, int p, int[] ctri, int num) {
		byte b;
		int i = num - 1;
		for (int x = 0; a - x >= 0; ++x) {
			b = (byte)(sys.rawReadMem(a - x) & ~077);
			if ((x & 1) == 0) {
				if (i >= 0) {
					b |= (byte)(ctri[i] & 077);
				} else {
					b |= (byte)p;
				}
			} else {
				if (i >= 0) {
					b |= (byte)((ctri[i--] >> 6) & 077);
				} else {
					b |= (byte)d;
				}
			}
			sys.rawWriteChar(a - x, b);
			if ((b & 0100) != 0) {
				break;
			}
		}
	}

	private void getChars(int a, byte[] out) {
		int n = out.length;
		// TODO: check WM?
		for (int x = 0; x < n; ++x) {
			out[x] = sys.readChar(a++);
		}
	}

	private void putChar(int a, int out) {
		sys.writeChar(a, (byte)out);
	}

	private void getDevTab(int b, MCA mca) {
		if (b == 0) {
			mca.devtab = new int[1];
			mca.devtab[0] = 0040000;
		}
		int c = b;
		int n = 0;
		// TODO: improve this
		while ((sys.rawReadMem(c) & 0300) != 0300) {
			if ((sys.rawReadMem(c) & 0100) == 0100) {
				++n;
			}
			++c;
		}
		mca.devtab = new int[n];
		b += 2;
		int x = 0;
		while (b < c) {
			mca.devtab[x++] = getInt(b);
			b += 3;
		}
	}

	private void getMCA(int adr) {
		if (mcas.containsKey(adr)) {
			xitMCA = mcas.get(adr);
			return;
		}
		MCA mca = new MCA();
		mca.adr = adr;
		int a = adr;
		getChars(a, mca.name);
		a += mca.name.length;
		mca.result = a++;
		++a;	// skip "unique char"
		mca.prot = sys.readChar(a++);	// protection
		mca.deliv = sys.readChar(a++);	// item delivery mode
		int b = fetchAdr(a); // index/indirect not allowed
		a += sys.am_na;
		getDevTab(b, mca);
		mca.buf1 = fetchAdr(a); // index/indirect allowed?
		a += sys.am_na;
		mca.itm1 = fetchAdr(a); // index/indirect not allowed
		a += sys.am_na;
		// Adjust for LOCATE mode - need leftmost char of address field.
		if (mca.deliv == 0) {
			mca.itm1 -= (sys.am_na - 1);
		}
		mca.xitDir = fetchAdr(a); // index/indirect allowed?
		a += sys.am_na;
		mca.xitIdx = fetchAdr(a); // index/indirect allowed?
		a += sys.am_na;
		mca.xitMmb = fetchAdr(a); // index/indirect allowed?
		a += sys.am_na;
		mca.xitDat = fetchAdr(a); // index/indirect allowed?
		a += sys.am_na;
		mca.xitDev = fetchAdr(a); // index/indirect allowed?
		a += sys.am_na;
		// could use WM to locate fields...
		mca.apdAdr = a;
		a += sys.am_na;
		mca.cadAdr = a + 7; // copy R-L
		a += 8;
		mca.ricAdr = a + 9; // copy R-L
		a += 10;
		mca.vnmAdr = a; // copy L-R
		a += 6;
		mca.vsnAdr = a; // copy L-R
		a += 6;
		// TODO: more data?
		mcas.put(adr, mca);
		xitMCA = mca;
	}

	// Called after resuming from EXIT
	private void updateMCA() {
		int a = xitMCA.adr;
		// TODO: which fields can change?
		a += xitMCA.name.length + 3 + sys.am_na;
		xitMCA.buf1 = fetchAdr(a); // index/indirect allowed?
		a += sys.am_na;
		xitMCA.itm1 = fetchAdr(a); // index/indirect not allowed
		a += sys.am_na;
		if (xitMCA.deliv == 0) {
			xitMCA.itm1 -= (sys.am_na - 1);
		}
		// TODO: more data?
		// Open file must be notified of buffer change...
		if (xitMCA.file != null) {
			// This is bad if I/O has started...
			// Should restrict changes to Item buffer only...
			xitMCA.file.setBuffer(sys, xitMCA.buf1);
		}
	}

	private boolean volOpen(MCA mca) {
		mca.pp = (mca.devtab[0] >> 12) & 077;
		mca.dd = (mca.devtab[0] >> 6) & 077;
		Peripheral p = sys.pdc.getPeriph((byte)mca.pp);
		if (!(p instanceof RandomRecordIO)) {
			handleError(00501);
			return false;
		}
		RandomRecordIO dsk = (RandomRecordIO)p;
		// TODO: share volume mounts between files...
		mca.vol = new DiskVolume(dsk, mca.dd);
		if (!mca.vol.mount()) {
			handleError(mca.vol.getError());
			mca.vol = null;
			return false;
		}
		sys.copyIn(mca.vnmAdr, mca.vol.getName(), 0, 6);
		sys.copyIn(mca.vsnAdr, mca.vol.getSerial(), 0, 6);
		return true;
	}

	// This is broken up into segments that roughly correspond
	// to advisory EXIT points, so that a "continue" at those
	// points can be done.
	private void msopen() {
		if (xitMCA == null) {
			getMCA(parms[0]);
			// TODO: check for file already open?
			xitMCA.mode = parms[1];
		} else {
			// resturning from an EXIT...
			// TODO: work out semantics...
			if (xitAct == 040 || xitAct == (xitErr & 077)) {
				haltErr(xitErr);
				return;
			} else if (xitAct == 021) {
				// force re-open - e.g. disk pack changed
				xitMCA.vol = null;
				xitMCA.file = null;
			} else if (xitAct == 010) {
				// continue. re-load MCA (buffers)
				updateMCA();
			}
		}
		if (xitMCA.vol == null) {
			if (!volOpen(xitMCA)) {
				// Exit already setup...
				return;
			}
		}
		if (xitMCA.file == null) {
			xitMCA.file = xitMCA.vol.openFile(xitMCA.name, xitMCA.mode,
						sys, xitMCA.buf1, sys, vbuf);
			if (xitMCA.file == null) {
				handleError(xitMCA.vol.getError());
				return;
			}
			// allow program to finalize MCA...
			if (setupExit(00101)) {
				putAdr(xitMCA.apdAdr, vbuf);
				return;
			}
		}
		if (xitMCA.deliv == 0) {
			// TODO: not for Partitioned Sequential files?
			putAdr(xitMCA.itm1, xitMCA.file.getItemAdr());
		}
		// TODO: all set?
		xitMCA = null;
	}

	private void msclos() {
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			// do we ever call exits from close?
			haltErr(xitErr);
		}
		if (xitMCA.file != null) {
			xitMCA.file.close();
			xitMCA.file = null;
		}
		xitMCA = null;
	}

	private void msget() {
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			// Any return from EXIT is "fatal"?
			// There are some "10 continue" cases...
			haltErr(xitErr);
			return;
		}
		if (xitMCA.file == null) {	// File not open
			// theoretically can't happen?
			// TODO: what is the right error/exit?
			handleError(00506); // Read error
			return;
		}
		boolean ok;
		if (xitMCA.deliv != 0) {
			ok = xitMCA.file.getItem(sys, xitMCA.itm1);
		} else {
			// This only moves the pointer to next item,
			// but that might involve reading a new block.
			ok = xitMCA.file.getItem();
			if (ok) {
				putAdr(xitMCA.itm1, xitMCA.file.getItemAdr());
			}
		}
		if (!ok) {
			handleError(xitMCA.file.getError());
			return;
		}
		int[] ctri = xitMCA.file.getAddress();
		putDskAdr(xitMCA.ricAdr, xitMCA.dd, 0, ctri, 4);
		putDskAdr(xitMCA.cadAdr, xitMCA.dd, 0, ctri, 3);
		xitMCA = null;
	}

	private void msrep() {
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			// Any return from EXIT is "fatal"?
			// There are some "10 continue" cases...
			haltErr(xitErr);
			return;
		}
		if (xitMCA.file == null) {	// File not open
			// theoretically can't happen?
			// TODO: what is the right error/exit?
			handleError(00510); // Write error
			return;
		}
		boolean ok;
		if (xitMCA.deliv != 0) {
			ok = xitMCA.file.repItem(sys, xitMCA.itm1);
		} else {
			// itm1 address not used here
			ok = xitMCA.file.repItem();
		}
		if (!ok) {
			handleError(xitMCA.file.getError());
			return;
		}
		xitMCA = null;
	}

	private void msput() {
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			// Any return from EXIT is "fatal"?
			// There are some "10 continue" cases...
			haltErr(xitErr);
			return;
		}
		if (xitMCA.file == null) {	// File not open
			// theoretically can't happen?
			// TODO: what is the right error/exit?
			handleError(00510); // Write error
			return;
		}
		boolean ok;
		if (xitMCA.deliv != 0) {
			ok = xitMCA.file.putItem(sys, xitMCA.itm1);
		} else {
			ok = xitMCA.file.putItem();
			if (ok) {
				// NOTE: this is *next* item address
				putAdr(xitMCA.itm1, xitMCA.file.getItemAdr());
			}
		}
		if (!ok) {
			handleError(xitMCA.file.getError());
			return;
		}
		int[] ctri = xitMCA.file.getAddress();
		putDskAdr(xitMCA.cadAdr, xitMCA.dd, 0, ctri, 3);
		xitMCA = null;
	}

	public void setm() {
		int mode = parms[2];
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			if (((xitErr >> 6) & 077) == 3) {
				mode = xitAct;
			} else {
				haltErr(xitErr);
				return;
			}
		}
		if (xitMCA.file == null) {	// File not open
			handleError(00510); // Write error
			return;
		}
		// TODO: modify mode value
		if (xitMCA.xitMmb != 0) {
			boolean ok = xitMCA.file.setMemb(null, 0, mode);
			if (ok) {
				if (mode != 052) {
					putAdr(xitMCA.apdAdr, xitMCA.file.getItemAdr());
					setupExit(00301);
					return;
				}
			} else {
				// end of index...
				handleError(00203);
				return;
			}
		} else if (!xitMCA.file.setMemb(sys, getStrPtr(parms[1]), parms[2])) {
			handleError(xitMCA.file.getError());
			return;
		}
		if (xitMCA.deliv == 0) {
			putAdr(xitMCA.itm1, xitMCA.file.getItemAdr());
		}
		xitMCA = null;
	}

	public void endm() {
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			// Any return from EXIT is "fatal"?
			// There are some "10 continue" cases...
			haltErr(xitErr);
			return;
		}
		if (xitMCA.file == null) {	// File not open
			handleError(00510); // Write error
			return;
		}
		// TODO: update mode
		if (!xitMCA.file.endMemb()) {
			handleError(xitMCA.file.getError());
			return;
		}
		xitMCA = null;
	}

	public void malter() {
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			// Any return from EXIT is "fatal"?
			// There are some "10 continue" cases...
			haltErr(xitErr);
			return;
		}
		if (xitMCA.file == null) {	// File not open
			handleError(00510); // Write error
			return;
		}
		// parms[2] was already encoded
		if (!xitMCA.file.alterMemb(sys, getStrPtr(parms[1]), parms[2],
				parms[3] > 0 ? sys : null,
				parms[3] > 0 ? getStrPtr(parms[3]) : 0)) {
			handleError(xitMCA.file.getError());
			return;
		}
		xitMCA = null;
	}

	public void msrel() {
		if (xitMCA == null) {
			getMCA(parms[0]);
		} else {
			// Any return from EXIT is "fatal"?
			// There are some "10 continue" cases...
			haltErr(xitErr);
			return;
		}
		if (xitMCA.file == null) {	// File not open
			handleError(00510); // Write error
			return;
		}
		if (!xitMCA.file.release()) {
			handleError(xitMCA.file.getError());
			return;
		}
		xitMCA = null;
	}
}
