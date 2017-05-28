// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;
import java.util.Vector;

public class MOD1MSIORunTime implements HW2000Trap {
	static final String name = "MOD1MSIO";
	private int base = 0;
	private int[] parms;
	private int nparms;
	private int exitSR;	// original action call return

	private HW2000 sys;

	public MOD1MSIORunTime(HW2000 sys) {
		this.sys = sys;
		sys.SR += name.length();
		base = getAdr();
		parms = new int[6];
	}

	public String getName() { return name; }
	static public String name() { return name; }
	static public boolean check(HW2000 sys) {
		if ((sys.rawReadMem(sys.SR) & 0100) == 0 ||
			(sys.rawReadMem(sys.SR + name.length()) & 0100) == 0) {
			return false;
		}
		String s = "";
		for (int a = 0; a < name.length(); ++a) {
			s += sys.pdc.cvt.hwToLP((byte)(sys.rawReadMem(sys.SR + a) & 077));
		}
		return name.equals(s);
	}

	public boolean doTrap() {
		if (sys.SR < base || sys.SR > base + 1) {
			return false;
		}
		int op;
		nparms = 0;
		if (sys.SR == base + 1) {
			op = 0; // some unused value
		} else {
			// Need parameters to decode call
			sys.SR = sys.BAR;
			// RM single char is end, rest are addresses.
			while ((sys.rawReadMem(sys.SR) & 0300) != 0300) {
				if (nparms >= parms.length) {
					// halt? panic?
					sys.halt = true;
					sys.SR = sys.BAR; // guess
					return true;
				}
				parms[nparms++] = getAdr();
			}
			op = sys.rawReadMem(sys.SR++);
			exitSR = sys.SR;
		}
		switch (op) {
		case 0: exitReturn(); break; // return from EXITs
		case 4:	msopen(); break; // MSOPEN
		case 5:	msclos(); break; // MSCLOS
		case 6:	msget(); break; // MSGET
		case 7:	msrep(); break; // MSREP
		case 8:	msput(); break; // MSPUT
		default:
			// our best guess... is?
			exit();
			break;
		}
		return true;
	}

	public void done() {
	}

	private void exit() {
		// System.err.format("exit %07o\n", sys.SR);
		sys.removeTrap(this);
	}

	private void exitReturn() {
		// TODO: restore context, recover if possible...
		//sys.halt = true;
		sys.SR = exitSR;
	}

	private void callExit(int rtn) {
		sys.BAR = base + 1;
		sys.SR = rtn;
	}

	// Doesn't check IM...
	private int getAdr() {
		int a = fetchAdr(sys.SR);
		sys.SR += sys.am_na;
		return a;
	}

	// This is a clone of HW2000.fetchAddr() - keep in sync!
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

	// Search backward until WM... TODO: is that right?
	private String getStr(int a) {
		String s = "";
		int b;
		for (b = a; b >= 0; --b) {
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
		while (b <= a) {
			s += sys.pdc.cvt.hwToLP((byte)(sys.rawReadMem(b++) & 077));
		}
		return s;
	}

	private void putStr(int a, String s) {
		int b;
		for (b = a; b >= 0; --b) {
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
		int x = 0;
		while (b <= a) {
			byte bb = (byte)015; // blank space
			if (x < s.length()) {
				bb = sys.pdc.cvt.asciiToHw((byte)s.charAt(x++));
			}
			sys.rawWriteChar(b, bb);
		}
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
	private void putInt(int a, int v) {
		int b;
		// TODO: re-use routines from instructions?
		for (b = a; b >= 0; --b) {
			sys.rawWriteChar(b, (byte)(v & 077));
			v >>= 6;
			if ((sys.rawReadMem(b) & 0100) != 0) {
				break;
			}
		}
	}

	private void msopen() {}
	private void msclos() {}
	private void msget() {}
	private void msrep() {}
	private void msput() {}
}
