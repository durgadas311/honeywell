// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.concurrent.Semaphore;

public class LoaderMonitorC implements HW2000Trap {
	Semaphore wait;
	HW2000 sys;

	public LoaderMonitorC(HW2000 sys, String prg, String seg, String rev, int vis) {
		this.sys = sys;
		wait = new Semaphore(0);
		// TODO: fill comm area with zero?
		putStr(65, rev);
		putStr(68, prg);
		putStr(74, seg);
		putInt(113, vis, 6);
		putAdr(139, 130, 3);
		// TODO: set date? others? delineate fields with WM?
	}

	public boolean doTrap() {
		if (sys.SR < 64 || sys.SR >= 1340) {
			return false;
		}
		wait.release();
		// no parameters... ever? Typically, our parent does STOP
		sys.SR = sys.BAR;
		// must halt system immediately... or at least prevent running
		sys.halt = true;
		sys.endWait(); // not really possible, if we got here
		return true;
	}
	public String getName() { return "AACMON"; }
	public void done() {
		// cleanup only
	}
	public void reinit() {}

	private void putAdr(int a, int v, int n) {
		// TODO: use sys.addrMode() ?
		putInt(a, v, n);
	}

	private void putInt(int a, int v, int n) {
		int b = a + n - 1;
		while (b >= a) {
			byte bb = (byte)(v & 077);
			if (b == a) {
				bb |= 0100;
			}
			sys.rawWriteMem(b--, bb);
		}
	}

	private void putStr(int a, String s) {
		for (int x = 0; x < s.length(); ++x) {
			byte bb = sys.cvt().asciiToHw((byte)s.charAt(x));
			if (x == 0) {
				bb |= 0100;
			}
			sys.rawWriteMem(a++, bb);
		}
	}

	public void waitReturn(long timeout) {
		// TODO: implement timeout
		try {
			wait.acquire();
		} catch (Exception ee) {}
	}
}
