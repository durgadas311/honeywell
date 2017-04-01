// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class RWChannel implements Runnable {
	Peripheral periph;
	Thread thr;
	public HW2000 sys;
	public byte clc;
	public byte slc;
	public byte c2;
	public byte c3;
	public byte c4;
	public byte c5;
	public byte c6;
	public byte c7;
	public int cn;

	public RWChannel(byte id) {
		periph = null;
		clc = (byte)(id & 027);
		slc = (byte)(clc + 010);
	}

	public void reset() {
		if (thr != null && thr.isAlive()) {
			// This doesn't really help anything
			//thr.interrupt();
		}
	}

	public boolean busy() {
		return (periph != null);
	}

	private void loadCtl(HW2000 hw) {
		cn = hw.numXtra();
		int x = 1;
		if (PeriphDecode.isEsc(hw.getXtra(1))) {
			++x;
			--cn;
		}
		// These return 0 if not exist
		c2 = hw.getXtra(x++);
		c3 = hw.getXtra(x++);
		c4 = hw.getXtra(x++);
		c5 = hw.getXtra(x++);
		c6 = hw.getXtra(x++);
		c7 = hw.getXtra(x++);
	}

	public boolean isInput() {
		return ((c2 & 040) == PeriphDecode.P_IN);
	}

	public void writeMem(byte c) {
		sys.rawWriteMem(sys.cr[clc], c);
	}

	public void writeChar(byte c) {
		sys.rawWriteChar(sys.cr[clc], (byte)(c & 077));
	}

	public byte readMem() {
		return sys.rawReadMem(sys.cr[clc]);
	}

	public byte readChar() {
		return (byte)(sys.rawReadMem(sys.cr[clc]) & 077);
	}

	public int getCLC() {
		return sys.cr[clc];
	}

	public void startCLC() {
		sys.cr[clc] = sys.cr[slc];
	}

	public boolean incrCLC() {
		sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
		return (sys.cr[clc] == 0);
	}

	public void io(HW2000 hw, Peripheral p) {
		if (periph != null) {
			// should never happen - already checked.
			return; // throw something?
		}
		sys = hw;
		periph = p;
		loadCtl(hw);
		sys.cr[slc] = sys.validAdr(sys.AAR); // translate to physical address
		lastSR = -1;
		sys.setupWait();
		p.io(this);
		thr = new Thread(this);
		thr.start();
	}

	int lastSR;
	int count = 0;

	public void ctl(HW2000 hw, Peripheral p) {
		int sr = hw.SR;
		if (periph != null) {
			// is this too draconian?
			// does only peripheral busy matter?
			hw.BAR = hw.SR;
			hw.SR = hw.AAR;
		} else if (p != null) {
			sys = hw;
			loadCtl(hw);
			if (p.ctl(this)) {
				hw.BAR = hw.SR;
				hw.SR = hw.AAR;
			}
		}
		if (sr != hw.SR && sr == lastSR) {
			if (++count >= 10000) {
				hw.waitIO();
				count = 0;
			}
		} else {
			count = 0;
		}
		lastSR = sr;
	}

	public void run() {
		periph.run(this);
		periph = null;
		sys.endWait();
	}
}
