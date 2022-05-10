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

	// id "-1" means null RWC...
	public RWChannel(byte id) {
		periph = null;
		if (id < 0) {
			clc = slc = -1;
			return;
		}
		// These can never be < 0
		clc = (byte)(id & 027);
		slc = (byte)(clc + 010);
	}

	// Called from JAVA Event thread (HW2000FrontPanel)
	public void reset() {
		if (thr != null && thr.isAlive() ) {
			// This doesn't really help anything
			//thr.interrupt();
			// peripherals need to be reset(),
			// should happen by caller.
		}
		synchronized(this) {
			if (periph != null) periph.cancel();
		}
	}

	public boolean busy() {
		return (clc >= 0 && periph != null);
	}

	private void loadCtl() {
		cn = sys.numXtra();
		int x = 1;
		if (PeriphDecode.isEsc(sys.getXtra(1))) {
			++x;
			--cn;
		}
		// These return 0 if not exist
		c2 = sys.getXtra(x++);
		c3 = sys.getXtra(x++);
		c4 = sys.getXtra(x++);
		c5 = sys.getXtra(x++);
		c6 = sys.getXtra(x++);
		c7 = sys.getXtra(x++);
	}

	public boolean isInput() {
		return ((c2 & 040) == PeriphDecode.P_IN);
	}

	// These are only called from peripheral during I/O, and
	// I/O is neever started for null RWC, so we don't need
	// to check "clc < 0" here.
	public void writeMem(byte c) {
		sys.rawWriteMem(sys.cr[clc], c);
	}

	// Returns puntuation bits.
	public byte writeChar(byte c) {
		if (sys.bootstrap) {
			// Special-case for bootstrap - clear punctuation
			sys.rawWriteMem(sys.cr[clc], (byte)(c & 077));
			return (byte)0;
		} else {
			return sys.rawWriteChar(sys.cr[clc], (byte)(c & 077));
		}
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

	// Called from CPU execution thread
	public void io(HW2000 hw, Peripheral p) {
		if (periph != null) {
			// should never happen - already checked.
			return; // throw something?
		}
		sys = hw;
		if (clc < 0) {
			// invalid! never do I/O on null RWC...
			// TODO: throw exception?
			return;
		}
		synchronized(this) { periph = p; }
		loadCtl();
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
			loadCtl();
			if (p.ctl(this)) {
				hw.BAR = hw.SR;
				hw.SR = hw.AAR;
			}
		}
		if (sr != hw.SR && sr == lastSR && clc >= 0) {
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
		synchronized(this) { periph = null; }
		sys.endWait();
	}
}
