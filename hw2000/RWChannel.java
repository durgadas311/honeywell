public class RWChannel implements Runnable {
	Peripheral periph;
	HW2000 sys;
	Thread thr;

	public RWChannel() {
		periph = null;
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

	public void io(HW2000 hw, Peripheral p) {
		if (periph != null) {
			// should never happen - already checked.
			return; // throw something?
		}
		sys = hw;
		periph = p;
		lastSR = -1;
		sys.setupWait();
		p.io(sys);
		thr = new Thread(this);
		thr.start();
	}

	int lastSR;
	int count = 0;

	public void ctl(HW2000 hw, Peripheral p) {
		int sr = sys.SR;
		if (periph != null) {
			// is this too draconian?
			// does only peripheral busy matter?
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		} else if (p != null) {
			p.ctl(hw);
		}
		if (sr != sys.SR && sr == lastSR) {
			if (++count >= 10000) {
				sys.waitIO();
				count = 0;
			}
		} else {
			count = 0;
		}
		lastSR = sr;
	}

	public void run() {
		periph.run(sys);
		periph = null;
		sys.endWait();
	}
}
