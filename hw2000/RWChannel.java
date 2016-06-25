public class RWChannel implements Runnable {
	Peripheral periph;
	HW2000 sys;

	public RWChannel() {
		periph = null;
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
		p.io(sys);
		Thread t = new Thread(this);
		t.start();
	}

	public void ctl(HW2000 hw, Peripheral p) {
		if (periph != null) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
			return;
		}
		if (p != null) {
			p.ctl(hw);
		}
	}

	public void run() {
		periph.run(sys);
		periph = null;
	}
}
