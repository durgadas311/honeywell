import java.io.*;

public class P_ConsolePrinter implements Peripheral {

	OutputStream dev;
	byte c3;
	int clc, slc;
	boolean busy;

	public P_ConsolePrinter() {
		this.dev = System.err;
		busy = false;
	}

	public void setOutput(OutputStream dev) {
		if (dev == null) {
			this.dev = System.err;
		} else {
			this.dev = dev;
		}
	}

	public OutputStream getOutput() {
		return this.dev;
	}

	public void setInput(InputStream dev) {
	}

	public InputStream getInput() {
		return null;
	}

	public void io(HW2000 sys) {
		clc = (byte)(sys.getXtra(0) & 027);
		slc = clc + 010;
		if ((sys.getXtra(1) & 030) == 010) {
			c3 = sys.getXtra(3);
		} else {
			c3 = sys.getXtra(2);
		}
		// C3:
		//	000000: Print, no CR/LF
		//	000001: Print, then issue CR (LF?)
		// NOTE: AAR was checked for protection violation in I_PDT.
		// No further checks will be made.
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		busy = true;
	}

	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		sys.cr[clc] = sys.cr[slc];
		String s = "";
		boolean print = true;
		// Printing stops *before* char with record mark...
		try {
			while (print) {
				byte a = sys.rawReadMem(sys.cr[clc]);
				if ((a & 0300)  == 0300) {
					break;
				}
				a &= 077;
				s += sys.pdc.cvt.hwToLP(a);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
			}
			if (c3 != 0) {
				s += "\n";
			}
			dev.write(s.getBytes());
		} catch (Exception ee) {
			// TODO: handle exceptions? pass along?
		}
		busy = false;
	}

	public boolean busy() {
		return busy;
	}

	public void ctl(HW2000 sys) {
		if (busy) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
			return;
		}
		// TODO: apply control chars
	}
}
