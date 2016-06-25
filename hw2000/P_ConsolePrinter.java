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
		clc = (byte)(sys.getXtra(0) & 007);
		slc = clc + 8;
		if ((sys.getXtra(1) & 030) == 010) {
			c3 = sys.getXtra(3);
		} else {
			c3 = sys.getXtra(2);
		}
		// C3:
		//	000000: Print, no CR/LF
		//	000001: Print, then issue CR (LF?)
		sys.cr[slc] = sys.AAR;
		sys.cr[clc] = sys.AAR;
		busy = true;
	}

	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		String s = "";
		boolean print = true;
		// Printing stops *before* char with record mark...
		while (print) {
			byte a = sys.readMem(sys.cr[clc]);
			if ((a & 0300)  == 0300) {
				break;
			}
			a &= 077;
			s += sys.pdc.cvt.hwToLP(a);
			sys.cr[clc] = sys.incrAdr(sys.cr[clc], 1);
		}
		if (c3 != 0) {
			s += "\n";
		}
		try {
			dev.write(s.getBytes());
		} catch (Exception ee) {
			// TODO: handle exceptions?
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
