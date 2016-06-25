import java.io.*;

public class P_LinePrinter implements Peripheral {

	OutputStream dev;
	byte c3;
	int clc, slc;
	boolean busy;

	public P_LinePrinter() {
		this.dev = System.out;
		busy = false;
	}

	public void setOutput(OutputStream dev) {
		if (dev == null) {
			this.dev = System.out;
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
		//	00nnnn: Print then advance nnnn lines.
		//	01nnnn: Print then advance nnnn lines unless HOF, etc.
		//	11nnnn: Do not print, advance nnnn lines.
		//	100xxx: Print, advance to channel xxx.
		//	101xxx: Do not print, advance to channel xxx.
		// NOTE: AAR was checked for protection violation in I_PDT.
		// No further checks will be made.
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		sys.cr[clc] = sys.cr[slc];
		busy = true;
	}

	// Must protect against exceptions, and eventually throw them to main thread...
	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		// Cannot depend on any processor state here.
		// The processor may be running a completely different program.
		String s = "";
		boolean print = ((c3 & 040) == 0 || (c3 & 030) == 0);
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
			if ((c3 & 060) != 040) {
				c3 &= 017;
				while (c3 > 0) {
					s += "\n";
					--c3;
				}
			}
			dev.write(s.getBytes());
		} catch (Exception ee) {
			// TODO: handle exceptions? How to pass along EI/II exceptions to CPU?
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
