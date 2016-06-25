import java.io.*;

public class P_ConsoleKeyboard implements Peripheral {

	InputStream dev;
	byte c3;
	int clc, slc;
	boolean busy;

	public P_ConsoleKeyboard() {
		this.dev = System.in;
		busy = false;
	}

	public void setOutput(OutputStream dev) {
	}

	public OutputStream getOutput() {
		return null;
	}

	public void setInput(InputStream dev) {
		if (dev == null) {
			this.dev = System.in;
		} else {
			this.dev = dev;
		}
	}

	public InputStream getInput() {
		return this.dev;
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
		//	000000: no CR/LF
		//	000001: CR (LF?)
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		sys.cr[clc] = sys.cr[slc];
		busy = true;
	}

	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		int a = -1;
		byte b;
		try {
			b = (byte)(sys.rawReadMem(sys.cr[clc]) & 0300);
			do {
				a = dev.read();
				if (a < 0) {
					break;
				}
				a = Character.toUpperCase((char)a);
				// TODO: what effect does c3 have?
				if (a == '\n') {
					break;
				}
				int ix = CharConverter.hwAsciiSup.indexOf((char)a);
				if (ix >= 0) {
					a = CharConverter.hwAsciiRep.charAt(ix);
				}
				byte c = sys.pdc.cvt.asciiToHw((byte)a);
				// Must not disturb punctuation
				c |= b;
				sys.rawWriteMem(sys.cr[clc], c);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
				b = (byte)(sys.rawReadMem(sys.cr[clc]) & 0300);
			} while (b != 0300); // always end at record mark, right?
		} catch (Exception ee) {
			// TODO: pass along EI/II exceptions
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
