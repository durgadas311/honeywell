import java.io.*;

public class P_ConsolePrinter implements Peripheral {

	OutputStream dev;

	public P_ConsolePrinter() {
		this.dev = System.err;
	}

	public void setOutput(OutputStream dev) {
		if (dev == null) {
			this.dev = System.err;
		} else {
			this.dev = dev;
		}
	}

	public void setInput(InputStream dev) {
	}

	public void io(HW2000 sys) {
		byte c3;
		if ((sys.getXtra(1) & 030) == 010) {
			c3 = sys.getXtra(3);
		} else {
			c3 = sys.getXtra(2);
		}
		// C3:
		//	000000: Print, no CR/LF
		//	000001: Print, then issue CR (LF?)

		String s = "";
		boolean print = true;
		// Printing stops *before* char with record mark...
		while (print) {
			byte a = sys.readMem(sys.AAR);
			if ((a & 0300)  == 0300) {
				break;
			}
			a &= 077;
			s += sys.pdc.cvt.hwToLP(a);
			sys.incrAAR(1);
		}
		if (c3 != 0) {
			s += "\n";
		}
		System.err.format("%s", s);
	}

	public void ctl(HW2000 sys) {
	}
}
