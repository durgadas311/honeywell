import java.io.*;

public class P_LinePrinter implements Peripheral {

	private OutputStream dev;

	public P_LinePrinter() {
		this.dev = System.out;
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
		byte c3;
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

		String s = "";
		boolean print = ((c3 & 040) == 0 || (c3 & 030) == 0);
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
		if ((c3 & 060) != 040) {
			c3 &= 017;
			while (c3 > 0) {
				s += "\n";
				--c3;
			}
		}
		try {
			dev.write(s.getBytes());
		} catch (Exception ee) {
			// TODO: handle exceptions?
		}
	}

	public void ctl(HW2000 sys) {
	}
}
