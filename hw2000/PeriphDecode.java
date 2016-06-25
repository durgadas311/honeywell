import java.io.*;
import java.util.Arrays;

public class PeriphDecode {

	public static final byte P_MT = 000;	// Mag Tape
	public static final byte P_PP = 001;	// Punch-card/Paper-tape
	public static final byte P_LP = 002;	// Line Printer
	public static final byte P_DK = 004;	// DisK
	public static final byte P_CO = 007;	// COnsole
	public static final byte P_IN = 040;	// Input modifier
	public static final byte P_OUT = 000;	// Output modifier

	public static final byte RWC_1 = 0;
	public static final byte RWC_1p = 1;
	public static final byte RWC_2 = 2;
	public static final byte RWC_3 = 3;

	private Peripheral[] p_odevs;
	private Peripheral[] p_idevs;
	public CharConverter cvt;
	private RWChannel[] p_chans;

	public PeriphDecode() {
		cvt = new CharConverter(new CardPunchOptions());
		p_odevs = new Peripheral[8];
		p_idevs = new Peripheral[8];
		p_chans = new RWChannel[8];
		p_odevs[P_LP] = new P_LinePrinter();
		p_odevs[P_CO] = new P_ConsolePrinter();
	}

	public Peripheral getPerph(byte pa) {
		if ((pa & 040) != 0) { // input
			return p_idevs[pa & 007];
		} else {
			return p_odevs[pa & 007];
		}
	}

	public RWChannel getChannel(byte ca) {
		// fudge, rather than use a complex translation
		ca &= 007;
		RWChannel c;
		// Only create channels as needed
		if ((c = p_chans[ca]) == null) {
			c = p_chans[ca] = new RWChannel();
		}
		return c;
	}

	public void setOutput(byte pa, OutputStream dev) {
		Peripheral p = null;
		if ((p = p_odevs[pa & 007]) != null) {
			p.setOutput(dev);
		}
	}

	public OutputStream getOutput(byte pa) {
		OutputStream o = null;
		Peripheral p = null;
		if ((p = p_odevs[pa & 007]) != null) {
			o = p.getOutput();
		}
		return o;
	}

	public void setInput(byte pa, InputStream dev) {
		Peripheral p = null;
		if ((p = p_idevs[pa & 007]) != null) {
			p.setInput(dev);
		}
	}

	public InputStream getInput(byte pa) {
		InputStream i = null;
		Peripheral p = null;
		if ((p = p_idevs[pa & 007]) != null) {
			i = p.getInput();
		}
		return i;
	}
}
