import java.io.*;
import java.util.Arrays;

public class PeriphDecode {

	public static final byte P_MT = 000;	// Mag Tape
	public static final byte P_PP = 001;	// Punch-card/Paper-tape
	public static final byte P_PP2 = 003;	// Second Punch-card/Paper-tape
	public static final byte P_LP = 002;	// Line Printer
	public static final byte P_DK = 004;	// DisK
	public static final byte P_CO = 007;	// COnsole
	public static final byte P_SEC_MSK = 030;	// Sector bits mask
	public static final byte P_SEC_ESC = 010;	// Esc sector
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
		cvt = new CharConverter();
		p_odevs = new Peripheral[8];
		p_idevs = new Peripheral[8];
		p_chans = new RWChannel[8];
		p_odevs[P_LP] = new P_LinePrinter();
		p_odevs[P_CO] = new P_Console();
		p_idevs[P_CO] = p_odevs[P_CO];
		p_odevs[P_MT] = new P_MagneticTape();
		p_idevs[P_MT] = p_odevs[P_MT];
		p_odevs[P_PP] = new P_CardReaderPunch();
		p_idevs[P_PP] = p_odevs[P_PP];
	}

	public void reset() {
		for (int x = 0; x < p_idevs.length; ++x) {
			if (p_idevs[x] != null) {
				p_idevs[x].reset();
			}
		}
		for (int x = 0; x < p_chans.length; ++x) {
			if (p_chans[x] != null) {
				p_chans[x].reset();
				p_chans[x] = null;
			}
		}
	}

	public Peripheral getPeriph(byte pa) {
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

	public static boolean isEsc(byte c2) {
		return ((c2 & P_SEC_MSK) == P_SEC_ESC);
	}
}
