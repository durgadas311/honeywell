import java.util.Arrays;

public class PeriphDecode {

	public static final byte P_MT = 000;	// Mag Tape
	public static final byte P_PP = 001;	// Punch-card/Paper-tape
	public static final byte P_LP = 002;	// Line Printer
	public static final byte P_DK = 004;	// DisK
	public static final byte P_CO = 007;	// COnsole

	private Peripheral[] p_odevs;
	private Peripheral[] p_idevs;
	public CharConverter cvt;

	public PeriphDecode() {
		cvt = new CharConverter(new CardPunchOptions());
		p_odevs = new Peripheral[8];
		p_idevs = new Peripheral[8];
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
}
