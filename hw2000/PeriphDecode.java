// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.util.Arrays;
import java.util.Properties;

public class PeriphDecode {

	public static final byte P_MT = 000;	// Mag Tape
	public static final byte P_PP = 001;	// Punch-card/Paper-tape
	public static final byte P_PP2 = 003;	// Second Punch-card/Paper-tape
	public static final byte P_LP = 002;	// Line Printer
	public static final byte P_DK = 004;	// DisK
	public static final byte P_TI = 006;	// TIme-related: T.O.D. and Interval Timer
	public static final byte P_CO = 007;	// COnsole
	public static final byte P_SEC_MSK = 030;	// Sector bits mask
	public static final byte P_SEC_ESC = 010;	// Esc sector
	public static final byte P_IN = 040;	// Input modifier
	public static final byte P_OUT = 000;	// Output modifier

	public static final byte RWC_1 = 0;
	public static final byte RWC_1p = 1;
	public static final byte RWC_2 = 2;
	public static final byte RWC_3 = 3;

	// Based on H200/H2000 PDT c1 tables.
	// 100 = "null" values for PCB, 101 = illegal values
	private static final byte[] map = new byte[]{
		100,025,026,027,027,027,027,026, 000,001,002,003,004,005,006,007,
		020,021,000,020,024,025,004,024, 020,021,022,023,024,025,026,027,
		006,005,006,007,007,007,007,007, 002,001,002,003,003,003,003,007,
		000,024,000,020,020,020,020,027, 022,021,022,023,023,023,023,100
	};

	private Peripheral[] p_odevs;
	private Peripheral[] p_idevs;
	public CharConverter cvt;
	private RWChannel[] p_chans;
	private RWChannel nullRWC;
	private boolean mapRWC;

	public PeriphDecode(Properties props, HW2000 hw) {
		cvt = new CharConverter();
		p_odevs = new Peripheral[8];
		p_idevs = new Peripheral[8];
		p_chans = new RWChannel[16];
		p_odevs[P_LP] = new P_LinePrinter(P_LP, hw);
		p_odevs[P_CO] = new P_Console(props, P_CO, hw);
		p_idevs[P_CO] = p_odevs[P_CO];
		p_odevs[P_MT] = new P_MagneticTape(P_MT, hw);
		p_idevs[P_MT] = p_odevs[P_MT];
		p_odevs[P_PP] = new P_CardReaderPunch(cvt, P_PP, hw);
		p_idevs[P_PP] = p_odevs[P_PP];
		p_odevs[P_DK] = new P_Disk(P_DK, hw);
		p_idevs[P_DK] = p_odevs[P_DK];
		p_odevs[P_TI] = new P_Time(P_TI, hw);
		p_idevs[P_TI] = p_odevs[P_TI];
		nullRWC = new RWChannel((byte)-1); // no I/O, never busy...
		String s = props.getProperty("rwc");
		mapRWC = (s != null && s.equals("map"));
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

	private byte getRWCdir(byte ca) {
		// fudge, rather than use a complex translation
		if (ca == 0 || ca == 077) {
			return 100;
		}
		return ca;
	}

	private byte getRWCmap(byte ca) {
		return map[ca];
	}

	public RWChannel getChannel(byte ca) {
		byte ci;
		if (mapRWC) {
			ci = getRWCmap(ca);
		} else {
			ci = getRWCdir(ca);
		}
		// 100 means "no RWC",
		// but need to have an object...
		if (ci > 077) {
			return nullRWC;
		}
		// squeeze 020 and 007 bits together
		byte cx = (byte)(ci & 027);
		cx = (byte)((cx + (cx & 007)) >> 1);
		RWChannel c;
		// Only create channels as needed
		if ((c = p_chans[cx]) == null) {
			c = p_chans[cx] = new RWChannel(ci);
		}
		return c;
	}

	public static boolean isEsc(byte c2) {
		return ((c2 & P_SEC_MSK) == P_SEC_ESC);
	}
}
