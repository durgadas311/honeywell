import java.util.Arrays;

public class HW2000CCR {

	public static final byte VR = (byte)0;
	public static final byte AIR = (byte)1;
	public static final byte XIR = (byte)2;
	public static final byte IOR = (byte)3;
	public static final byte PIR = (byte)4;
	public static final byte EIR = (byte)5;
	private static final byte IIR = (byte)6; // treated as EIR

	public static final byte VR_BCT = (byte)0200;
	public static final byte VR_VR = (byte)0077;
	private static final byte VR_CLEAR = (byte)0000;

	public static final byte AIR_TRAP = (byte)0200;
	public static final byte AIR_AM = (byte)0060;
	public static final byte AIR_AM_2C = (byte)0020;
	public static final byte AIR_AM_3C = (byte)0000; // also default (0040)
	public static final byte AIR_AM_4C = (byte)0060;
	public static final byte AIR_OVR = (byte)0010;
	public static final byte AIR_ZB = (byte)0004;
	public static final byte AIR_LE = (byte)0002;
	public static final byte AIR_EQ = (byte)0001;
	private static final byte AIR_CLEAR = (byte)0017;

	public static final byte XIR_TRAP = AIR_TRAP;
	public static final byte XIR_AM = AIR_AM;
	public static final byte XIR_OVR = AIR_OVR;
	public static final byte XIR_ZB = AIR_ZB;
	public static final byte XIR_LE = AIR_LE;
	public static final byte XIR_EQ = AIR_EQ;
	private static final byte XIR_CLEAR = AIR_CLEAR;

	public static final byte IOR_XIO = (byte)0200;
	public static final byte IOR_MPO = (byte)0040;
	public static final byte IOR_DVC = (byte)0020;
	public static final byte IOR_EXO = (byte)0010;
	public static final byte IOR_S1_IM = (byte)0004;
	public static final byte IOR_S2_IM = (byte)0002;
	public static final byte IOR_S3_IM = (byte)0001;
	private static final byte IOR_CLEAR = (byte)0070;

	public static final byte PIR_PROTECT = (byte)0040;
	public static final byte PIR_TIMOUT = (byte)0020;
	public static final byte PIR_S_MODE = (byte)0010;
	public static final byte PIR_PROCEED = (byte)0004;
	public static final byte PIR_RELOC = (byte)0002;
	public static final byte PIR_II = (byte)0001;
	private static final byte PIR_CLEAR = (byte)0064;

	public static final byte EIR_CLOCK = (byte)0200;
	public static final byte EIR_ADRVIO = (byte)0040;
	public static final byte EIR_MC = (byte)0020;
	public static final byte EIR_CONS = (byte)0010;
	public static final byte EIR_PC = (byte)0004;
	public static final byte EIR_EI = (byte)0002;
	public static final byte EIR_II = (byte)0001;
	private static final byte EIR_CLEAR = (byte)0270;

	public static final byte IIR_FPE = (byte)0200;
	public static final byte IIR_ADRVIO = (byte)0040;
	public static final byte IIR_OPVIO = (byte)0020;
	public static final byte IIR_TIMOUT = (byte)0010;
	private static final byte IIR_CLEAR = (byte)0270;

	private byte[] ccr;
	private byte[] clr;
	private boolean eIntr;
	private boolean iIntr;
	private byte varLIB;

	public HW2000CCR() {
		ccr = new byte[7];
		Arrays.fill(ccr, (byte)0);
		// TODO: set defaults
		clr = new byte[7];
		clr[VR] = VR_CLEAR;
		clr[AIR] = AIR_CLEAR;
		clr[XIR] = XIR_CLEAR;
		clr[IOR] = IOR_CLEAR;
		clr[PIR] = PIR_CLEAR;
		clr[EIR] = EIR_CLEAR;
		clr[IIR] = IIR_CLEAR;
		varLIB = 0;
		eIntr = false;
		iIntr = false;
	}

	public boolean isEQ() { return ((ccr[AIR] & AIR_EQ) != 0); }
	public boolean isLE() { return ((ccr[AIR] & AIR_LE) != 0); }
	public boolean isZB() { return ((ccr[AIR] & AIR_ZB) != 0); }

	// CAUTION: these are destructive
	public boolean isOVR() {
		boolean r = ((ccr[AIR] & AIR_OVR) != 0);
		ccr[AIR] &= ~AIR_OVR;
		return r;
	}
	public boolean isDVC() {
		boolean r = ((ccr[IOR] & IOR_DVC) != 0);
		ccr[IOR] &= ~IOR_DVC;
		return r;
	}
	public boolean isMPO() {
		boolean r = ((ccr[IOR] & IOR_MPO) != 0);
		ccr[IOR] &= ~IOR_MPO;
		return r;
	}
	public boolean isEXO() {
		boolean r = ((ccr[IOR] & IOR_EXO) != 0);
		ccr[IOR] &= ~IOR_EXO;
		return r;
	}

	public boolean isPROTECT() { return ((ccr[PIR] & PIR_PROTECT) != 0); }
	public boolean isTIMOUT() { return ((ccr[PIR] & PIR_TIMOUT) != 0); }
	public boolean isPROCEED() { return ((ccr[PIR] & PIR_PROCEED) != 0); }
	public boolean isRELOC() { return ((ccr[PIR] & PIR_RELOC) != 0); }
	public boolean isS_MODE() { return ((ccr[PIR] & PIR_S_MODE) != 0); }
	public boolean privBCT() { return ((ccr[VR] & VR_BCT) != 0); }
	public boolean allowLCR() { return ((varLIB & 004) != 0); }

	public void clrPROCEED() { ccr[PIR] &= ~PIR_PROCEED; }

	// 'am' contains adr mode bits, in final position.
	// Typically called with values AIR_AM_2C, AIR_AM_3C, or AIR_AM_4C
	public void setAM(byte am) {
		ccr[AIR] = (byte)((ccr[AIR] & ~AIR_AM) | (am & AIR_AM));
	}
	public byte getAM() {
		return (byte)(ccr[AIR] & AIR_AM);
	}

	public void setLIB(byte var) { varLIB = var; }

	public void setS_MODE(boolean sm) {
		if (sm) {
			ccr[PIR] |= PIR_S_MODE;
		} else {
			ccr[PIR] &= ~PIR_S_MODE;
		}
	}

	public void setTRAP(boolean tr) {
		if (tr) {
			ccr[AIR] |= AIR_TRAP;
		} else {
			ccr[AIR] &= ~AIR_TRAP;
		}
	}

	public void setCompare(boolean lt, boolean eq) {
		if (lt || eq) {
			ccr[AIR] |= AIR_LE;
		} else {
			ccr[AIR] &= ~AIR_LE;
		}
		if (eq) {
			ccr[AIR] |= AIR_EQ;
		} else {
			ccr[AIR] &= ~AIR_EQ;
		}
	}

	public void setZB(boolean tr) {
		if (tr) {
			ccr[AIR] |= AIR_ZB;
		} else {
			ccr[AIR] &= ~AIR_ZB;
		}
	}

	public void setOVR(boolean tr) {
		if (tr) {
			ccr[AIR] |= AIR_OVR;
		} else {
			ccr[AIR] &= ~AIR_OVR;
		}
	}

	public void setDVC(boolean tr) {
		if (tr) {
			ccr[IOR] |= IOR_DVC;
		} else {
			ccr[IOR] &= ~IOR_DVC;
		}
	}

	public void setMPO(boolean tr) {
		if (tr) {
			ccr[IOR] |= IOR_MPO;
		} else {
			ccr[IOR] &= ~IOR_MPO;
		}
	}

	public void setEXO(boolean tr) {
		if (tr) {
			ccr[IOR] |= IOR_EXO;
		} else {
			ccr[IOR] &= ~IOR_EXO;
		}
	}

	public boolean inStdMode() {
		return ((ccr[EIR] & (EIR_EI | EIR_II)) == 0);
	}

	public boolean inEI() {
		return ((ccr[EIR] & EIR_EI) != 0);
	}

	public boolean inII() {
		return ((ccr[EIR] & (EIR_EI | EIR_II)) == EIR_II);
	}

	public void setEI(byte typ) {
		eIntr = true;
		ccr[EIR] |= typ;
	}

	// Returns EIR_EI, EIR_II, or 0
	public byte clearIntr() {
		byte r = 0;
		if ((ccr[EIR] & EIR_EI) != 0) {
			ccr[EIR] &= ~EIR_EI;
			ccr[AIR] = ccr[XIR];
			r = EIR_EI;
		} else if ((ccr[EIR] & EIR_II) != 0) {
			ccr[EIR] &= ~EIR_II;
			r = EIR_II;
		}
		return r;
	}

	public void setII(byte typ) {
		iIntr = true;
		ccr[IIR] |= typ;
	}

	public boolean isEI() {
		if (eIntr && (ccr[EIR] & EIR_EI) == 0) {
			eIntr = false;
			ccr[EIR] |= EIR_EI;
			ccr[XIR] = ccr[AIR];
			// Seems like these should be done, but no doc.
			//ccr[AIR] &= ~AIR_TRAP;
			//ccr[PIR] &= ~PIR_S_MODE;
			return true;
		}
		return false;
	}

	public boolean isII() {
		if (iIntr && inStdMode()) {
			iIntr = false;
			ccr[EIR] |= EIR_II;
			return true;
		}
		return false;
	}

	public void doRNM() {
		if ((ccr[EIR] & EIR_EI) != 0) {
			ccr[EIR] &= ~EIR_EI;
			ccr[AIR] = ccr[XIR];
			return;
		} else if ((ccr[EIR] & EIR_II) != 0) {
			ccr[EIR] &= ~EIR_II;
			return;
		}
	}

	// This is for SVI only - has side-effects
	public byte getCR(int x) {
		if (x < VR || x > EIR) {
			// throw?
			return 0;
		}
		byte v = 0;
		if (x == EIR) {
			if ((ccr[EIR] & EIR_EI) == 0) {
				if ((ccr[EIR] & EIR_II) == 0) {
					return 0;
				}
				x = IIR;
				ccr[x] |= EIR_II;
			}
		}
		v = ccr[x];
		ccr[x] = (byte)(ccr[x] & ~clr[x]);
		return v;
	}

	public void putCR(int x, byte v) {
		if (x < VR || x > IIR) {
			// throw?
			return;
		}
		// TODO: need mask?
		ccr[x] = v;
	}

	public void setV(byte v) {
		ccr[VR] = (byte)((ccr[VR] & 0200) | (v & 0077));
	}

	public byte getV() {
		return (byte)(ccr[VR] & 0077);
	}
}
