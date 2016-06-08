public class HW2000
{
	static final byte M_IM = 0x80;
	static final byte M_WM = 0x40;
	static final byte M_SIGN = 0x30;
	static final byte M_DIGIT = 0x0f;
	static final byte M_CHAR = 0x3f;

	byte[] mem;

	int[] CLC;
	int[] SLC;
	int ATR;
	int CSR;
	int EIR;
	int AAR;
	int BAR;
	int IIR;
	int SR;

	byte IBR;
	byte BRR;
	HW2000CCR CTL;
	double[] AC;

	int op_flags;
	int op_xflags;
	Instruction op_exec;
	byte[] op_xtra;

	private int fsr;
	int iaar;	// needed by branch instructions
	int am_mask;	// populated by CAM instruction
	int am_shift;	// populated by CAM instruction
	int am_na;	// populated by CAM instruction
	int adr_min;	// set by changes in PROTECT, based on IBR/BRR (RVI instr)
	int adr_max;	// set by changes in PROTECT, based on IBR/BRR (RVI instr)

	InstrDecode idc;

	public HW2000() {
		CTL = new HW2000CCR();
		AC = new double[4];
		mem = new byte[524288]; // TODO: mmap file
		idc = new InstrDecode();
		CLC = new int[16];
		SLC = new int[16];
	}

	private boolean hasA() { return ((op_flags & InstrDecode.OP_HAS_A) != 0); }
	private boolean hasB() { return ((op_flags & InstrDecode.OP_HAS_B) != 0); }
	private boolean dupA() { return ((op_flags & InstrDecode.OP_DUP_A) != 0); }
	private boolean hasV() { return ((op_flags & InstrDecode.OP_HAS_V) != 0); }
	private boolean inval() { return ((op_flags & InstrDecode.OP_INVAL) != 0); }
	private boolean priv() { return ((op_flags & InstrDecode.OP_PRIV) != 0); }
	private boolean hadA() { return ((op_xflags & InstrDecode.OP_HAS_A) != 0); }
	private boolean hadB() { return ((op_xflags & InstrDecode.OP_HAS_B) != 0); }
	private boolean hadV() { return ((op_xflags & InstrDecode.OP_HAS_V) != 0); }

	private void setOp(byte op) {
		op_exec = null;
		op_flags = idc.getFlags(op);
		if (inval()) {
			throw new RuntimeException("Invalid OpCode");
		}
		if (priv()) { // and !proceed?
			throw new RuntimeException("OpCode Violation");
		}
		op_exec = idc.getExec(op);
	}

	// 'am' contains adr mode bits, in final position.
	// Typically called with values AIR_AM_2C, AIR_AM_3C, or AIR_AM_4C
	public void setAM(byte am) {
		CTL.setAM(am);
		switch(am & HW2000CCR.AIR_AM) {
		case HW2000CCR.AIR_AM_2C:
			am_mask = 0x0fff;
			am_shift = 12;
			am_na = 2;
			break;
		case HW2000CCR.AIR_AM_4C:
			am_mask = 0x07ffff;
			am_shift = 19;
			am_na = 4;
			break;
		default:
			am_mask = 0x07fff;
			am_shift = 15;
			am_na = 3;
			break;
		}
	}

	public void storeToAAR(int v) {
		int val = (v & am_mask);
		for (int x = 0; x < am_na; ++x) {
			writeChar(AAR, val & 077);
			incrAAR(-1);
			val >>= 6;
		}
	}

	public void incrAAR(int inc) {
		int a = ((AAR & am_mask) + inc) & am_mask;
		AAR = (AAR & ~am_mask) | a;
	}

	public void incrBAR(int inc) {
		int a = (BAR + inc) & am_mask;
		BAR = (BAR & ~am_mask) | a;
	}

	public byte readMem(int adr) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		return mem[adr];
	}

	public byte readChar(int adr) {
		return readMem(adr) & 077;
	}

	public void writeMem(int adr, byte val) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		mem[adr] = val;
	}

	public void writeChar(int adr, byte val) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		mem[adr] = (mem[adr] & 0300) | (val & 077);
	}

	// 
	private int fetchAddr(int ptr) {
		int a = 0;
		for (int n = am_na; n > 0; --n) {
			a = (a << 6) | (readMem(ptr++) & M_CHAR);
		}
		am = (a >> am_shift);
		a &= am_mask;
		if (am == 0) {
			return a;
		}
		if (am_na == 3 && am == 0x07 || am == 0x10) {
			// Indirect...
			return fetchAddr(a);
		}
		// Indexed... determine which index register
		ix = (((am & 0x0f) - 1) * 4);
		if (am > 0x10) {
			// must be 4-char addr mode
			ix += (ibr << 12);	// Y1-Y15
		} else if (am_na == 4) {
			// no further adjustment for X1-X15
		} else {
			// must be 3-char addr mode
			ix += (SR & ~0x07fff);	// X1-X6
		}
		int ax = 0;
		for (int n = 4; n > 0; --n) {
			ax = (ax << 6) | (readMem(ix++) & M_CHAR);
		}
		a = (a + ax) & 0x7ffff;
		return a;
	}

	public void fetchAAR(int limit) {
		if (!hasA() || limit - SR < am_na) {
			return;
		}
		op_xflags |= InstrDecode.OP_HAS_A;
		iaar = AAR;
		AAR = fetchAddr(fsr);
		fsr += am_na;
	}

	public void restoreAAR() {
		if (iaar >= 0) {
			AAR = iaar;
		}
	}

	public void fetchBAR(int limit) {
		if (!hadA() || !hasB() || limit - SR < am_na) {
			if (dupA()) {
				BAR = AAR;
			}
			return;
		}
		op_xflags |= InstrDecode.OP_HAS_B;
		BAR = fetchAddr(fsr);
		fsr += am_na;
	}

	private void fetchXtra(int limit) {
		if (limit - fsr <= 0) {
			return;
		}
		op_xtra = new byte[limit - fsr];
		for (int x = 0; x < op_xtra.length; ++x) {
			op_xtra[x] = readMem(fsr + x);
		}
	}

	private void checkIntr() {
		if (CTL.isEI()) {
			// AIR already saved...
			setAM(HW2000CCR.AIR_AM_3C);
			int t = EIR;
			EIR = SR;
			SR = t;
		} else if (CTL.isII()) {
			int t = IIR;
			IIR = SR;
			SR = t;
		}
	}

	public void fetch() {
		checkIntr(); // might get diverted here...

		// It appears to be common practice to use CW/SW on instructions
		// to turn off/on various pieces of code. So, this routine must
		// allow for "garbage" after an instruction - scan to next word mark
		// and ignore extra bytes. It appears, in most cases, the instruction
		// preceeding will be NOP to avoid confusion about operands.
		// In any case, it is programmer's responsibility to ensure
		// there is no confusion when the instruction is turned off.

		fsr = SR;
		// TODO: how to avoid including garbage in variant array.
		int isr = (fsr + 1) & 0x1ffff;
		while (isr != 0 && (mem[isr] & M_WM) == 0) {
			isr = (isr + 1) & 0x1ffff;
		}
		iaar = -1;
		op_xtra = null;
		// Caller handles exceptions, leave SR at start of instruction
		// (if during fetch/extract). Exceptions during execute
		// terminate instruction and leave SR at next.
		if (isr == 0) {
			// ran off end of memory... need to halt...
			halt = true;
			throw new RuntimeException("ran off end of memory");
		}
		setOp(readMem(fsr++));	// might throw illegal op-code
		fetchAAR(isr);	// might throw exceptions
		fetchBAR(isr);	// might throw exceptions
		// just get all extra characters, let implementations
		// sort it out...
		fetchXtra(isr);
		CTL.clrPROCEED();
		SR = isr;
	}

	public void execute() {
		op_exec.execute(this);
	}

	public void run() {
		while (!halt) {
			try {
				fetch();
				execute();
			} catch (Exception ee) {
				// Loop around, either we're halted or there
				// is a pending interrupt.
			}
		}
	}
}
