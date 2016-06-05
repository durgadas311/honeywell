public class HW2000
{
	static final byte M_IM = 0x80;
	static final byte M_WM = 0x40;
	static final byte M_SIGN = 0x30;
	static final byte M_DIGIT = 0x0f;
	static final byte M_CHAR = 0x3f;

	byte[] mem;

	int AAR;
	int BAR;
	int SR;
	int IIR;
	int EIR;
	int CSR;
	byte IBR;
	byte BRR;
	HW2000CCR CTL;
	double[] AC;

	int op_flags;
	Instruction op_exec;
	byte[] op_xtra;
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
	}

	private boolean hasA() { return ((op_flags & InstrDecode.OP_HAS_A) != 0); }
	private boolean hasB() { return ((op_flags & InstrDecode.OP_HAS_B) != 0); }
	private boolean dupA() { return ((op_flags & InstrDecode.OP_DUP_A) != 0); }
	private boolean hasV() { return ((op_flags & InstrDecode.OP_HAS_V) != 0); }
	private boolean inval() { return ((op_flags & InstrDecode.OP_INVAL) != 0); }
	private boolean priv() { return ((op_flags & InstrDecode.OP_PRIV) != 0); }

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

	private int incrAAR(int inc) {
		int a = ((AAR & am_mask) + inc) & am_mask;
		AAR = (AAR & ~am_mask) | a;
	}

	private int incrBAR(int inc) {
		int a = (BAR + inc) & am_mask;
		BAR = (BAR & ~am_mask) | a;
	}

	private byte readMem(int adr) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		return mem[adr];
	}

	private void writeMem(int adr, byte val) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		mem[adr] = val;
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
		iaar = AAR;
		AAR = fetchAddr(SR);
		SR += am_na;
	}

	public void fetchBAR(int limit) {
		if (!hasB() || limit - SR < am_na) {
			if (dupA()) {
				BAR = AAR;
			}
			return;
		}
		BAR = fetchAddr(SR);
		SR += am_na;
	}

	public void fetch() {
		int isr = (SR + 1) & 0x1ffff;
		while (isr != 0 && (mem[isr] & M_WM) == 0) {
			isr = (isr + 1) & 0x1ffff;
		}
		if (isr == 0) {
			// ran off end of memory...
			throw new RuntimeException("Ran off end of memory");
		}
		iaar = -1;
		op_xtra = null;
		setOp(readMem(SR++));	// might throw illegal op-code
		fetchAAR(isr);	// might throw exceptions
		fetchBAR(isr);	// might throw exceptions
		if (hasV()) {
			op_xtra = new byte[isr - SR];
			for (int x = 0; x < op_xtra.length; ++x) {
				op_xtra[x] = readMem(SR + x);
			}
		}
		SR = isr;
	}

	public void execute() {
		op_exec.execute(this);
	}

	public void run() {
		while (!isHalted()) {
			fetch();
			execute();
		}
	}
}
