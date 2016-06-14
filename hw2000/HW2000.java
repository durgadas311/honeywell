import java.util.Arrays;
import java.io.*;

public class HW2000
{
	public static final byte M_IM = (byte)0x80;
	public static final byte M_WM = 0x40;
	public static final byte M_SIGN = 0x30;
	public static final byte M_DIGIT = 0x0f;
	public static final byte M_CHAR = 0x3f;

	public static final byte LOR = 007;

	byte[] mem;

	int[] CLC;
	int[] SLC;
	int ATR;
	public int CSR;
	public int EIR;
	public int AAR;
	public int BAR;
	public int IIR;
	public int SR;

	public byte IBR;
	public byte BRR;
	public HW2000CCR CTL;
	public double[] AC;

	public int oSR;
	int op_flags;
	int op_xflags;
	private byte[] op_xtra;
	private int op_xtra_siz;
	private int op_xtra_num;
	Instruction op_exec;
	public boolean halt;

	private int fsr;
	public int iaar;	// needed by branch instructions
	public int am_mask;	// populated by CAM instruction
	public int am_shift;	// populated by CAM instruction
	public int am_na;	// populated by CAM instruction
	public int adr_min;	// set by changes in PROTECT, based on IBR/BRR (RVI instr)
	public int adr_max;	// set by changes in PROTECT, based on IBR/BRR (RVI instr)

	InstrDecode idc;
	PeriphDecode pdc;

	public HW2000() {
		CTL = new HW2000CCR();
		AC = new double[8];
		mem = new byte[524288]; // TODO: mmap file
		Arrays.fill(mem, (byte)0);
		idc = new InstrDecode();
		pdc = new PeriphDecode();
		CLC = new int[16];
		SLC = new int[16];
		adr_min = 0;
		adr_max = 0x80000;
		halt = false;
		setAM((byte)000);
		SR = 0;
		AAR = 0;
		BAR = 0;
		op_xtra_siz = 8;
		op_xtra = new byte[op_xtra_siz];
	}

	public boolean hasA() { return ((op_flags & InstrDecode.OP_HAS_A) != 0); }
	public boolean reqA() { return ((op_flags & InstrDecode.OP_REQ_A) != 0); }
	public boolean hasB() { return ((op_flags & InstrDecode.OP_HAS_B) != 0); }
	public boolean reqB() { return ((op_flags & InstrDecode.OP_REQ_B) != 0); }
	public boolean dupA() { return ((op_flags & InstrDecode.OP_DUP_A) != 0); }
	public boolean hasV() { return ((op_flags & InstrDecode.OP_HAS_V) != 0); }
	public boolean reqV() { return ((op_flags & InstrDecode.OP_REQ_V) != 0); }
	public boolean inval() { return ((op_flags & InstrDecode.OP_INVAL) != 0); }
	public boolean priv() { return ((op_flags & InstrDecode.OP_PRIV) != 0); }
	public boolean hadA() { return ((op_xflags & InstrDecode.OP_HAS_A) != 0); }
	public boolean hadB() { return ((op_xflags & InstrDecode.OP_HAS_B) != 0); }
	public boolean hadV() { return ((op_xflags & InstrDecode.OP_HAS_V) != 0); }

	public Peripheral getPeriph(byte op) {
		Peripheral p = pdc.getPerph(op);
		if (p == null) {
			throw new RuntimeException("Invalid Peripheral " + op);
		}
		return p;
	}

	private void setOp(byte op) {
		op_exec = null;
		op_flags = idc.getFlags(op);
		if (inval()) {
			throw new RuntimeException("Invalid OpCode");
		}
		if (priv() && CTL.inStdMode() && CTL.isPROTECT() &&
				!CTL.isPROCEED()) {
			throw new RuntimeException("OpCode Violation " + op);
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
		int aar = AAR;
		for (int x = 0; x < am_na; ++x) {
			writeChar(aar, (byte)(val & 077));
			aar = incrAdr(aar, -1);
			val >>= 6;
		}
	}

	public int loadFromAAR() {
		int val = 0;
		int aar = incrAdr(AAR, -am_na);
		for (int x = 0; x < am_na; ++x) {
			aar = incrAdr(aar, 1);
			val <<= 6;
			val |= readChar(aar);
		}
		return (val & am_mask);
	}

	public int incrAdr(int adr, int inc) {
		int a = (adr + inc) & am_mask;
		return (adr & ~am_mask) | a;
	}

	public void incrAAR(int inc) {
		AAR = incrAdr(AAR, inc);
	}

	public void incrBAR(int inc) {
		BAR = incrAdr(BAR, inc);
	}

	public byte readMem(int adr) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		return mem[adr];
	}

	public byte readChar(int adr) {
		return (byte)(readMem(adr) & 077);
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
		mem[adr] = (byte)((mem[adr] & 0300) | (val & 077));
	}

	public void setWord(int adr) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		mem[adr] |= 0100;
	}

	public void setItem(int adr) {
		if (adr < adr_min || adr >= adr_max) {
			throw new RuntimeException("Address violation");
		}
		mem[adr] |= 0200;
	}

	// 
	private int fetchAddr(int ptr, int ref) {
		int a = 0;
		for (int n = am_na; n > 0; --n) {
			a = (a << 6) | readChar(ptr++);
		}
		byte am = (byte)(a >> am_shift);
		a = (a & am_mask) | (ref & ~am_mask);
		if (am == 0) {
			return a;
		}
		if (am_na == 3 && am == 0x07 || am == 0x10) {
			// Indirect...
			return fetchAddr(a, a);
		}
		// Indexed... determine which index register
		int ix = (((am & 0x0f) - 1) * 4);
		if (am > 0x10) {
			// must be 4-char addr mode
			ix += (IBR << 12);	// Y1-Y15
		} else if (am_na == 4) {
			// no further adjustment for X1-X15
		} else {
			// must be 3-char addr mode
			ix += (SR & ~0x07fff);	// X1-X6
		}
		int ax = 0;
		for (int n = 4; n > 0; --n) {
			ax = (ax << 6) | readChar(ix++);
		}
		a = (a + ax) & 0x7ffff;
		return a;
	}

	public void fetchAAR(int limit) {
		if (!hasA() || limit - fsr < am_na) {
			if (reqA()) {
				throw new RuntimeException("Missing required A-field");
			}
			return;
		}
		op_xflags |= InstrDecode.OP_HAS_A;
		iaar = AAR;
		AAR = fetchAddr(fsr, AAR);
		fsr += am_na;
	}

	public void restoreAAR() {
		if (iaar >= 0) {
			AAR = iaar;
		}
	}

	public void fetchBAR(int limit) {
		if (!hadA() || !hasB() || limit - fsr < am_na) {
			if (reqB()) {
				throw new RuntimeException("Missing required B-field");
			}
			if (dupA()) {
				BAR = AAR;
			}
			return;
		}
		op_xflags |= InstrDecode.OP_HAS_B;
		BAR = fetchAddr(fsr, BAR);
		fsr += am_na;
	}

	private void fetchXtra(int limit) {
		op_xtra_num = limit - fsr;
		if (op_xtra_num <= 0) {
			if (reqV()) {
				// probably just let instructions do this...
				throw new RuntimeException("Missing required variant");
			}
			return;
		}
		if (op_xtra_num > op_xtra_siz) {
			op_xtra_siz = op_xtra_num + 2;
			op_xtra = new byte[op_xtra_siz];
		}
		for (int x = 0; x < op_xtra_num; ++x) {
			op_xtra[x] = readMem(fsr + x);
		}
	}

	public int numXtra() {
		return op_xtra_num;
	}

	public byte getXtra(int ix) {
		if (ix >= op_xtra_num) {
			return 0;
		}
		return op_xtra[ix];
	}

	public void checkIntr() {
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

	public void clearIntr() {
		byte i = CTL.clearIntr();
		int t;
		if (i == HW2000CCR.EIR_EI) {
			t = SR;
			SR = EIR;
			EIR = t;
		} else if (i == HW2000CCR.EIR_II) {
			t = SR;
			SR = IIR;
			IIR = t;
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

		oSR = SR;
		fsr = SR;
		// TODO: how to avoid including garbage in variant array.
		int isr = (fsr + 1) & 0x1ffff;
		while (isr != 0 && (mem[isr] & M_WM) == 0) {
			isr = (isr + 1) & 0x1ffff;
		}
		iaar = -1;
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

	private boolean _trace = false;

	public void loadNGo(String pgm, byte am, int start, boolean trace) {
		_trace = trace;
		int n = -1;
		try {
			FileInputStream f = new FileInputStream(pgm);
			n = f.read(mem);
			f.close();
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
		if (n <= 0) {
			return;
		}
		setAM(am);
		SR = start;
		run();
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

	public void dumpMem(String tag, int excl, int end) {
		int start = excl;
		System.err.format("{%s=", tag);
		if (end - start > 8) {
			System.err.format("...");
			start = end - 8;
		}
		while (start < end) {
			System.err.format(" %03o", mem[++start] & 0x0ff);
		}
		System.err.format("}");
	}
}
