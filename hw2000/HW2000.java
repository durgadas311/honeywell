import java.util.Arrays;
import java.io.*;

public class HW2000 implements CoreMemory
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
	private boolean _proceed;

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
		idc = new InstrDecode(false);
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

	public boolean isProceed() { return _proceed; }

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
			throw new FaultException("Invalid OpCode");
		}
		op_exec = idc.getExec(op);
		if (priv() && CTL.inStdMode() && CTL.isPROTECT() &&
				!isProceed()) {
			throw new IIException("OpCode Violation " + op, HW2000CCR.IIR_OPVIO);
		}
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

	private int validAdr(int adr) {
		int a = adr;
		if (CTL.inStdMode()) {
			if (CTL.isRELOC()) {
				a += adr_min;
			}
			if (CTL.isPROTECT() &&
					a < adr_min || a >= adr_max) {
				throw new IIException(
					String.format("Address violation %07o", a),
					HW2000CCR.IIR_ADRVIO);
			}
		}
		return a;
	}

	public byte readMem(int adr) {
		int a = validAdr(adr);
		return mem[a];
	}

	public byte readChar(int adr) {
		return (byte)(readMem(adr) & 077);
	}

	public void rawWriteMem(int adr, byte val) {
		mem[adr] = val;
	}

	public void writeMem(int adr, byte val) {
		int a = validAdr(adr);
		mem[a] = val;
	}

	public void writeChar(int adr, byte val) {
		int a = validAdr(adr);
		mem[a] = (byte)((mem[a] & 0300) | (val & 077));
	}

	public void setWord(int adr) {
		int a = validAdr(adr);
		mem[a] |= 0100;
	}

	public void setItem(int adr) {
		int a = validAdr(adr);
		mem[a] |= 0200;
	}

	public void clrWord(int adr) {
		int a = validAdr(adr);
		mem[a] &= ~0100;
	}

	public void clrItem(int adr) {
		int a = validAdr(adr);
		mem[a] &= ~0200;
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
				throw new FaultException("Missing required A-field");
			}
			return;
		}
		op_xflags |= InstrDecode.OP_HAS_A;
		iaar = AAR;
		AAR = fetchAddr(fsr, 0);
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
				throw new FaultException("Missing required B-field");
			}
			if (dupA()) {
				BAR = AAR;
			}
			return;
		}
		op_xflags |= InstrDecode.OP_HAS_B;
		BAR = fetchAddr(fsr, 0);
		fsr += am_na;
	}

	private void fetchXtra(int limit) {
		op_xtra_num = limit - fsr;
		if (op_xtra_num <= 0) {
			if (reqV()) {
				// probably just let instructions do this...
				throw new FaultException(
					String.format("Missing required variant %07o", oSR));
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
			setAM(CTL.getAM());
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
		if (CTL.inStdMode()) {
			_proceed = CTL.isPROCEED();
			CTL.clrPROCEED();
		}
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
		while (isr != 0 && (readMem(isr) & M_WM) == 0) {
			isr = (isr + 1) & 0x1ffff;
		}
		iaar = -1;
		// Caller handles exceptions, leave SR at start of instruction
		// (if during fetch/extract). Exceptions during execute
		// terminate instruction and leave SR at next.
		if (isr == 0) {
			// ran off end of memory... need to halt...
			halt = true;
			throw new FaultException("ran off end of memory");
		}
		setOp(readMem(fsr++));	// might throw illegal op-code
		fetchAAR(isr);	// might throw exceptions
		fetchBAR(isr);	// might throw exceptions
		// just get all extra characters, let implementations
		// sort it out...
		fetchXtra(isr);
		SR = isr;
	}

	public void execute() {
		op_exec.execute(this);
	}

	private boolean _trace = false;

	// Set a value into a word-marked field
	private void setField(int adr, int val) {
		int w;
		do {
			w = mem[adr] & 0100;
			mem[adr] = (byte)((mem[adr] & 0300) | (val & 077));
			val >>= 6;
			--adr;
		} while (w == 0);
	}

	public void monGo(String pgm, String lst, boolean trace) {
		File list = null;
		if (lst != null) {
			list = new File(lst);
		}
		_trace = trace;
		Assembler asm = new Assembler(new File(pgm));
		int e = asm.passOne();
		if (e < 0) {
			return;
		}
		int low = asm.getMin();
		int hi = asm.getMax();
		int start = asm.getStart();
		int brr = 2;
		int ibr = 2; // give only 4K for now...
		int reloc = (brr << 12);
		e = asm.passTwo(this, reloc, list);
		if (e < 0) {
			return;
		}
		System.err.format("Running via monitor %07o %07o %07o\n", low, hi, start);
		setField(0007, ibr);
		setField(0005, brr);
		setField(0003, start);
		SR = CSR;
		run();
		if (_trace) {
			dumpRange(reloc + low, reloc + hi);
		}
	}

	public void asmNGo(String pgm, String lst, boolean trace) {
		File list = null;
		if (lst != null) {
			list = new File(lst);
		}
		_trace = trace;
		Assembler asm = new Assembler(new File(pgm));
		int e = asm.passOne();
		if (e < 0) {
			return;
		}
		int low = asm.getMin();
		int hi = asm.getMax();
		int start = asm.getStart();
		e = asm.passTwo(this, 0, list);
		if (e < 0) {
			return;
		}
		System.err.format("Running %07o %07o %07o\n", low, hi, start);
		setAM(HW2000CCR.AIR_AM_2C);	// TODO: fix this
		SR = start;
		run();
		if (_trace) {
			dumpRange(low, hi);
		}
	}

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

	private boolean setIntr(byte mod, byte typ) {
		if (CTL.inStdMode()) {
			SR = oSR;
			if (mod == HW2000CCR.EIR_EI) {
				CTL.setEI(typ);
			} else {
				CTL.setII(typ);
			}
			return true;
		} else if (CTL.inEI() && mod == HW2000CCR.EIR_II) {
			CTL.setII(typ);
			return true;
		} else {
			return false;
		}
	}

	public void run() {
		while (!halt) {
			try {
				fetch();
if (_trace) {
	String op = op_exec.getClass().getName();
	System.err.format("%07o: %s [%07o %07o] (%d)\n", oSR, op, AAR, BAR, op_xtra_num);
	System.err.flush();
}
				execute();
			} catch (IIException ie) {
				// TODO: need to handle II within II...
				if (IIR != 0 && setIntr(HW2000CCR.EIR_II, ie.type)) {
if (_trace) {
	String op = op_exec.getClass().getName();
	System.err.format("II %07o: %s [%07o %07o] (%d)\n", oSR, op, AAR, BAR, op_xtra_num);
	System.err.flush();
}
				} else {
					ie.printStackTrace();
					halt = true;
				}
			} catch (EIException ee) {
				if (EIR != 0 && setIntr(HW2000CCR.EIR_EI, ee.type)) {
if (_trace) {
	String op = op_exec.getClass().getName();
	System.err.format("EI %07o: %s [%07o %07o] (%d)\n", oSR, op, AAR, BAR, op_xtra_num);
	System.err.flush();
}
				} else {
					ee.printStackTrace();
					halt = true;
				}
			} catch (Exception fe) {
				fe.printStackTrace();
				halt = true;
			}
		}
		halt = false;
	}

	// Range is inclusive, both ends
	public void dumpRange(int beg, int end) {
		int x = 0;
		int m = beg;
		while (m <= end) {
			if (x == 0) {
				System.err.format("%07o:", m);
			}
			System.err.format(" %03o", mem[m++] & 0x0ff);
			if (++x >= 16) {
				x = 0;
				System.err.format("\n");
			}
		}
		if (x != 0) {
			System.err.format("\n");
		}
	}

	public void dumpMem(String tag, int excl, int end) {
		int start = excl;
		System.err.format("%s={", tag);
		if (end - start > 8) {
			System.err.format("...");
			start = end - 8;
		}
		while (start < end) {
			System.err.format(" %03o", mem[++start] & 0x0ff);
		}
		System.err.format(" }");
	}

	public void dumpBin(String tag, int var) {
		long l = 0;
		int b = var;
		int x = 0;
		while (x < 15 && b > 0 && (mem[b] & 0100) == 0) {
			--b;
			++x;
		}
		System.err.format("%s={", tag);
		while (b <= var) {
			System.err.format(" %03o", mem[b] & 0x0ff);
			l = (l << 6) | (mem[b] & 077);
			++b;
		}
		System.err.format("} (%d)\n", l);
	}

	public void dumpDec(String tag, int var) {
		long l = 0;
		int b = var;
		int x = 0;
		int s = mem[var] & 060;
		while (x < 15 && b > 0 && (mem[b] & 0100) == 0) {
			--b;
			++x;
		}
		System.err.format("%s={", tag);
		while (b <= var) {
			System.err.format(" %03o", mem[b] & 0x0ff);
			l = (l * 10) + (mem[b] & 017);
			++b;
		}
		System.err.format("} (%s%d)\n", (s == 040 ? "-" : "+"), l);
	}
}
