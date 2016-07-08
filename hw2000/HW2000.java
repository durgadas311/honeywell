import java.util.Arrays;
import java.io.*;
import java.util.concurrent.Semaphore;

public class HW2000 implements CoreMemory
{
	public static final byte M_IM = (byte)0x80;
	public static final byte M_WM = 0x40;
	public static final byte M_SIGN = 0x30;
	public static final byte M_DIGIT = 0x0f;
	public static final byte M_CHAR = 0x3f;

	public static final byte LOR = 007;

	byte[] mem;
	public int[] cr;

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
	public boolean[] denorm;

	public int oSR;
	int op_flags;
	int op_xflags;
	private byte[] op_xtra;
	private int op_xtra_siz;
	private int op_xtra_num;
	Instruction op_exec;
	public boolean halt;
	public boolean singleStep;
	public boolean bootstrap; // force clearing of punctuation
	private boolean _proceed;
	private int tics;

	private int fsr;
	public int iaar;	// needed by branch instructions
	public int am_mask;	// populated by CAM instruction
	public int am_shift;	// populated by CAM instruction
	public int am_na;	// populated by CAM instruction
	public int adr_min;	// set by changes in PROTECT, based on IBR/BRR (RVI instr)
	public int adr_max;	// set by changes in PROTECT, based on IBR/BRR (RVI instr)

	InstrDecode idc;
	PeriphDecode pdc;
	FrontPanel fp;

	public HW2000() {
		waitLock = new Semaphore(1);
		fp = null;
		CTL = new HW2000CCR();
		AC = new double[8];
		denorm = new boolean[8];
		Arrays.fill(denorm, false);
		mem = new byte[524288]; // TODO: mmap file
		Arrays.fill(mem, (byte)0);
		cr = new int[64];
		Arrays.fill(cr, 0);
		idc = new InstrDecode(false);
		pdc = new PeriphDecode();
		op_xtra_siz = 8;
		op_xtra = new byte[op_xtra_siz];

		adr_min = 0;
		adr_max = 0x80000;
		AAR = 0;
		BAR = 0;
		reset();
	}

	public void setFrontPanel(FrontPanel fp) {
		this.fp = fp;
	}

	public void setTrace(int low, int hi) {
		_trace_low = low;
		_trace_hi = hi;
		_trace = (low < hi);
	}

	public void reset() {
		halt = true;
		singleStep = false;
		bootstrap = false;
		_trace = false;
		tics = 0;
		ATR = 0;
		SR = 0;
		setAM(HW2000CCR.AIR_AM_3C);
		CTL.reset();
		Arrays.fill(mem, (byte)0); // This is a cheat, but needed for now
		pdc.reset();
	}

	public byte getSENSE() {
		if (fp != null) {
			return (byte)fp.getSense();
		} else {
			return 0;
		}
	}

	// A wait/wake mechanism to use for I/O to avoid spinning
	private Semaphore waitLock;
	public void waitIO() {
		try {
			waitLock.acquire();
		} catch (Exception ee) {}
	}
	public void endWait() {
		waitLock.release();
	}
	public void setupWait() {
		// This is used to ensure waiter will sleep next time...
		waitLock.drainPermits();
	}

	// 'tics' could also be used as instruction timeout counter,
	// but would need to check during memory access, too.
	public void addTics(int tics) {
		this.tics += tics;
	}

	private void updClock() {
		// TODO: don't even advance clock unless enabled?
		if (CTL.isPROTECT() && CTL.allowCLK()) {
			ATR += tics;
			int cy = ATR & ~077777777;
			ATR &= 077777777;
			if (cy != 0) { //should only ever be "1(xxx)"
				// TODO: trigger interrupt as appropriate
				CTL.setEI(HW2000CCR.EIR_CLOCK);
			}
		}
		// In all cases, reset tics
		tics = 0;
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
	public boolean noWM() { return ((op_flags & InstrDecode.OP_NO_WM) != 0); }
	public boolean hadA() { return ((op_xflags & InstrDecode.OP_HAS_A) != 0); }
	public boolean hadB() { return ((op_xflags & InstrDecode.OP_HAS_B) != 0); }
	public boolean hadV() { return ((op_xflags & InstrDecode.OP_HAS_V) != 0); }

	public boolean isProceed() { return _proceed; }

	public Peripheral getPeriph(byte op) {
		Peripheral p = pdc.getPeriph(op);
		if (p == null) {
			throw new RuntimeException("Invalid Peripheral " + op);
		}
		return p;
	}

	public RWChannel getChannel(byte op) {
		RWChannel p = pdc.getChannel(op);
		if (p == null) {
			throw new RuntimeException("Invalid Channel " + op);
		}
		return p;
	}

	private void setOp(byte op) {
		op_exec = null;
		op_flags = idc.getFlags(op);
		if (inval()) {
			throw new IIException("OpCode Violation " + op, HW2000CCR.IIR_OPVIO);
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
		if (fp != null) {
			fp.setAdrMode(am_na);
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

	public int validAdr(int adr) {
		int a = adr;
		// TODO: if PROCEED is set on PDT, also check violations
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

	public byte rawReadMem(int adr) {
		return mem[adr];
	}

	public void rawWriteMem(int adr, byte val) {
		mem[adr] = val;
	}

	public void rawWriteChar(int adr, byte val) {
		if (bootstrap) {
			// force clearing of punctuation
			mem[adr] = (byte)(val & 077);
		} else {
			mem[adr] = (byte)((mem[adr] & 0300) | (val & 077));
		}
	}

	public byte readMem(int adr) {
		int a = validAdr(adr);
		++tics;
		return mem[a];
	}

	public byte readChar(int adr) {
		return (byte)(readMem(adr) & 077);
	}

	public void writeMem(int adr, byte val) {
		int a = validAdr(adr);
		++tics;
		mem[a] = val;
	}

	// 'mask' is bits to preserve from current mem value
	public byte writeMemMask(int adr, byte val, byte mask) {
		int a = validAdr(adr);
		++tics;
		mem[a] = (byte)((mem[a] & mask) | (val & ~mask));
		return mem[a];
	}

	public void writeChar(int adr, byte val) {
		writeMemMask(adr, val, (byte)0300);
	}

	// These do not suffer accounting timer tics?
	public boolean chkWord(int adr) {
		int a = validAdr(adr);
		return (mem[a] & 0100) != 0;
	}

	public boolean chkItem(int adr) {
		int a = validAdr(adr);
		return (mem[a] & 0200) != 0;
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
		// 'am' is 1-6 (3 char adr) or 1-15,17-31
		// 'ix' must be a *physical* address after all this...
		int ix = ((am & 0x0f) * 4) - am_na + 1;
		// this is really confusing... not sure at all...
		if (am > 0x10) {
			// must be 4-char addr mode.
			if (!CTL.isRELOC()) {
				ix += (IBR << 12);	// Y1-Y15
			}
		} else if (am_na == 4) {
			// no further adjustment for X1-X15?
		} else {
			// must be 3-char addr mode
			if (!CTL.isRELOC()) {
				ix += (oSR & ~0x07fff);	// X1-X6
			}
		}
		if (CTL.isRELOC()) {
			ix += (BRR << 12);	// Y1-Y15
		}
		// 'ix' is physical address. This also avoids protection, which is implied in docs
		int ax = 0;
		for (int n = am_na; n > 0; --n) {
			ax = (ax << 6) | (rawReadMem(ix++) & 077);
		}
		ax &= am_mask;
		a = (a + ax) & 0x7ffff;
		return a;
	}

	public void fetchAAR(int limit) {
		if (!hasA() || limit - fsr < am_na) {
			if (reqA()) {
				throw new FaultException(String.format("Missing required A-field %07o", oSR));
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
				throw new FaultException(String.format("Missing required B-field %07o", oSR));
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
			op_xtra[x] = readChar(fsr + x);
		}
	}

	public int numXtra() {
		return op_xtra_num;
	}

	public void setXtra(byte[] xt) {
		op_xtra_num = xt.length;
		if (op_xtra_num > op_xtra_siz) {
			op_xtra_siz = op_xtra_num + 2;
			op_xtra = new byte[op_xtra_siz];
		}
		for (int x = 0; x < op_xtra_num; ++x) {
			op_xtra[x] = xt[x];
		}
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
			if (fp != null) {
				fp.setInterrupt(HW2000CCR.EIR_EI);
			}
		} else if (CTL.isII()) {
			int t = IIR;
			IIR = SR;
			SR = t;
			if (fp != null) {
				fp.setInterrupt(HW2000CCR.EIR_II);
			}
		}
	}

	public void clearIntr() {
		byte i = CTL.clearIntr();
		if (i == 0) {
			return;
		}
		if (fp != null) {
			fp.setInterrupt(0);
		}
		int t;
		if (i == HW2000CCR.EIR_EI) {
			// TODO: handle pending II?
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
		iaar = -1;
		setOp(readMem(fsr++));	// might throw illegal op-code
		// TODO: how to avoid including garbage in variant array.
		int isr = fsr & 0x7ffff;
		if (noWM()) {
			int max = (hasA() ? am_na : 0);
			max += (hasB() ? am_na : 0);
			while (isr != 0 && isr - fsr < max && !chkWord(isr)) {
				isr = (isr + 1) & 0x7ffff;
			}
		} else {
			while (isr != 0 && !chkWord(isr)) {
				isr = (isr + 1) & 0x7ffff;
			}
		}
		// Caller handles exceptions, leave SR at start of instruction
		// (if during fetch/extract). Exceptions during execute
		// terminate instruction and leave SR at next.
		if (isr == 0) {
			// ran off end of memory... need to halt...
			halt = true;
			throw new FaultException(String.format("ran off end of memory %07o", oSR));
		}
		fetchAAR(isr);	// might throw exceptions
		fetchBAR(isr);	// might throw exceptions
		// just get all extra characters, let implementations
		// sort it out...
		fetchXtra(isr);
		SR = isr;
		// NOTE: this does not cover the case of exceptions above
		if (_trace && oSR >= _trace_low && oSR < _trace_hi) {
			traceInstr();
		}
	}

	private void traceInstr() {
		String op = op_exec.getClass().getName();
		String s = String.format("%07o: %s [%07o %07o] ", oSR, op, AAR, BAR);
		for (int x =  0; x < op_xtra_num; ++x) {
			s += String.format(" %02o", op_xtra[x]);
		}
		s += "\n";
		listOut(s);
	}

	int count = 0;

	public void execute() {
		if (fp != null && !singleStep) {
			if (count == 0) {
				fp.setAddress(oSR);
				fp.setContents(mem[oSR]);
			}
			if (++count > 20) {
				count = 0;
			}
		}
		op_exec.execute(this);
		updClock();
	}

	private boolean _trace = false;
	private int _trace_low = 0;
	private int _trace_hi = 02000000;

	// Set a value into a word-marked field
	public void setField(int adr, int val) {
		int w;
		do {
			w = mem[adr] & 0100;
			mem[adr] = (byte)((mem[adr] & 0300) | (val & 077));
			val >>= 6;
			--adr;
		} while (w == 0);
	}

	private boolean setIntr(byte mod, byte typ) {
		if (CTL.inStdMode()) {
			SR = oSR; // this is problematic...
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
		if (fp != null) {
			fp.setRunStop(true);
		}
		halt = false;
		while (!halt) {
			try {
				fetch();
				execute();
			} catch (IIException ie) {
				// TODO: need to handle II within II...
				if (_trace && oSR >= _trace_low && oSR < _trace_hi) {
					traceInstr();
				}
				if (IIR != 0 && setIntr(HW2000CCR.EIR_II, ie.type)) {
					// nothing else
				} else {
					if (fp != null) {
						HW2000FrontPanel.warning(fp.getContentPane(), "Run",
							String.format("%07o: %s",
								oSR,
								ie.getMessage()));
					} else {
						ie.printStackTrace();
					}
					halt = true;
				}
			} catch (EIException ee) {
				if (_trace && oSR >= _trace_low && oSR < _trace_hi) {
					traceInstr();
				}
				if (EIR != 0 && setIntr(HW2000CCR.EIR_EI, ee.type)) {
					// nothing else
				} else {
					if (fp != null) {
						HW2000FrontPanel.warning(fp.getContentPane(), "Run",
							String.format("%07o: %s",
								oSR,
								ee.getMessage()));
					} else {
						ee.printStackTrace();
					}
					halt = true;
				}
			} catch (Exception fe) {
				if (fp != null) {
					HW2000FrontPanel.warning(fp.getContentPane(), "Run",
						String.format("%07o: %s",
								oSR,
								fe.getMessage()));
				} else {
					fe.printStackTrace();
				}
				halt = true;
			}
			if (singleStep) {
				singleStep = false;
				halt = true;
			}
		}
		if (fp != null) {
			fp.setRunStop(false);
			fp.setAddress(oSR);
			fp.setContents(mem[oSR]);
		}
	}

	public void listOut(String str) {
		Peripheral p = pdc.getPeriph(PeriphDecode.P_LP);
		if (p == null) {
			return;
		}
		p.output(str);
	}

	// Honeywell-style memory dump.
	// Range is inclusive, both ends
	public void dumpHW(int beg, int end) {
		String marks = " WIR";
		int m = beg & ~0177;	// 128 locations per row...
		listOut(String.format("Memory Dump: %07o - %07o\n", beg, end));
		while (m <= end) {
			String l = String.format("%07o 1       2       3       4       5       6       7"
				+ "       %07o 1       2       3       4       5       6       7\n", m, m + 64);
			listOut(l);
			l = "";
			int a = m;
			int e = m + 128;
			while (a < e && a <= end) {
				if (a < beg) {
					l += ' ';
				} else {
					l += pdc.cvt.hwToLP((byte)(mem[a] & 077));
				}
				++a;
			}
			listOut(l + "\n");
			l = "";
			a = m;
			e = m + 128;
			while (a < e && a <= end) {
				if (a < beg) {
					l += ' ';
				} else {
					l += (char)(((mem[a] >> 3) & 07) + '0');
				}
				++a;
			}
			listOut(l + "\n");
			l = "";
			a = m;
			e = m + 128;
			while (a < e && a <= end) {
				if (a < beg) {
					l += ' ';
				} else {
					l += (char)((mem[a] & 07) + '0');
				}
				++a;
			}
			listOut(l + "\n");
			l = "";
			a = m;
			e = m + 128;
			while (a < e && a <= end) {
				if (a < beg) {
					l += ' ';
				} else {
					l += marks.charAt((mem[a] >> 6) & 03);
				}
				++a;
			}
			listOut(l + "\n");
			m += 128;
		}
	}

	public void dumpMem(String tag, int excl, int incl) {
		int start = validAdr(excl);
		int end = validAdr(incl);
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
