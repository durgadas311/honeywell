// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;
import java.util.Properties;
import java.util.Random;
import java.io.*;
import java.util.concurrent.Semaphore;
import java.lang.reflect.Constructor;

public class HW2000 implements CoreMemory
{
	public static final int checkIntv = 1000; // number of tics between sleeps
	public static final long nsPerTic = 2000; // 2uS per tic (cycle)

	public static final byte M_IM = (byte)0x80;
	public static final byte M_WM = 0x40;
	public static final byte M_SIGN = 0x30;
	public static final byte M_DIGIT = 0x0f;
	public static final byte M_CHAR = 0x3f;

	public static final byte LOR = 010;

	byte[] mem;
	public int[] cr;

	int ATR;
	int atrr;
	public int CSR;
	public int EIR;
	public int AAR;
	public int BAR;
	public int IIR;
	public int SR;

	public byte IBR;
	public byte BRR;
	public HW2000CCR CTL;
	public long[] AC;
	public boolean[] denorm;

	public int oSR;
	public int oAAR;
	public int oBAR;
	public byte opSR; // op-code at oSR (physical)
	int op_flags;
	int op_xflags;
	private byte[] op_xtra;
	private int op_xtra_siz;
	private int op_xtra_num;
	private boolean isPDT;
	private boolean protViol;	// delayed exception
	private int protAdr;
	Instruction op_exec;
	public boolean halt;
	public boolean singleStep;
	public boolean bootstrap; // force clearing of punctuation
	private boolean _proceed;
	private int tics;
	private long u0; // FP update timer
	private boolean throttled;
	private Map<String, HW2000Trap> traps;

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

	public HW2000(Properties props) {
		waitLock = new Semaphore(1);
		fp = null;
		CTL = new HW2000CCR();
		AC = new long[9];
		denorm = new boolean[9];
		Arrays.fill(denorm, false);
		mem = new byte[524288]; // TODO: mmap file
		Arrays.fill(mem, (byte)0);
		traps = new HashMap<String, HW2000Trap>();
		cr = new int[64];
		Arrays.fill(cr, 0);
		idc = new InstrDecode(false);
		pdc = new PeriphDecode(props, this);
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
		for (HW2000Trap trap : traps.values()) {
			trap.done();
		}
		traps.clear();
		tics = 0;
		ATR = 0;
		atrr = 0;
		SR = 0;
		setAM(HW2000CCR.AIR_AM_2C);
		CTL.reset();
		pdc.reset();
	}

	public void clearMem() {
		Arrays.fill(mem, (byte)0);
	}

	public void randMem() {
		Random r = new Random(System.nanoTime());
		r.nextBytes(mem);
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
			atrr += tics;	// TODO: always incr?
			int cy = atrr & ~0377;
			atrr &= 0377;	// (i.e. free-running?)
			if (cy != 0) {
				ATR += 1;
				ATR &= 01777777;
				if (ATR == 0) {
					// TODO: trigger interrupt as appropriate
					CTL.setEI(HW2000CCR.EIR_CLOCK);
				}
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

	// Translate the address
	public int validAdr(int adr) {
		int a = adr;
		if (CTL.inStdMode() && CTL.isRELOC()) {
			a += adr_min;
		}
		return a;
	}

	// Translate address and check for protection violation
	public int writeAdr(int adr) {
		int a = validAdr(adr);
		if (CTL.inStdMode() && CTL.isPROTECT() && !isPDT &&
				a < adr_min || a >= adr_max) {
			protViol = true;
			protAdr = a;
			return -1;
		}
		return a;
	}

	public byte rawReadMem(int adr) {
		return mem[adr];
	}

	public int getAddr(int adr) {
		int v = (mem[adr--] & 077);
		v |= (mem[adr--] & 077) << 6;
		v |= (mem[adr--] & 077) << 12;
		v |= (mem[adr--] & 077) << 18;
		return v;
	}

	// No punc set, a la EXM...011
	public void putStr(int adr, String val, int max) {
		int n = 0;
		while (n < max && n < val.length()) {
			mem[adr++] = pdc.cvt.asciiToHw((byte)
				Character.toUpperCase(val.charAt(n)));
			++n;
		}
		while (n < max) {
			mem[adr++] = (byte)015; // blanks
			++n;
		}
	}

	public void putRaw(int adr, long val, int len) {
		// TODO: preserve punct?
		while (len-- > 0) {
			mem[adr--] = (byte)(val & 077);
			val >>= 6;
		}
	}

	// Overwrites punc, like LCA
	public void putAddr(int adr, int val, int pnc) {
		mem[adr--] = (byte)(val & 077);
		val >>= 6;
		mem[adr--] = (byte)(val & 077);
		val >>= 6;
		mem[adr--] = (byte)(val & 077);
		val >>= 6;
		mem[adr--] = (byte)((val & 077) | pnc);
	}

	public void rawWriteMem(int adr, byte val) {
		mem[adr] = val;
	}

	public byte rawWriteChar(int adr, byte val) {
		if (bootstrap) {
			// force clearing of punctuation
			mem[adr] = (byte)(val & 077);
		} else {
			mem[adr] = (byte)((mem[adr] & 0300) | (val & 077));
		}
		return mem[adr];
	}

	private void fpUpdate(int a) {
		if (fp == null) return;
		long u1 = System.nanoTime();
		// update every ~20mS
		if (u1 - u0 >= 20000000) {
			fp.setAddress(a);
			fp.setContents(mem[a]);
			u0 = u1;
		}
	}

	public byte readMem(int adr) {
		if (halt) {
			throw new HaltException("memory read");
		}
		int a = validAdr(adr);
		++tics;
		if (tics > 1000 && CTL.inStdMode() &&
				CTL.isPROTECT() && CTL.isTIMOUT()) {
			throw new IIException("Instruction Timeout",
				HW2000CCR.IIR_TIMOUT);
		}
		fpUpdate(a);
		return mem[a];
	}

	public byte readChar(int adr) {
		return (byte)(readMem(adr) & 077);
	}

	public void writeMem(int adr, byte val) {
		if (halt) {
			throw new HaltException("memory write");
		}
		int a = writeAdr(adr);
		++tics;
		if (tics > 1000 && CTL.inStdMode() &&
				CTL.isPROTECT() && CTL.isTIMOUT()) {
			throw new IIException("Instruction Timeout",
				HW2000CCR.IIR_TIMOUT);
		}
		if (a < 0) {
			return;
		}
		mem[a] = val;
		fpUpdate(a);
	}

	// 'mask' is bits to preserve from current mem value
	public byte writeMemMask(int adr, byte val, byte mask) {
		if (halt) {
			throw new HaltException("memory mask");
		}
		int a = validAdr(adr);
		++tics;
		if (tics > 1000 && CTL.inStdMode() &&
				CTL.isPROTECT() && CTL.isTIMOUT()) {
			throw new IIException("Instruction Timeout",
				HW2000CCR.IIR_TIMOUT);
		}
		if (CTL.inStdMode() && CTL.isPROTECT() && !isPDT &&
				a < adr_min || a >= adr_max) {
			protViol = true;
			protAdr = a;
		} else {
			mem[a] = (byte)((mem[a] & mask) | (val & ~mask));
			fpUpdate(a);
		}
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
		int a = writeAdr(adr);
		if (a < 0) {
			return;
		}
		mem[a] |= 0100;
	}

	public void setItem(int adr) {
		int a = writeAdr(adr);
		if (a < 0) {
			return;
		}
		mem[a] |= 0200;
	}

	public void clrWord(int adr) {
		int a = writeAdr(adr);
		if (a < 0) {
			return;
		}
		mem[a] &= ~0100;
	}

	public void clrItem(int adr) {
		int a = writeAdr(adr);
		if (a < 0) {
			return;
		}
		mem[a] &= ~0200;
	}

	public void copyIn(int adr, byte[] buf, int start, int len) {
		for (int x = 0; x < len; ++x) {
			writeChar(adr++, buf[start + x]);
		}
	}

	public void copyOut(int adr, byte[] buf, int start, int len) {
		int a = start;
		for (int x = 0; x < len; ++x) {
			buf[a] = (byte)((buf[a] & 0300) | readChar(adr++));
			++a;
		}
	}

	public void copyIn(int adr, CoreMemory buf, int start, int len) {
		for (int x = 0; x < len; ++x) {
			writeChar(adr++, buf.readChar(start + x));
		}
	}

	public void copyOut(int adr, CoreMemory buf, int start, int len) {
		buf.copyIn(start, this, adr, len);
	}

	public boolean compare(int adr, CoreMemory buf, int start, int len) {
		for (int x = 0; x < len; ++x) {
			if (readChar(adr++) != buf.readChar(start + x)) {
				return false;
			}
		}
		return true;
	}
	public boolean compare(int adr, byte[] buf, int start, int len) {
		int a = start;
		for (int x = 0; x < len; ++x) {
			if (readChar(adr++) != (buf[a++] & 077)) {
				return false;
			}
		}
		return true;
	}

	public void zero(int adr, int len) {
		int a = writeAdr(adr);
		if (a >= mem.length) {
			return;
		}
		if (a < 0) {
			return;
		}
		if (len < 0) {
			len = mem.length - a;
		}
		int end = a + len;
		if (end > mem.length) {
			end = mem.length;
		}
		Arrays.fill(mem, a, end, (byte)0);
	}

	public int size() {
		return mem.length;
	}

	public int getCtrlReg(byte reg) {
		int val = 0;
		int r;
		long d;
		byte s;
		switch(reg & 077) {
		case 041:
		case 045:
		case 051:
		case 055: // exponent of FP ACC
		case 042:
		case 046:
		case 052:
		case 056: // low mantissa of FP ACC
		case 043:
		case 047:
		case 053:
		case 057: // high mantissa of FP ACC
			r = (reg & 014) >> 2;
			d  = AC[r];
			s = (byte)((d >> 48) & 1);
			if ((d & 03777777777777777L) == 0) {
				break;
			}
			switch(reg & 003) {
			case 001: // exponent
				val = (int)(d & 07777);
				break;
			case 002: // low mantissa
				val = (int)((d >> 12) & 0777777);
				break;
			case 003: // high mantissa
				val = (int)((d >> 30) & 0777777);
				break;
			}
			break;
		case 054:
			val = ATR;
			break;
		case 064:
			val = CSR;
			break;
		case 066:
			val = EIR;
			break;
		case 070:
			val = BAR;
			break;
		case 074:
			val = AAR;
			break;
		case 076:
			val = IIR;
			break;
		case 077:
			val = SR;
			break;
		default:
			val = cr[reg & 077];
			break;
		}
		return val;
	}

	public void setCtrlReg(byte reg, int val) {
		int r;
		long d;
		byte s;
		switch(reg & 077) {
		case 041:
		case 045:
		case 051:
		case 055: // exponent of FP ACC
		case 042:
		case 046:
		case 052:
		case 056: // low mantissa of FP ACC
		case 043:
		case 047:
		case 053:
		case 057: // high mantissa of FP ACC
			r = (reg & 014) >> 2;
			d  = AC[r];
			switch(reg & 003) {
			case 001: // exponent
				val &= 07777;
				d = (d & 07777777777770000L) | val;
				break;
			case 002: // low mantissa
				// must know sign to handle properly...
				val &= 0777777;
				d = (d & 07777770000007777L) | (val << 12);
				break;
			case 003: // high mantissa
				val &= 0777777;
				denorm[r] = ((val & 0200000) == 0);
				d = (d & 00000007777777777L) | (val << 30);
				break;
			}
			AC[r] = d;
			break;
		case 054:
			ATR = val & 01777777;
			break;
		case 064:
			CSR = val & 01777777;
			break;
		case 066:
			EIR = val & 01777777;
			break;
		case 070:
			BAR = val & 01777777;
			break;
		case 074:
			AAR = val & 01777777;
			break;
		case 076:
			IIR = val & 01777777;
			break;
		case 077:
			SR = val & 01777777;
			break;
		default:
			cr[reg & 077] = val & 01777777;
			break;
		}
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
			// must be 4-char addr mode. Y1-Y15
			if (!CTL.isRELOC()) {
				ix += (IBR << 12);
			}
		} else if (am_na == 4) {
			// no further adjustment for X1-X15?
		} else {
			// must be 3-char addr mode. X1-X6
			if (!CTL.isRELOC()) {
				ix += (oSR & ~0x07fff);
			}
		}
		if (CTL.isRELOC()) {
			ix += (BRR << 12);
		}
		// 'ix' is physical address. This also avoids protection, which is implied in docs
		int ax = 0;
		for (int n = am_na; n > 0; --n) {
			ax = (ax << 6) | (rawReadMem(ix++) & 077);
		}
		ax &= am_mask;
		a = ((a + ax) & am_mask) | (ref & ~am_mask);
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

	public void saveAAR() {
		if (iaar >= 0) {
			cr[067] = iaar;
		}
	}

	public void restoreAAR() {
		if (iaar >= 0) {
			AAR = cr[067];
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
		byte vr = 0;
		op_xtra_num = limit - fsr;
		if (op_xtra_num <= 0) {
			if (reqV()) {
				// probably just let instructions do this...
				throw new FaultException(
					String.format("Missing required variant %07o", oSR));
			}
			return;
		}
		op_xflags |= InstrDecode.OP_HAS_V;
		if (op_xtra_num > op_xtra_siz) {
			op_xtra_siz = op_xtra_num + 2;
			op_xtra = new byte[op_xtra_siz];
		}
		for (int x = 0; x < op_xtra_num; ++x) {
			op_xtra[x] = vr = readChar(fsr + x);
		}
		if (hasV()) {
			if ((op_flags & InstrDecode.OP_VR1) != 0) {
				CTL.setV(op_xtra[0]);
			} else {
				CTL.setV(vr);
			}
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
			// was not in interrupt...
			return;
		}
		if (fp != null) {
			fp.setInterrupt(0);
		}
		int t;
		// Interrupts may have been triggered while
		// servicing previous one(s). Intr was re-asserted,
		// but must keep EIR/IIR in sync with SR so do the
		// exchange and let fetch detect interrupt.
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

		op_xflags = 0;
		oSR = SR;
		fsr = SR;
		iaar = -1;
		opSR = readMem(fsr++); // might throw address violation
		setOp(opSR);	// might throw illegal op-code
		// TODO: how to avoid including garbage in variant array.
		int isr = fsr & 0x7ffff;
		// The problem with enforcing a max here is that some programs
		// turned off WMs in order to implement conditional execution
		// of instructions. However, it seems unlikely that such code
		// would be using that technique to turn off large blocks of
		// instructions (conditional branches would be more practical).
		int max = 500; // a reasonable limitation?
		if (noWM()) {
			max = (hasA() ? am_na : 0);
			max += (hasB() ? am_na : 0);
		}
		while (isr != 0 && isr - fsr < max && !chkWord(isr)) {
			// With enforced max, this check may not be needed.
			if (halt) {
				throw new HaltException("extraction");
			}
			// TODO: add visual feedback?
			isr = (isr + 1) & 0x7ffff;
		}
		if ((isr == 0 || isr - fsr >= max) && CTL.inStdMode() &&
				CTL.isPROTECT() && CTL.isTIMOUT()) {
			throw new IIException("Instruction Timeout, extract",
				HW2000CCR.IIR_TIMOUT);
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
		isPDT = (_proceed && op_exec instanceof I_PDT);
		// PDT A operand protection violation...
		if (isPDT && writeAdr(AAR) < 0) {
			throw new IIException(
				String.format("Address violation %07o", AAR),
				HW2000CCR.IIR_ADRVIO);
		}
	}

	// TODO: trace AAR/BAR (etc) after execute?
	private void traceInstr() {
		String op;
		Character ac, bc;
		if (op_exec == null) {
			op = "null";
		} else {
			op = op_exec.mnem();
		}
		ac = hadA() ? ' ' : '_';
		bc = hadB() ? ' ' : '_';
		String s = String.format("%07o: %-3s%c%07o%c%07o [%07o %07o]",
				oSR, op, ac, oAAR, bc, oBAR, AAR, BAR);
		if (op_xtra_num == 0) {
			s += String.format("_%02o", CTL.getV());
		} else for (int x = 0; x < op_xtra_num; ++x) {
			s += String.format(" %02o", op_xtra[x]);
		}
		s += String.format(" - %03o\n", CTL.peekCR(HW2000CCR.AIR));
		if (fp != null) {
			fp.traceOut(s);
		} else {
			System.err.format("%s", s);
		}
	}

	public int execute() {
		if (fp != null) {
			// TODO: clean this up (?)
			fp.setActive(op_exec instanceof I_PDT || op_exec instanceof I_PCB);
		}
		op_exec.execute(this);
		int clk = tics;
		updClock();
		if (protViol) {
			throw new IIException(
				String.format("Address violation %07o", protAdr),
				HW2000CCR.IIR_ADRVIO);
		}
		return clk;
	}

	private boolean _trace = false; // enable/disable
	private int _trace_low = 0;
	private int _trace_hi = 02000000;
	private boolean tracing = false; // curr instr is traced

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

	// This is for programs that load their own traps...
	// right now, we have to know what those might be...
	private void trap() {
		SR = BAR;	// return to caller, always
		int a = SR;
		String rt = "";
		do {
			rt += pdc.cvt.hwToLP((byte)(mem[a++] & 077));
		} while (a - SR < 30 && (mem[a] & 0100) == 0);
		if ((mem[a] & 0100) == 0) {
			halt = true;
			return;
		}
		if (traps.containsKey(rt)) {
			// Must advance SR past parameters, but...
			// This instance of a program might need different
			// setup than previous, so make sure we re-init.
			traps.get(rt).reinit();
			return;
		}
		//System.err.format("Trap %07o \"%s\"\n", SR, rt);
		try {
			Class<?> clazz = Class.forName(rt + "RunTime");
			Constructor<?> ctor = clazz.getConstructor(HW2000.class);
			HW2000Trap rti = (HW2000Trap)ctor.newInstance(this);
			addTrap(rti);
		} catch (Exception ee) {
			halt = true;
			return;
		}
	}

	// May be used by other classes (e.g. CoreLoader) to
	// enable traps (e.g. LoaderMonitorC).
	public void addTrap(HW2000Trap trap) {
		// Assume ctor did all necessary init...
		String key = trap.getName();
		if (!traps.containsKey(key)) {
			traps.put(key, trap);
		}
	}

	public void removeTrap(HW2000Trap trap) {
		trap.done();
		traps.remove(trap.getName());
	}

	private boolean doTraps() {
		for (HW2000Trap trap : traps.values()) {
			if (trap.doTrap()) {
				return true;
			}
		}
		return false;
	}

	public void throttle(boolean throt) {
		throttled = throt;
	}

	public void run() {
		int clk;
		int limit = checkIntv;
		long t0;
		if (fp != null) {
			fp.setRunStop(true);
			// Pause, for aesthetics
			try {
				Thread.sleep(100);
			} catch (Exception ee) {}
		}
		halt = false;
		t0 = u0 = System.nanoTime();
		while (!halt) {
			if (SR == (-1 & am_mask)) {
				trap(); // protect from Exceptions also?
				continue; // might set 'halt', change SR, ...
			}
			try {
				if (doTraps()) { // might set 'halt',...
					continue;
				}
				tracing = (_trace && SR >= _trace_low && SR < _trace_hi);
				protViol = false;
				fetch();
				if (tracing) {
					oAAR = AAR;
					oBAR = BAR;
				}
				clk = execute();
				// NOTE: this does not cover the case of exceptions above
				if (tracing) {
					if (fp != null) {
						fp.setAddress(oSR);
						fp.setContents(opSR);
					}
					traceInstr();
					try { Thread.sleep(1); } catch (Exception ee) {}
				} else if (throttled) { // "real" timing
					limit -= clk;
					if (limit <= 0) {
						long t1 = System.nanoTime();
						long dt = (checkIntv + limit) * nsPerTic;
						long backlog = dt - (t1 - t0);
						if (backlog > 0) try {
							Thread.sleep(backlog / 1000000,
								(int)(backlog % 1000000));
							t1 = System.nanoTime();
						} catch (Exception ee) {}
						limit = checkIntv;
						t0 = t1;
					}
				}
			} catch (HaltException he) {
				// Not fatal, but must cleanup CPU state...
				halt = true; // in case not already set.
				SR = oSR; // out best bet is to repeat instruction
				// TODO: more cleanup required?
			} catch (IIException ie) {
				// TODO: need to handle II within II...
				if (tracing) {
					traceInstr();
				}
				if (IIR != 0 && setIntr(HW2000CCR.EIR_II, ie.type)) {
					// nothing else
				} else {
					if (fp != null) {
						fp.setProgram(true);
						PopupFactory.warning(fp.getContentPane(), "Run II",
							String.format("II %07o: %s",
								oSR,
								ie.toString()));
					} else {
						ie.printStackTrace();
					}
					halt = true;
				}
			} catch (EIException ee) {
				if (tracing) {
					traceInstr();
				}
				if (EIR != 0 && setIntr(HW2000CCR.EIR_EI, ee.type)) {
					// nothing else
				} else {
					if (fp != null) {
						fp.setProgram(true);
						PopupFactory.warning(fp.getContentPane(), "Run EI",
							String.format("EI %07o: %s",
								oSR,
								ee.toString()));
					} else {
						ee.printStackTrace();
					}
					halt = true;
				}
			} catch (FaultException fe) {
				if (fp != null) {
					fp.setProgram(true);
					PopupFactory.warning(fp.getContentPane(), "Run Fault",
						String.format("Fault %07o: %s",
								oSR,
								fe.toString()));
				} else {
					fe.printStackTrace();
				}
				halt = true;
			} catch (Exception e) {
				if (fp != null) {
					fp.setProgram(true);
					//e.printStackTrace();
					PopupFactory.warning(fp.getContentPane(), "Run",
						String.format("<HTML><PRE>%07o: %s</PRE></HTML>",
							oSR,
							e.toString()));
				} else {
					e.printStackTrace();
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
			fp.setAddress(SR);
			try {
				fp.setContents(rawReadMem(SR));
			} catch (Exception ee) {}
		}
	}

	public void listOut(String str) {
		if (fp != null) {
			fp.listOut(str);
		} else {
			System.err.format("%s", str);
		}
	}

	public CharConverter cvt() {
		return pdc.cvt;
	}

	// Honeywell-style memory dump.
	// Range is inclusive, both ends
	public void dumpHW(int beg, int end) {
		String marks = " WIR";
		if (beg >= mem.length) {
			beg = mem.length - 1;
		}
		if (end >= mem.length) {
			end = mem.length - 1;
		}
		int m = beg & ~0177;	// 128 locations per row...
		listOut(String.format("\n\nMemory Dump: %07o - %07o\n", beg, end));
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
