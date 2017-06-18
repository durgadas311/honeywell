// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Arrays;

public class BufferMemory implements CoreMemory
{
	private byte[] mem;

	public BufferMemory(int len) {
		mem = new byte[len];
	}

	public BufferMemory(byte[] mem) {
		this.mem = mem;
	}

	public byte rawReadMem(int adr) {
		return mem[adr];
	}

	public void rawWriteMem(int adr, byte val) {
		mem[adr] = val;
	}

	// returns puntuation bits
	public byte rawWriteChar(int adr, byte val) {
		mem[adr] = (byte)((mem[adr] & 0300) | (val & 077));
		return mem[adr];
	}

	public byte readMem(int adr) {
		if (adr >= mem.length) {
			return (byte)0300; // try to stop caller
		}
		return mem[adr];
	}

	public byte readChar(int adr) {
		return (byte)(readMem(adr) & 077);
	}

	public void writeMem(int adr, byte val) {
		if (adr >= mem.length) {
			return;
		}
		mem[adr] = val;
	}

	// 'mask' is bits to preserve from current mem value
	public byte writeMemMask(int adr, byte val, byte mask) {
		if (adr >= mem.length) {
			return (byte)0300;
		}
		mem[adr] = (byte)((mem[adr] & mask) | (val & ~mask));
		return mem[adr];
	}

	public void writeChar(int adr, byte val) {
		writeMemMask(adr, val, (byte)0300);
	}

	// These do not suffer accounting timer tics?
	public boolean chkWord(int adr) {
		if (adr >= mem.length) {
			return true;
		}
		return (mem[adr] & 0100) != 0;
	}

	public boolean chkItem(int adr) {
		if (adr >= mem.length) {
			return true;
		}
		return (mem[adr] & 0200) != 0;
	}

	public void setWord(int adr) {
		if (adr >= mem.length) {
			return;
		}
		mem[adr] |= 0100;
	}

	public void setItem(int adr) {
		if (adr >= mem.length) {
			return;
		}
		mem[adr] |= 0200;
	}

	public void clrWord(int adr) {
		if (adr >= mem.length) {
			return;
		}
		mem[adr] &= ~0100;
	}

	public void clrItem(int adr) {
		if (adr >= mem.length) {
			return;
		}
		mem[adr] &= ~0200;
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
		if (adr >= mem.length) {
			return;
		}
		if (len < 0) {
			len = mem.length - adr;
		}
		int end = adr + len;
		if (end > mem.length) {
			end = mem.length;
		}
		Arrays.fill(mem, adr, end, (byte)0);
	}

	public int size() {
		return mem.length;
	}


	// The rest are not implemented
	public void addTrap(HW2000Trap trap) {}
	public void removeTrap(HW2000Trap trap) {}

	public void listOut(String str) {
		System.err.format("%s", str);
	}
	public CharConverter cvt() {
		return null;
	}
}
