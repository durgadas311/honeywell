// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public interface CoreMemory {
	byte readMem(int adr);
	byte readChar(int adr);
	byte rawReadMem(int adr);
	void rawWriteMem(int adr, byte val);
	byte rawWriteChar(int adr, byte val);	// returns puntuation bits
	void writeMem(int adr, byte val);
	void writeChar(int adr, byte val);
	void setWord(int adr);
	void setItem(int adr);
	void clrWord(int adr);
	void clrItem(int adr);
	void copyIn(int adr, byte[] buf, int start, int len);
	void copyOut(int adr, byte[] buf, int start, int len);
	void copyIn(int adr, CoreMemory buf, int start, int len);
	void copyOut(int adr, CoreMemory buf, int start, int len);
	boolean compare(int adr, CoreMemory buf, int start, int len);
	boolean compare(int adr, byte[] buf, int start, int len);
	void zero(int adr, int len);
	int size();

	// Not specifically memory-related
	void listOut(String str);
	void addTrap(HW2000Trap trap);
	void removeTrap(HW2000Trap trap);
	CharConverter cvt();
}
