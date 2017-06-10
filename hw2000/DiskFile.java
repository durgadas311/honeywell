// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public interface DiskFile {
	static final byte SEQUENTIAL = 001;
	static final byte DIRECT = 002;
	static final byte INDEXED_SEQ = 003;
	static final byte PART_SEQ = 011;

	static final int IN = 001;
	static final int OUT = 002;
	static final int IN_OUT = (IN | OUT);
	static final int UPDATE = 004;

	// Open done by ctor and caller.
	// All types (except MSPUT on INDEXED_SEQ, DIRECT)
	boolean getItem();
	boolean repItem();
	boolean putItem();
	boolean getItem(CoreMemory itm, int adr);
	boolean repItem(CoreMemory itm, int adr);
	boolean putItem(CoreMemory itm, int adr);
	boolean close();
	// PART_SEQ only
	boolean setMemb(CoreMemory memb, int adr, int mode); // open partition
	boolean endMemb(); // close partition
	boolean alterMemb(CoreMemory memb, int adr, int op, CoreMemory newm, int nadr);
	boolean release(); // re-init entire file
	// Quasi-standard
	int getItemAdr();

	// Non-standard: (Not part of MOD1 i/f)
	void setBuffer(CoreMemory blkBuf, int blkAdr);
	void setDescr(CoreMemory dscBuf, int dscAdr);
	DiskFile dup();	// R/O clone
	byte[] getName();
	int getMode();
	int getType();
	int getError();
	DiskUnit[] getAlloc();
	int itemLen();
	int recordLen();
	int blockLen();
	boolean isEOF();
	boolean rewind();
	boolean sync();
	boolean seek(int cyl, int trk, int rec, int itm);
	boolean seek(int[] ctri);
	int[] getAddress();
}
