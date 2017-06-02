// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

// Sequential files can:
//	GET - (IN,IN/OUT) sequential read of items
//	REP - (IN/OUT) replace of current item (previous GET)
//	PUT - (OUT) write items sequentially

public class SequentialFile implements DiskFile {
	RandomRecordIO dsk = null;
	int unit;
	byte[] name;
	boolean prot; // R/O
	int itmLen;
	int recLen;
	int blkLen;
	int recBlk;
	int recTrk;
	DiskUnit[] units;
	int lastUnit;
	int relRec;
	int curCyl;
	int curTrk;
	int curRec;
	int curOff;
	int curItm;
	int nxtCyl;
	int nxtTrk;
	int nxtRec;
	int blkCyl;
	int blkTrk;
	int blkRec;
	CoreMemory blkBufMem;
	int blkBufAdr;
	CoreMemory tlrBuf;
	boolean dirty;
	boolean put;
	boolean eof;
	int error = 0;

	// Special-case for *VOLNAMES*, etc.
	public SequentialFile(RandomRecordIO dsk, int unit, byte[] name, boolean prot,
			int itmLen, int recLen, int recTrk, int recBlk,
			int sCyl, int sTrk, int eCyl, int eTrk) {
		init(dsk, unit, name, prot, itmLen, recLen, recTrk, recBlk);
		units = new DiskUnit[6];
		Arrays.fill(units, null);
		units[0] = new DiskUnit(sCyl, sTrk, eCyl, eTrk);
		lastUnit = 0;
		blkBufMem = new BufferMemory(blkLen);
		blkBufAdr = 0;
	}

	// General Open of "real" file.
	// Caller locates *VOLNAMES*, etc items and passes info to this ctor.
	public SequentialFile(RandomRecordIO dsk, int unit, byte[] name, boolean prot,
			CoreMemory blkBuf, int blkAdr,
			int itmLen, int recLen, int recTrk, int recBlk,
			DiskUnit[] alloc) {
		init(dsk, unit, name, prot, itmLen, recLen, recTrk, recBlk);
		units = alloc;
		for (int u = 0; u < 6 && u < units.length && units[u] != null; ++u) {
			lastUnit = u;
		}
		if (blkBuf == null) {
			blkBufMem = new BufferMemory(blkLen);
			blkBufAdr = 0;
		} else {
			blkBufMem = blkBuf;
			blkBufAdr = blkAdr;
		}
	}

	private void init(RandomRecordIO dsk, int unit, byte[] name, boolean prot,
			int itmLen, int recLen, int recTrk, int recBlk) {
		// TODO: do we need recTrk?
		this.dsk = dsk;
		this.unit = unit;
		this.name = name;
		this.prot = prot;
		this.itmLen = itmLen;
		this.recLen = recLen;
		this.recBlk = recBlk;
		this.recTrk = recTrk;
		this.blkLen = recLen * recBlk;
		tlrBuf = new BufferMemory(6);
		curOff = -1;
		blkCyl = -1;
		blkTrk = -1;
		blkRec = -1;
		put = false;
		eof = false;
	}

	// Create a R/O clone of this file.
	public DiskFile dup() {
		SequentialFile dup = new SequentialFile(dsk, unit, name, true,
				null, 0, itmLen, recLen, recTrk, recBlk,
				units);
		return dup;
	}

	public int getError() {
		return error;
	}

	public byte[] getName() {
		return name;
	}

	public DiskUnit[] getAlloc() {
		return units;
	}

	public int itemLen() {
		return itmLen;
	}

	public int recordLen() {
		return recLen;
	}

	public int blockLen() {
		return blkLen;
	}

	// This is NOT an allowed MOD1 action.
	// This will be used by Volume file search.
	public boolean rewind() {
		// This only works if no item is size 1.
		curOff = -1;
		put = false;
		eof = false;
		return true;
	}
	public boolean sync() {
		if (!cacheBlock(-1, -1, -1)) {
			return false;
		}
		return true;
	}
	// MUST be followed by getItem or putItem, NOT repItem!
	public boolean seek(int[] ctri) {
		if (ctri.length != 4) {
			error = 00001;
			return false;
		}
		return seek(ctri[0], ctri[1], ctri[2], ctri[3]);
	}

	// MUST be followed by getItem or putItem, NOT repItem!
	public boolean seek(int cyl, int trk, int rec, int itm) {
		// 'rec' is start of block (MSR Data Management, pg 3-54)
		// TODO: validate against units[*], itmBlk, etc?
		if (!cacheBlock(cyl, trk, rec)) {
			return false;
		}
		curCyl = cyl;
		curTrk = trk;
		curRec = rec;
		// must be different than curOff = -1 when itm == 0
		curOff = (itm - 1) * itmLen;
		put = false;	// only if backward seek?
		eof = false;	// only if backward seek?
		return true;
	}

	// Return CCTTRRII of current item.
	public int[] getAddress() {
		int[] ints = new int[4];
		ints[0] = curCyl;
		ints[1] = curTrk;
		ints[2] = curRec;
		ints[3] = curOff / itmLen;
		return ints;
	}

	public boolean isEOF() { return eof; }

	// called when 'rec' is a TLR and about to follow it.
	private boolean lastRecord(int cyl, int trk, int rec) {
		return (units[lastUnit].eCyl == cyl && units[lastUnit].eTrk == trk);
	}

	// TODO: must allow for last record on different track from first...
	// and must update our cyl,trk,rec accordingly...
	// also must remember ending cyl,trk,rec for "increment"...
	// So, really can't ignore TLRs...
	// TODO: block cannot span allocation units (cylinders)?
	private boolean readBlock(int cyl, int trk, int rec,
				CoreMemory buf, int adr) {
		if (!dsk.begin(unit)) {
			error = dsk.getError();
			return false;
		}
		try {
			int off = adr;
			for (int r = 0; r < recBlk; ++r) {
				int f = dsk.seekRecord(cyl, trk, rec);
				if (f < 0) {
					error = dsk.getError();
					return false;
				}
				if ((f & P_Disk.HDR_TLR) != 0) {
					if (lastRecord(cyl, trk, rec)) {
						// User error: ran off end
						error = 00401;	// File overrun
						return false;
					}
					if (!dsk.readRecord(tlrBuf, 0, -1)) {
						error = dsk.getError();
						return false;
					}
					cyl = tlrBuf.readChar(0) << 6;
					cyl |= tlrBuf.readChar(1);
					trk = tlrBuf.readChar(2) << 6;
					trk |= tlrBuf.readChar(3);
					rec = tlrBuf.readChar(4) << 6;
					rec |= tlrBuf.readChar(5);
					f = dsk.seekRecord(cyl, trk, rec);
					if (f < 0) {
						error = dsk.getError();
						return false;
					}
				}
				if (!dsk.readRecord(buf, off, off + recLen)) {
					error = dsk.getError();
					return false;
				}
				++rec;
				off += recLen;
			}
		} finally {
			dsk.end();
		}
		// save cyl/trk/rec for "increment"...
		// These should always point to an existing record,
		// although might be a TLR. We never care about units[*]
		// except at rewind/initial record and running off the end.
		// But, detecting running off the end requires we check
		// units[*], or at least know the last track of the last unit.
		nxtCyl = cyl;
		nxtTrk = trk;
		nxtRec = rec;
		return true;
	}

	private boolean writeBlock(int cyl, int trk, int rec,
				CoreMemory buf, int adr) {
		if (!dsk.begin(unit)) {
			error = dsk.getError();
			return false;
		}
		try {
			int off = adr;
			for (int r = 0; r < recBlk; ++r) {
				int f = dsk.seekRecord(cyl, trk, rec);
				if (f < 0) {
					error = dsk.getError();
					return false;
				}
				if ((f & P_Disk.HDR_TLR) != 0) {
					if (lastRecord(cyl, trk, rec)) {
						// User error: ran off end
						error = 00411;
						return false;
					}
					if (!dsk.readRecord(tlrBuf, 0, -1)) {
						error = dsk.getError();
						return false;
					}
					cyl = tlrBuf.readChar(0) << 6;
					cyl |= tlrBuf.readChar(1);
					trk = tlrBuf.readChar(2) << 6;
					trk |= tlrBuf.readChar(3);
					rec = tlrBuf.readChar(4) << 6;
					rec |= tlrBuf.readChar(5);
					f = dsk.seekRecord(cyl, trk, rec);
					if (f < 0) {
						error = dsk.getError();
						return false;
					}
				}
				if (!dsk.writeRecord(buf, off, off + recLen)) {
					error = dsk.getError();
					return false;
				}
				++rec;
				off += recLen;
			}
		} finally {
			dsk.end();
		}
		// save cyl/trk/rec for "increment"...
		// this must always be the same as readBlock()!
		nxtCyl = cyl;
		nxtTrk = trk;
		nxtRec = rec;
		return true;
	}

	// 'rec' must be the first record of a block...
	private boolean cacheBlock(int cyl, int trk, int rec) {
		boolean ok = true;
		if (blkCyl >= 0 && blkTrk >= 0 && blkRec >= 0 &&
				blkCyl == cyl && blkTrk == trk && blkRec == rec) {
			return true;
		}
		if (dirty && blkCyl >= 0 && blkTrk >= 0 && blkRec >= 0) {
			// TODO: preserve error? PERMIT errors need to be reported
			ok = writeBlock(blkCyl, blkTrk, blkRec, blkBufMem, blkBufAdr);
		}
		dirty = false;
		if (cyl < 0 || trk < 0 || rec < 0) {
			return ok;
		}
		if (!readBlock(cyl, trk, rec, blkBufMem, blkBufAdr)) {
			return false;
		}
		blkCyl = cyl;
		blkTrk = trk;
		blkRec = rec;
		return ok;
	}

	private boolean cacheNextItem() {
		// This only works if no item is size 1.
		if (curOff == -1) { // initial access (or after rewind)
			curOff = 0;
			curCyl = units[0].sCyl;
			curTrk = units[0].sTrk;
			curRec = 0;
		} else {
			curOff += itmLen;
			if (curOff >= blkLen) {
				curOff = 0;
				curCyl = nxtCyl;
				curTrk = nxtTrk;
				curRec = nxtRec;
				// These might point to TLR, which is
				// where we detect the next allocation unit.
			}
		}
		if (curOff == 0) {	// must fetch new block.
			if (!cacheBlock(curCyl, curTrk, curRec)) {
				return false;
			}
		}
		return true;
	}

	//
	// These routines are for the MOVE item delivery mode...
	//
	public boolean getItem(CoreMemory itm, int adr) {
		if (!cacheNextItem()) {
			return false;
		}
		if (DiskVolume.isEOD(blkBufMem, blkBufAdr + curOff)) {
			eof = true;
			error = 00401;	// End of File
			return false;
		}
		itm.copyIn(adr, blkBufMem, blkBufAdr + curOff, itmLen);
		return true;
	}

	public boolean repItem(CoreMemory itm, int adr) {
		if (prot) {
			error = 00502; // Not really device protection...
			return false;
		}
		if (curOff < 0 || eof) {
			return false;
		}
		itm.copyOut(adr, blkBufMem, blkBufAdr + curOff, itmLen);
		dirty = true;
		return true;
	}

	public boolean putItem(CoreMemory itm, int adr) {
		if (prot) {
			error = 00502; // Not really device protection...
			return false;
		}
		// Techically, we don't need to read block first,
		// But this way we catch physical end of file...
		if (!cacheNextItem()) {
			return false;
		}
		itm.copyOut(adr, blkBufMem, blkBufAdr + curOff, itmLen);
		dirty = true;
		put = true;
		eof = false;	// right?
		return true;
	}

	public boolean close() {
		boolean ret = true;
		// TODO: partitioned sequential places EOD in each partition.
		if (put) {
			if (!cacheNextItem()) {
				ret = false;
			} else {
				DiskVolume.setEOD(blkBufMem, blkBufAdr + curOff);
				dirty = true;
			}
		}
		if (!sync()) {
			ret = false;
		}
		rewind(); // not needed?
		return ret;
	}
}
