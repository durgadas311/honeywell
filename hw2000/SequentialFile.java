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
	int itmLen;
	int recLen;
	int blkLen;
	int recBlk;
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
	byte[] blkBuf;
	CoreMemory blkBufMem;
	CoreMemory tlrBuf;
	boolean dirty;
	String error = null;

	// Special-case for *VOLNAMES*, etc.
	public SequentialFile(RandomRecordIO dsk, int unit, byte[] name,
			int itmLen, int recLen, int recTrk, int recblk,
			int sCyl, int sTrk, int eCyl, int eTrk) {
		// TODO: do we need recTrk?
		this.dsk = dsk;
		this.unit = unit;
		this.name = name;
		this.itmLen = itmLen;
		this.recLen = recLen;
		this.recBlk = recblk;
		this.blkLen = recLen * recblk;
		units = new DiskUnit[6];
		Arrays.fill(units, null);
		units[0] = new DiskUnit(sCyl, sTrk, eCyl, eTrk);
		lastUnit = 0;
		// TODO: use this buf even when program supplied buf?
		blkBuf = new byte[blkLen];
		blkBufMem = new BufferMemory(blkBuf);
		tlrBuf = new BufferMemory(6);
		curOff = -1;
		blkCyl = -1;
		blkTrk = -1;
		blkRec = -1;
	}

	// General Open of "real" file.
	// Caller locates *VOLNAMES* item and passes to this ctor.
	public SequentialFile(RandomRecordIO dsk, int unit, byte[] volname,
			int itmLen, int recLen, int recTrk, int recblk,
			DiskUnit[] alloc) {
		this.dsk = dsk;
		this.unit = unit;
		name = new byte[10];
		System.arraycopy(volname, 0, name, 0, 10);
		units = alloc;
		for (int u = 0; u < 6 && u < units.length && units[u] != null; ++u) {
			lastUnit = u;
		}
		this.itmLen = itmLen;
		this.recLen = recLen;
		this.recBlk = recblk;
		this.blkLen = recLen * recblk;
		blkBuf = new byte[blkLen];
		blkBufMem = new BufferMemory(blkBuf);
		tlrBuf = new BufferMemory(6);
		curOff = -1;
		blkCyl = -1;
		blkTrk = -1;
		blkRec = -1;
	}

	public String getError() {
		return error;
	}

	public byte[] getName() {
		return name;
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
		return true;
	}
	public boolean sync() {
		if (!cacheBlock(-1, -1, -1)) {
			return false;
		}
		return true;
	}
	public boolean seek(int[] ctri) {
		if (ctri.length != 4) {
			error = "Internal error";
			return false;
		}
		return seek(ctri[0], ctri[1], ctri[2], ctri[3]);
	}

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

	// called when 'rec' is a TLR and about to follow it.
	private boolean lastRecord(int cyl, int trk, int rec) {
		return (units[lastUnit].eCyl == cyl && units[lastUnit].eTrk == trk);
	}

	// TODO: must allow for last record on different track from first...
	// and must update our cyl,trk,rec accordingly...
	// also must remember ending cyl,trk,rec for "increment"...
	// So, really can't ignore TLRs...
	// TODO: block cannot span allocation units (cylinders)?
	private boolean readBlock(int cyl, int trk, int rec, CoreMemory buf) {
		if (!dsk.begin(unit)) {
			error = dsk.getError();
			return false;
		}
		try {
			int off = 0;
			for (int r = 0; r < recBlk; ++r) {
				int f = dsk.seekRecord(cyl, trk, rec);
				if (f < 0) {
					error = dsk.getError();
					return false;
				}
				if ((f & P_Disk.HDR_TLR) != 0) {
					if (lastRecord(cyl, trk, rec)) {
						// User error: ran off end
						error = "File overrun";
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

	private boolean writeBlock(int cyl, int trk, int rec, CoreMemory buf) {
		if (!dsk.begin(unit)) {
			error = dsk.getError();
			return false;
		}
		try {
			int off = 0;
			for (int r = 0; r < recBlk; ++r) {
				int f = dsk.seekRecord(cyl, trk, rec);
				if (f < 0) {
					error = dsk.getError();
					return false;
				}
				if ((f & P_Disk.HDR_TLR) != 0) {
					if (lastRecord(cyl, trk, rec)) {
						// User error: ran off end
						error = "File overrun";
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
			ok = writeBlock(blkCyl, blkTrk, blkRec, blkBufMem);
		}
		dirty = false;
		if (cyl < 0 || trk < 0 || rec < 0) {
			return ok;
		}
		if (!readBlock(cyl, trk, rec, blkBufMem)) {
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
		itm.copyIn(adr, blkBuf, curOff, itmLen);
		return true;
	}

	public boolean repItem(CoreMemory itm, int adr) {
		if (curOff < 0) {
			return false;
		}
		itm.copyOut(adr, blkBuf, curOff, itmLen);
		dirty = true;
		return true;
	}

	public boolean putItem(CoreMemory itm, int adr) {
		// Techically, we don't need to read block first,
		// But this way we catch physical end of file...
		if (!cacheNextItem()) {
			return false;
		}
		itm.copyOut(adr, blkBuf, curOff, itmLen);
		dirty = true;
		return true;
	}

	public boolean close() {
		// TODO: add _EOD_ if OUTPUT-only and written...
		// or, does caller do that?
		sync();
		rewind();
		return true;
	}
}
