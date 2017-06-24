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
	int mode;
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
	int numBlks;
	CoreMemory blkBufMem;
	int blkBufAdr;
	CoreMemory tlrBuf;
	boolean dirty;
	boolean inPut;
	boolean put;
	boolean eof;
	boolean initial;
	int error = 0;

	// Special-case for *VOLNAMES*, etc.
	public SequentialFile(RandomRecordIO dsk, int unit, byte[] name, int mode,
			int itmLen, int recLen, int recTrk, int recBlk,
			int sCyl, int sTrk, int eCyl, int eTrk) {
		init(dsk, unit, name, mode, itmLen, recLen, recTrk, recBlk);
		units = new DiskUnit[6];
		Arrays.fill(units, null);
		units[0] = new DiskUnit(sCyl, sTrk, eCyl, eTrk);
		lastUnit = 0;
		setBuffer(new BufferMemory(blkLen), 0);
		computeSize();
		_rewind();
	}

	// General Open of "real" file.
	// Caller locates *VOLNAMES*, etc items and passes info to this ctor.
	public SequentialFile(RandomRecordIO dsk, int unit, byte[] name, int mode,
			CoreMemory blkBuf, int blkAdr,
			int itmLen, int recLen, int recTrk, int recBlk,
			DiskUnit[] alloc) {
		init(dsk, unit, name, mode, itmLen, recLen, recTrk, recBlk);
		units = alloc;
		for (int u = 0; u < 6 && u < units.length && units[u] != null; ++u) {
			lastUnit = u;
		}
		if (blkBuf == null) {
			setBuffer(new BufferMemory(blkLen), 0);
		} else {
			setBuffer(blkBuf, blkAdr);
		}
		computeSize();
		_rewind();
	}

	public void setBuffer(CoreMemory blkBuf, int blkAdr) {
		// TODO: if I/O has begin, this can be bad...
		blkBufMem = blkBuf;
		blkBufAdr = blkAdr;
	}

	protected void init(RandomRecordIO dsk, int unit, byte[] name, int mode,
			int itmLen, int recLen, int recTrk, int recBlk) {
		// TODO: do we need recTrk?
		inPut = false;
		this.dsk = dsk;
		this.unit = unit;
		this.name = name;
		this.mode = mode;
		this.itmLen = itmLen;
		this.recLen = recLen;
		this.recBlk = recBlk;
		this.recTrk = recTrk;
		this.blkLen = recLen * recBlk;
		tlrBuf = new BufferMemory(6);
		initial = true;
		curOff = -1;
		blkCyl = -1;
		blkTrk = -1;
		blkRec = -1;
		put = false;
		eof = false;
	}

	private void computeSize() {
		int trks = 0;
		for (int a = 0; a <= lastUnit; ++a) {
			trks += units[a].size();
		}
		numBlks = ((trks * recTrk) / recBlk);
	}

	public int getMode() { return mode; }

	// Setup a "fake" *VOLDESCR* item based on this file
	public void setDescr(CoreMemory dscBuf, int dscAdr) {
		dscBuf.zero(dscAdr + 0, DiskVolume.descrItmLen); // clean slate
		dscBuf.rawWriteMem(dscAdr + 0, (byte)001); // type = Sequential
		DiskVolume.putOne(itmLen, dscBuf, dscAdr + 1);
		DiskVolume.putOne(recLen, dscBuf, dscAdr + 3);
		DiskVolume.putOne(blkLen / itmLen, dscBuf, dscAdr + 5);
		DiskVolume.putOne(recBlk, dscBuf, dscAdr + 7);
		DiskVolume.putOne(recTrk, dscBuf, dscAdr + 9);
		DiskVolume.putNum(numBlks, dscBuf, dscAdr + 65, 3);
		// TODO: populate other fields... datestamps???
	}

	// Create a R/O clone of this file.
	public DiskFile dup() {
		SequentialFile dup = new SequentialFile(dsk, unit, name, DiskFile.IN,
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

	// Return the address/offset of current item in block buffer
	public int getItemAdr() {
		return blkBufAdr + curOff;
	}

	// This is NOT an allowed MOD1 action.
	// This will be used by Volume file search.
	public boolean rewind() {
		return _rewind();
	}

	public boolean sync() {
		if (!cacheBlock(put, -1, -1, -1)) {
			return false;
		}
		return true;
	}
	// MUST be followed by getItem or putItem, NOT repItem!
	public boolean seek(int[] ctri) {
		if (ctri.length != 4) {
			error = 00503;
			return false;
		}
		return _seek(ctri[0], ctri[1], ctri[2], ctri[3]);
	}

	// MUST be followed by getItem or putItem, NOT repItem!
	public boolean seek(int cyl, int trk, int rec, int itm) {
		return _seek(cyl, trk, rec, itm);
	}

	private boolean _rewind() {
		return _seek(units[0].sCyl, units[0].sTrk, 0, 0);
	}

	private boolean _seek(int cyl, int trk, int rec, int itm) {
		// 'rec' is start of block (MSR Data Management, pg 3-54)
		// TODO: validate against units[*], itmBlk, etc?
		if (!cacheBlock(put, cyl, trk, rec)) {
			return false;
		}
		curCyl = cyl;
		curTrk = trk;
		curRec = rec;
		curOff = itm * itmLen;
		put = false;	// only if backward seek?
		eof = false;	// only if backward seek?
		initial = true;	// do not incr on GET...
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
	protected boolean lastRecord(int cyl, int trk, int rec) {
		return (units[lastUnit].eCyl == cyl && units[lastUnit].eTrk == trk);
	}

	// TODO: must allow for last record on different track from first...
	// and must update our cyl,trk,rec accordingly...
	// also must remember ending cyl,trk,rec for "increment"...
	// So, really can't ignore TLRs...
	// TODO: block cannot span allocation units (cylinders)?
	protected boolean readBlock(boolean inPut, int cyl, int trk, int rec,
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
						error = inPut ? 00411 : 00401;
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

	protected boolean writeBlock(boolean inPut, int cyl, int trk, int rec,
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
				// We should never encounter this here,
				// Only in readBlock() (when filling the cache).
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
	protected boolean cacheBlock(boolean inPut, int cyl, int trk, int rec) {
		boolean ok = true;
		if (blkCyl >= 0 && blkTrk >= 0 && blkRec >= 0 &&
				blkCyl == cyl && blkTrk == trk && blkRec == rec) {
			return true;
		}
		if (dirty && blkCyl >= 0 && blkTrk >= 0 && blkRec >= 0) {
			// TODO: preserve error? PERMIT errors need to be reported
			ok = writeBlock(inPut, blkCyl, blkTrk, blkRec, blkBufMem, blkBufAdr);
		}
		dirty = false;
		if (cyl < 0 || trk < 0 || rec < 0) {
			return ok;
		}
		if (!readBlock(inPut, cyl, trk, rec, blkBufMem, blkBufAdr)) {
			return false;
		}
		blkCyl = cyl;
		blkTrk = trk;
		blkRec = rec;
		return ok;
	}

	// This is only used for MSGET/MSPUT. Direct users of this object
	// must position with seek(), which always reads block.
	protected boolean cacheNextItem(boolean inPut) {
		// This only works if no item is size 1.
		if (curOff == -1) { // initial access (or after rewind)
			curOff = 0;
			curCyl = units[0].sCyl;
			curTrk = units[0].sTrk;
			curRec = 0;
		} else if (inPut || !initial) {
			curOff += itmLen;
			if (curOff + itmLen > blkLen) {
				curOff = 0;
				curCyl = nxtCyl;
				curTrk = nxtTrk;
				curRec = nxtRec;
				// These might point to TLR, which is
				// where we detect the next allocation unit.
			}
		}
		initial = false;
		if (curOff == 0) {	// must fetch new block.
			if (!cacheBlock(inPut, curCyl, curTrk, curRec)) {
				return false;
			}
		}
		return true;
	}

	//
	// These routines are for the LOCATE item delivery mode...
	// Also used by MOVE delivery mode.
	//
	public boolean getItem() {
		return _getItem();
	}

	private boolean _getItem() {
		// Semantic protection (i.e. enforce IN) done by MIOC
		if (!cacheNextItem(false)) {
			return false;
		}
		if (DiskVolume.isEOD(blkBufMem, blkBufAdr + curOff)) {
			eof = true;
			error = 00401;	// End of File
			return false;
		}
		return true;
	}

	public boolean repItem() {
		// Semantic protection (i.e. enforce IN/OUT) done by MIOC
		if (curOff < 0 || eof) {
			error = 00401;
			return false;
		}
		dirty = true;
		return true;
	}

	// Caller's item has *already* been moved into blkBuf...
	// Register update and go to *next* item.
	// This requires that MSOPEN/SETM be fully prepared for MSPUT
	// *and* computed item address!
	public boolean putItem() {
		return _putItem();
	}

	private boolean _putItem() {
		dirty = true;
		put = true;
		eof = false;	// right?
		if (!cacheNextItem(true)) {
			return false;
		}
		return true;
	}

	//
	// These routines are for the MOVE item delivery mode...
	//
	public boolean getItem(CoreMemory itm, int adr) {
		// Semantic protection (i.e. enforce IN) done by MIOC
		if (!_getItem()) {
			return false;
		}
		itm.copyIn(adr, blkBufMem, blkBufAdr + curOff, itmLen);
		return true;
	}

	public boolean repItem(CoreMemory itm, int adr) {
		// Semantic protection (i.e. enforce IN/OUT) done by MIOC
		if (curOff < 0 || eof) {
			error = 00401;
			return false;
		}
		itm.copyOut(adr, blkBufMem, blkBufAdr + curOff, itmLen);
		dirty = true;
		return true;
	}

	// This routine must parallel the LOCATE item delivery mode version.
	// move new item, flush buf, point to next item.
	// MSOPEN/SETM must have prepared the first item!
	// TODO: must look-ahead and prevent PUT in location
	// reserved for _EOD_.
	public boolean putItem(CoreMemory itm, int adr) {
		// Semantic protection (i.e. enforce OUT) done by MIOC
		itm.copyOut(adr, blkBufMem, blkBufAdr + curOff, itmLen);
		if (!_putItem()) {
			return false;
		}
		return true;
	}

	public boolean close() {
		boolean ret = true;
		// TODO: partitioned sequential places EOD in each partition.
		if (put) {
			DiskVolume.setEOD(blkBufMem, blkBufAdr + curOff);
			dirty = true;
		}
		if (!sync()) {
			ret = false;
		}
		// must leave address pointing to last item
		return ret;
	}

	public int getType() { return DiskFile.SEQUENTIAL; }

	public boolean setMemb(CoreMemory memb, int adr, int mode) {
		error = 00005;
		return false;
	}

	public boolean endMemb() {
		error = 00005;
		return false;
	}

	public boolean alterMemb(CoreMemory memb, int adr, int op,
				CoreMemory newm, int nadr) {
		error = 00005;
		return false;
	}

	// NOTE: this is not part of the Sequential File protocol,
	// but is used to hide initialization code.
	public boolean release() {
		// TODO: creation time/number?
		if (!_rewind()) {
			return false;
		}
		DiskVolume.setEOD(blkBufMem, blkBufAdr);
		dirty = true;
		int recXXX = (numBlks * recBlk - recBlk) % recTrk;
		if (!_seek(units[lastUnit].eCyl, units[lastUnit].eTrk,
					recXXX, (blkLen / itmLen) - 1)) {
			return false;
		}
		DiskVolume.setEOD(blkBufMem, blkBufAdr + curOff);
		dirty = true;
		if (!sync()) {
			return false;
		}
		return true;
	}
}
