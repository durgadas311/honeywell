// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// Partitioned Sequential files can:
//	GET - (IN,IN/OUT) sequential read of member items
//	REP - (IN/OUT) replace of current member item (previous GET)
//	PUT - (OUT) write member items sequentially
//	SETM - open/create a partition/member
//	ENDM - close current partition/member
//	MALTER - modify member
//	MREL - erase all members

public class PartitionedSeqFile extends SequentialFile {
	static final byte[] _UNUSED_ = new byte[]
		{ 054, 064, 045, 064, 062, 025, 024, 054,
				015, 015, 015, 015, 015, 015 };	// *UNUSED*______
	static final byte[] _ENDINDEX_ = new byte[]
		{ 054, 025, 045, 024, 031, 045, 024, 025, 067, 054,
					015, 015, 015, 015 };	// *ENDINDEX*____
	int idxLen;	// number of blocks in index
	int mmbIdxLen;	// length of index item (must be 25?)
	int itmBlk;	// number of items per block
	int totBlks;

	int putItms;	// number of items MSPUT

	// The following are only valid after openMemb() returns 00203
	int[] freeCCTTRR;	// from *UNUSED* item
	int freeBlocks;		// from *UNUSED* item
	int[] freeMember;
	int freeOff;
	int[] foundMember;
	int foundOff;

	// Caller locates *VOLNAMES*, etc items and passes info to this ctor.
	public PartitionedSeqFile(RandomRecordIO dsk, int unit, byte[] name, boolean prot,
			CoreMemory blkBuf, int blkAdr,
			int itmLen, int recLen, int recTrk, int recBlk,
			int idxLen, int mmbIdxLen,
			DiskUnit[] alloc) {
		super(dsk, unit, name, prot, blkBuf, blkAdr,
				itmLen, recLen, recTrk, recBlk, alloc);
		itmBlk = (recLen * recBlk) / itmLen;
		this.idxLen = idxLen;	// number of blocks
		this.mmbIdxLen = mmbIdxLen;	// always 25? at least 25?
		totBlks = totalBlocks();
	}

	// Create a R/O clone of this file. Not used?
	@Override
	public DiskFile dup() {
		PartitionedSeqFile dup = new PartitionedSeqFile(dsk, unit, name, true,
				null, 0, itmLen, recLen, recTrk, recBlk,
				idxLen, mmbIdxLen, units);
		return dup;
	}

	private int totalBlocks() {
		int trks = 0;
		for (int u = 0; u < units.length && units[u] != null; ++u) {
			trks += ((units[u].eCyl - units[u].sCyl + 1) *
				(units[u].eTrk - units[u].sTrk + 1));
		}
		return trks * recTrk;
	}

	private int getNum(CoreMemory buf, int start, int num) {
		int val = 0;
		while (num-- > 0) {
			val <<= 6;
			val |= buf.readChar(start++);
		}
		return val;
	}

	private void putNum(int val, CoreMemory buf, int start, int num) {
		while (--num >= 0) {
			buf.writeChar(start + num, (byte)val);
			val >>= 6;
		}
	}

	private boolean compare(CoreMemory buf, int adr,
				CoreMemory buf2, int adr2, int len) {
		for (int x = 0; x < len; ++x) {
			if (buf.readChar(x + adr) != buf2.readChar(x + adr2)) {
				return false;
			}
		}
		return true;
	}

	private boolean rewindIndex() {
		curCyl = units[0].sCyl;
		curTrk = units[0].sTrk;
		curRec = 0;
		if (!cacheBlock(false, curCyl, curTrk, curRec)) {
			return false;
		}
		return true;
	}

	private boolean openMemb(CoreMemory mmb, int adr) {
		if (!rewindIndex()) {
			return false;
		}
		int sts = blkBufMem.readChar(blkBufAdr + 24);
		// not necessary to compare to _UNUSED_?
		if (sts != 010) {
			error = 00434;	// actually, file not initialized
			return false;
		}
		freeCCTTRR = DiskVolume.getSome(blkBufMem, blkBufAdr + 15, 3);
		freeBlocks = getNum(blkBufMem, blkBufAdr + 21, 3);
		freeMember = null;
		foundMember = null;
		int off = mmbIdxLen;
		int nBlks = 0;
		while (true) {
			while (off < blkLen) {
				sts = blkBufMem.readChar(blkBufAdr + off + 24);
				// not necessary to compare to _ENDINDEX_?
				if (sts == 001) {	// _ENDINDEX_
					return true;	// caller examines foundMember
				}
				if (sts == 040) {	// deleted
					if (freeMember == null) {
						freeMember = getAddress();
						freeOff = off;
					}
					continue;
				}
				if (compare(mmb, adr, blkBufMem, blkBufAdr + off, 14)) {
					foundMember = getAddress();
					foundOff = off;
					return true;
				}
				off += mmbIdxLen;
			}
			// next block...
			if (++nBlks >= idxLen) {
				// actually an error if we get here w/o _ENDINDEX_
				error = 00434;	// a.k.a. file not initialized
				return false;
			}
			curCyl = nxtCyl;
			curTrk = nxtTrk;
			curRec = nxtRec;
			if (!cacheBlock(false, curCyl, curTrk, curRec)) {
				return false;
			}
			off = 0;
		}
		// NOTREACHED
	}

	@Override
	public boolean rewind() { error = 00005; return false; }

	@Override
	public boolean seek(int[] ints) { error = 00005; return false; }

	@Override
	public boolean seek(int c, int t, int r, int i) { error = 00005; return false; }

	//
	// These routines are for the MOVE item delivery mode...
	//
	@Override
	public boolean getItem(CoreMemory itm, int adr) {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		return super.getItem(itm, adr);
	}

	@Override
	public boolean repItem(CoreMemory itm, int adr) {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		return super.repItem(itm, adr);
	}

	@Override
	public boolean putItem(CoreMemory itm, int adr) {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		++putItms;
		return super.putItem(itm, adr);
	}

	@Override
	public boolean close() {
		boolean ok1 = endMemb();
		boolean ok2 = sync();
		return (ok1 && ok2);
	}

	@Override
	public int getType() { return DiskFile.PART_SEQ; }

	@Override
	public boolean setMemb(CoreMemory memb, int adr, int mode) {
		// Open or create member.
		endMemb(); // probably an error...
		boolean ok = openMemb(memb, adr);
		if (!ok) {
			return false;
		}
		// output-only requires member status 020...
		byte req = (byte)(mode == 2 ? 020 : 000);
		byte sts = (byte)020;	// default for new members
		int[] beg = null;
		if (foundMember == null) {
			// no match found
			if (freeMember == null) {
				error = 00204;	// no space in index
				return false;
			}
			beg = freeCCTTRR;
			foundMember = freeMember;
			foundOff = freeOff;
			super.seek(foundMember);
			blkBufMem.copyIn(blkBufAdr + foundOff, memb, adr, 14);
			blkBufMem.writeChar(blkBufAdr + foundOff + 14, (byte)015);
			DiskVolume.putSome(beg, blkBufMem, blkBufAdr + foundOff + 15);
			putNum(0, blkBufMem, blkBufAdr + foundOff + 21, 3);
			blkBufMem.writeChar(blkBufAdr + foundOff + 24, sts);
			dirty = true;
		} else {
			// blkBufMem has the index item at foundOff...
			sts = blkBufMem.readChar(blkBufAdr + foundOff + 24);
			if (req > sts) {
				error = 00214;
				return false;
			}
			beg = DiskVolume.getSome(blkBufMem, blkBufAdr + 15, 3);
		}
		if (!super.seek(beg[0], beg[1], beg[2], 0)) {
			return false;
		}
		putItms = 0;
		return true;
	}

	@Override
	public boolean endMemb() {
		// Must update member index for blocks used,
		// for both this member and the *UNUSED* member.
		// Also, terminate member data with [EOD^.
		// Also must tolerate being called when no active member.
		boolean ok = true;
		if (foundMember == null) {
			return ok;
		}
		// this should add EOD if MSPUT...
		super.close();	// must not be terminal
		if (put) {
			int putBlks = (putItms + itmBlk - 1) / itmBlk;
			int[] end = new int[]{ nxtCyl, nxtTrk, nxtRec };
			super.seek(foundMember);
			putNum(putBlks, blkBufMem, blkBufAdr + foundOff + 21, 3);
			dirty = true;
			ok = rewindIndex();
			if (ok) {
				int n = getNum(blkBufMem, blkBufAdr + 21, 3);
				n -= putBlks;
				if (n < 0) n = 0;
				putNum(n, blkBufMem, blkBufAdr + 21, 3);
				DiskVolume.putSome(end, blkBufMem, blkBufAdr + 15);
				dirty = true;
				ok = sync();
			}
		}
		put = false;
		return ok;
	}

	@Override
	public boolean alterMemb(CoreMemory memb, int adr, int op,
				CoreMemory newm, int nadr) {
		error = 00434;
		return false;
	}

	@Override
	public boolean release() {
		// This must handle the case of a new file,
		// i.e. no existing *ENDINDEX* member.
		endMemb(); // probably an error...
		if (!rewindIndex()) {
			return false;
		}
		int off = mmbIdxLen;
		int nBlks = 0;
		while (true) {
			while (off < blkLen) {
				blkBufMem.writeChar(blkBufAdr + off + 24, (byte)040);
				dirty = true;
				off += mmbIdxLen;
			}
			if (++nBlks >= idxLen) {
				break;
			}
			curCyl = nxtCyl;
			curTrk = nxtTrk;
			curRec = nxtRec;
			if (!cacheBlock(false, curCyl, curTrk, curRec)) {
				return false;
			}
			off = 0;
		}
		// back up one and set _ENDINDEX_...
		// also save "data start" address...
		off -= mmbIdxLen;
		int[] dat = new int[]{ nxtCyl, nxtTrk, nxtRec };
		blkBufMem.copyIn(blkBufAdr + off, _ENDINDEX_, 0, 14);
		blkBufMem.writeChar(blkBufAdr + off + 14, (byte)015);
		DiskVolume.putSome(dat, blkBufMem, blkBufAdr + off + 15);
		putNum(totBlks - idxLen, blkBufMem, blkBufAdr + off + 21, 3);
		blkBufMem.writeChar(blkBufAdr + off + 24, (byte)001);
		dirty = true;
		if (!rewindIndex()) {
			return false;
		}
		blkBufMem.copyIn(blkBufAdr, _UNUSED_, 0, 14);
		blkBufMem.writeChar(blkBufAdr + 14, (byte)015);
		DiskVolume.putSome(dat, blkBufMem, blkBufAdr + 15);
		putNum(totBlks - idxLen, blkBufMem, blkBufAdr + 21, 3);
		blkBufMem.writeChar(blkBufAdr + 24, (byte)010);
		dirty = true;
		if (!sync()) {
			return false;
		}
		return true;
	}
}
