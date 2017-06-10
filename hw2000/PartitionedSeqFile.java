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
	static final byte _BEG_ = (byte)010;
	static final byte _END_ = (byte)001;
	static final byte _ALL_ = (byte)020;
	static final byte _PRT_ = (byte)000;
	static final byte _DEL_ = (byte)040;
	static final byte[] _UNUSED_ = new byte[]
		{ 054, 064, 045, 064, 062, 025, 024, 054,
				015, 015, 015, 015, 015, 015 };	// *UNUSED*______
	static final byte[] _ENDINDEX_ = new byte[]
		{ 054, 025, 045, 024, 031, 045, 024, 025, 067, 054,
					015, 015, 015, 015 };	// *ENDINDEX*____
	int idxLen;	// number of blocks in index
	int mmbIdxLen;	// length of index item (must be 25?)
	int itmBlk;	// number of items per block

	int putItms;	// number of items MSPUT

	// The following are only valid after openMemb() returns 00203
	int[] freeCCTTRR;	// from *UNUSED* item
	int freeBlocks;		// from *UNUSED* item
	int[] freeMember;
	int freeOff;
	int[] foundMember;
	int foundOff;
	CoreMemory newMembBuf;
	int newMembOff;

	// Caller locates *VOLNAMES*, etc items and passes info to this ctor.
	public PartitionedSeqFile(RandomRecordIO dsk, int unit, byte[] name, int mode,
			CoreMemory blkBuf, int blkAdr,
			int itmLen, int recLen, int recTrk, int recBlk,
			int idxLen, int mmbIdxLen,
			DiskUnit[] alloc) {
		super(dsk, unit, name, mode, blkBuf, blkAdr,
				itmLen, recLen, recTrk, recBlk, alloc);
		itmBlk = (recLen * recBlk) / itmLen;
		this.idxLen = idxLen;	// number of blocks
		this.mmbIdxLen = mmbIdxLen;	// always 25? at least 25?
	}

	// Create a R/O clone of this file. Not used?
	@Override
	public DiskFile dup() {
		PartitionedSeqFile dup = new PartitionedSeqFile(dsk, unit, name,
				DiskFile.IN, null, 0, itmLen, recLen, recTrk, recBlk,
				idxLen, mmbIdxLen, units);
		return dup;
	}

	@Override
	public void setDescr(CoreMemory dscBuf, int dscAdr) {
		super.setDescr(dscBuf, dscAdr);
		DiskVolume.putOne(idxLen, dscBuf, dscAdr + 63);
		dscBuf.writeChar(dscAdr + 68, (byte)mmbIdxLen);
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
		if (sts != _BEG_) {
			error = 00434;	// actually, file not initialized
			return false;
		}
		freeCCTTRR = DiskVolume.getSome(blkBufMem, blkBufAdr + 15, 3);
		freeBlocks = DiskVolume.getNum(blkBufMem, blkBufAdr + 21, 3);
		freeMember = null;
		foundMember = null;
		int off = mmbIdxLen;
		int nBlks = 0;
		while (true) {
			while (off + mmbIdxLen <= blkLen) {
				sts = blkBufMem.readChar(blkBufAdr + off + 24);
				// not necessary to compare to _ENDINDEX_?
				if (sts == _END_) {	// _ENDINDEX_
					// there must be 1 more item available...
					// in order to declare freeMember...
					if (freeMember == null &&
							(off + mmbIdxLen <= blkLen ||
							nBlks + 1 < idxLen)) {
						freeMember = getAddress();
						freeOff = off;
					}
					return true;	// caller examines foundMember
				}
				if (sts == _DEL_) {	// deleted
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

	// MSPUT was done on member... must create member...
	private boolean updateMemb() {
		int putBlks = (putItms + itmBlk - 1) / itmBlk;
		int[] end = new int[]{ nxtCyl, nxtTrk, nxtRec };
		boolean ok = rewindIndex();
		if (!ok) return false;
		int n = DiskVolume.getNum(blkBufMem, blkBufAdr + 21, 3);
		n -= putBlks;
		if (n < 0) n = 0;
		DiskVolume.putNum(n, blkBufMem, blkBufAdr + 21, 3);
		DiskVolume.putSome(end, blkBufMem, blkBufAdr + 15);
		dirty = true;
		ok = super.seek(foundMember); // this might be *ENDINDEX*...
		if (!ok) return false;
		int off = foundOff;
		byte sts = blkBufMem.readChar(blkBufAdr + off + 24);
		CoreMemory tmp = null;
		if (sts == _END_) {
			// openMemb() does not set freeMember unless
			// we have space to insert one more...
			tmp = new BufferMemory(mmbIdxLen);
			tmp.copyIn(0, blkBufMem, blkBufAdr + off, mmbIdxLen);
		}
		blkBufMem.copyIn(blkBufAdr + off, newMembBuf, newMembOff, 14);
		blkBufMem.writeChar(blkBufAdr + off + 14, (byte)015);
		DiskVolume.putSome(freeCCTTRR, blkBufMem, blkBufAdr + off + 15);
		DiskVolume.putNum(putBlks, blkBufMem, blkBufAdr + off + 21, 3);
		blkBufMem.writeChar(blkBufAdr + off + 24, _ALL_);
		dirty = true;
		if (sts == _END_) {
			off += mmbIdxLen;
			if (off + mmbIdxLen > blkLen) {
				// we already know there is room
				curCyl = nxtCyl;
				curTrk = nxtTrk;
				curRec = nxtRec;
				if (!cacheBlock(false, curCyl, curTrk, curRec)) {
					return false;
				}
				off = 0;
			}
			tmp.copyOut(0, blkBufMem, blkBufAdr + off, mmbIdxLen);
			dirty = true;
		}
		return sync();
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
	public boolean getItem() {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		return super.getItem();
	}

	@Override
	public boolean getItem(CoreMemory itm, int adr) {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		return super.getItem(itm, adr);
	}

	@Override
	public boolean repItem() {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		return super.repItem();
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
	public boolean putItem() {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		// TODO: can member be extended? must detect end of space...
		++putItms;
		return super.putItem();
	}

	@Override
	public boolean putItem(CoreMemory itm, int adr) {
		if (foundMember == null) {
			error = 00203; // TODO: what is right error?
			return false;
		}
		// TODO: can member be extended? must detect end of space...
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
		byte req = (byte)(mode == DiskFile.OUT ? _ALL_ : _PRT_);
		int[] beg = null;
		if (foundMember == null) {
			// no match found
			if (mode != DiskFile.OUT) {
				error = 00203;
				return false;
			}
			if (freeMember == null) {
				error = 00204;	// no space in index
				return false;
			}
			// This member might be _END_, in which case
			// we insert new member and push _END_ down one.
			foundMember = freeMember;
			foundOff = freeOff;
			newMembBuf = memb;
			newMembOff = adr;
			// Member index is not updated until ENDM...
			beg = freeCCTTRR;
		} else {
			// blkBufMem has the index item at foundOff...
			byte sts = blkBufMem.readChar(blkBufAdr + foundOff + 24);
			// TODO: can PUT ever be allowed? what about end?
			if (req > sts) {
				error = 00214;
				return false;
			}
			beg = DiskVolume.getSome(blkBufMem, blkBufAdr + foundOff + 15, 3);
		}
		// This buffers the first block, ready for MOVE or LOCATE modes.
		if (!super.seek(beg[0], beg[1], beg[2], 0)) {
			return false;
		}
		// TODO: update 'mode' to reflect current member mode
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
			updateMemb();
		}
		mode &= ~DiskFile.IN_OUT;
		foundMember = null;
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
		// Brute-force mechanism to locate end of index...
		int nBlks = 0;
		while (true) {
			if (++nBlks >= idxLen) {
				break;
			}
			curCyl = nxtCyl;
			curTrk = nxtTrk;
			curRec = nxtRec;
			if (!cacheBlock(false, curCyl, curTrk, curRec)) {
				return false;
			}
		}
		// snapshot end of index (start of data)
		int[] dat = new int[]{ nxtCyl, nxtTrk, nxtRec };
		if (!rewindIndex()) {
			return false;
		}
		int off = 0;
		blkBufMem.copyIn(blkBufAdr + off, _UNUSED_, 0, 14);
		blkBufMem.writeChar(blkBufAdr + off + 14, (byte)015);
		DiskVolume.putSome(dat, blkBufMem, blkBufAdr + off + 15);
		DiskVolume.putNum(numBlks - idxLen, blkBufMem, blkBufAdr + off + 21, 3);
		blkBufMem.writeChar(blkBufAdr + off + 24, _BEG_);
		dirty = true;
		// assume at least two per record?
		off += mmbIdxLen;
		blkBufMem.copyIn(blkBufAdr + off, _ENDINDEX_, 0, 14);
		blkBufMem.writeChar(blkBufAdr + off + 14, (byte)015);
		DiskVolume.putSome(dat, blkBufMem, blkBufAdr + off + 15);
		DiskVolume.putNum(numBlks - idxLen, blkBufMem, blkBufAdr + off + 21, 3);
		blkBufMem.writeChar(blkBufAdr + off + 24, _END_);
		dirty = true;
		if (!sync()) {
			return false;
		}
		return true;
	}
}
