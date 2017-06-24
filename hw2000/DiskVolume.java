// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DiskVolume {
	public static final int namesItmLen = 30;
	public static final int descrItmLen = 100;
	public static final int allocItmLen = 20;
	public static final byte[] _1VOL = new byte[]{ 001, 065, 046, 043, 015}; // 1VOL_
	public static final byte[] _EOD_ = new byte[]{ 076, 025, 046, 024, 077}; // [EOD^
	public static final byte[] VOLNAMES = new byte[]
		{ 054, 065, 046, 043, 045, 021, 044, 025, 062, 054 }; // "*VOLNAMES*"
	public static final byte[] VOLDESCR = new byte[]
		{ 054, 065, 046, 043, 024, 025, 062, 023, 051, 054 }; // "*VOLDESCR*"
	public static final byte[] VOLALLOC = new byte[]
		{ 054, 065, 046, 043, 021, 043, 043, 046, 023, 054 }; // "*VOLALLOC*"
	RandomRecordIO dsk = null;
	int unit;
	CharConverter cvt;
	boolean mounted = false;
	int error = 0;
	byte[] name;	// a.k.a. volume name (i.e. label - may change)
	byte[] snum;	// a.k.a. volume serial number (i.e. UUID)
	static final int def_reclen = 250;
	// These should never be closed (except by unmount())
	DiskFile volNames;
	DiskFile volDescr;
	DiskFile volAlloc;
	static SimpleDateFormat _timestamp = new java.text.SimpleDateFormat("yyDDD");

	// Each operation must be atomic, i.e. bounded by dsk.begin()/dsk.end().
	// This is because the peripheral cannot operate on multiple units
	// at the same time (single buffer, etc).
	// cyl 0, trk 0 = bootstrap
	// cyl 0, trk 1 = volume header/label (first record only)
	// cyl 0, trk 2 = (3-7 tracks) volume directory
	//              *VOLNAMES* == 1 track (cyl 0, trk 2)
	//              *VOLDESCR* <= 3 tracks (cyl 0, trk 3...) *
	//              *VOLALLOC* <= 3 tracks (cyl 0, trk 3+...) *
	// * Based on MAX FILES parameter specified at init time.
	//   *VOLDESCR* and *VOLALLOC* are the same length (in tracks).
	// Or...
	// Volume Header specifies beginning CCTT of *VOLNAMES* (*VOLDESCR*... follow?)
	//
	public DiskVolume(RandomRecordIO dsk, int unit, CharConverter cvt) {
		this.dsk = dsk;
		this.unit= unit;
		this.cvt= cvt;
		// Both of these appear to be strings (characters, not binary)
		name = new byte[6];
		snum = new byte[6];
		Arrays.fill(name, (byte)015);
	}

	public int getError() {
		return error;
	}

	public byte[] getName() {
		return name;
	}

	public byte[] getSerial() {
		return snum;
	}

	public DiskFile getVolNames() {
		return volNames;
	}

	public DiskFile getVolDescr() {
		return volDescr;
	}

	public DiskFile getVolAlloc() {
		return volAlloc;
	}

	public boolean mount() {
		// "mount" the volume.
		if (!dsk.begin(unit)) {
			error = dsk.getError();
			return false;
		}
		int[] ct = new int[]{ 0, 2 };	// default location CCTT
		try {
			CoreMemory r = new BufferMemory(def_reclen);
			int f = dsk.seekRecord(0, 1, 0);
			if (f < 0) {
				error = dsk.getError();
				return false;
			}
			if (!dsk.readRecord(r, 0, -1)) {
				error = dsk.getError();
				return false;
			}
			if (!compare(_1VOL, r, 0)) {
				// Not previously initialized
				error = 00000; // Disk pack not initialized
				return false;
			}
			// TODO: trim trailing blanks?
			r.copyOut(5, name, 0, 6);
			r.copyOut(12, snum, 0, 6);
			// TODO: verify device type (what vlaue is 278?)
			// TODO: what other data?
			if (r.readChar(19) == 010) {
				ct = getSome(r, 23, 2);
			}
			mounted = true;
		} finally {
			dsk.end();
		}
		// Length of data may vary, but starting cyl/trk cannot.
		volNames = newVolNames(dsk, unit, DiskFile.IN_OUT, ct[0], ct[1]);
		ct[1] += 1;
		volDescr = newVolDescr(dsk, unit, DiskFile.IN_OUT, ct[0], ct[1]);
		ct[1] += 3;
		volAlloc = newVolAlloc(dsk, unit, DiskFile.IN_OUT, ct[0], ct[1]);
		ct[1] += 3;
		return true;
	}

	// Always consumes one track
	static public DiskFile newVolNames(RandomRecordIO dsk, int unit,
						int mode, int cyl, int trk) {
		if (cyl < 0) {
			cyl = 0;
		}
		if (trk < 0) {
			trk = 2;
		}
		int recTrk = dsk.numRecords(def_reclen);
		// TODO: compute records/block... following assumes 250 char record.
		return new SequentialFile(dsk, unit, VOLNAMES, mode,
					namesItmLen, def_reclen, recTrk, 3,
					cyl, trk, cyl, trk + 0);
	}

	// Always consumes three tracks
	static public DiskFile newVolDescr(RandomRecordIO dsk, int unit,
						int mode, int cyl, int trk) {
		if (cyl < 0) {
			cyl = 0;
		}
		if (trk < 0) {
			trk = 3;
		}
		int recTrk = dsk.numRecords(def_reclen);
		// TODO: compute records/block... following assumes 250 char record.
		return new SequentialFile(dsk, unit, VOLDESCR, mode,
					descrItmLen, def_reclen, recTrk, 2,
					cyl, trk, cyl, trk + 2);
	}

	// Always consumes three tracks
	static public DiskFile newVolAlloc(RandomRecordIO dsk, int unit,
						int mode, int cyl, int trk) {
		if (cyl < 0) {
			cyl = 0;
		}
		if (trk < 0) {
			trk = 6;
		}
		int recTrk = dsk.numRecords(def_reclen);
		// TODO: compute records/block... following assumes 250 char record.
		return new SequentialFile(dsk, unit, VOLALLOC, mode,
					allocItmLen, def_reclen, recTrk, 2,
					cyl, trk, cyl, trk + 2);
	}

	public void unmount() {
		// must commit all buffered data... is there any?
		volNames.close();
		volDescr.close();
		volAlloc.close();
		volNames = null;
		volDescr = null;
		volAlloc = null;
		mounted = false;
	}

	public byte[] timestamp() {
		String now = _timestamp.format(new Date());
		return cvt.hwString(now, 5);
	}

	// TODO: name, blockBuffer, workBuffer, moveLocate, inOut,
	public DiskFile openFile(byte[] name, int mode) {
		return openFile(name, mode, null, 0, null, 0);
	}
	public DiskFile openFile(byte[] name, int mode,
				CoreMemory blkBuf, int blkBufAdr,
				CoreMemory dscBuf, int dscBufAdr) {
		// Special-case system files - only allow R/O
		if (compare(VOLNAMES, name)) {
			if (dscBuf != null) {
				volNames.setDescr(dscBuf, dscBufAdr);
			}
			return volNames.dup();
		}
		if (compare(VOLDESCR, name)) {
			if (dscBuf != null) {
				volDescr.setDescr(dscBuf, dscBufAdr);
			}
			return volDescr.dup();
		}
		if (compare(VOLALLOC, name)) {
			if (dscBuf != null) {
				volAlloc.setDescr(dscBuf, dscBufAdr);
			}
			return volAlloc.dup();
		}
		CoreMemory nItm = new BufferMemory(volNames.itemLen());
		volNames.rewind();
		while (true) {
			if (!volNames.getItem(nItm, 0)) {
				if (volNames.isEOF()) {
					error = 00103;	// No file
				} else {
					error = volNames.getError();
				}
				return null;
			}
			if (nItm.readChar(0) == 077) {
				continue;
			}
			if (compare(name, nItm, 0)) {
				break;
			}
		}
		CoreMemory dItm;
		int dAdr;
		if (dscBuf != null) {
			dItm = dscBuf;
			dAdr = dscBufAdr;
		} else {
			dItm = new BufferMemory(volDescr.itemLen());
			dAdr = 0;
		}
		CoreMemory aItm = new BufferMemory(volAlloc.itemLen());
		// TODO: volume sequence number, etc.
		int[] ctri = getSome(nItm, 14, 4);
		// get *VOLDESCR* item...
		if (!volDescr.seek(ctri)) {
			error = volDescr.getError();
			return null;
		}
		if (!volDescr.getItem(dItm, dAdr)) {
			error = volDescr.getError();
			return null;
		}
		// TODO: implement Volume Directory Exit 01...
		// Must export dItm to caller, and allow for
		// resumption either here or in volNames loop (find another).
		// Unfortunately, must make this exit any time the
		// Volume Directory Exit was specified, even if caller
		// never uses it.
		int type = dItm.readChar(dAdr + 0);
		int[] desc = getSome(dItm, dAdr + 1, 5);
		int idxLen = getNum(dItm, dAdr + 63, 2);
		int mmbIdxLen = dItm.readChar(dAdr + 68);
		// TODO: overflow, etc.
		if ((mode & (DiskFile.OUT | DiskFile.UPDATE)) != 0) {
			dItm.copyIn(dAdr + 29, timestamp(), 0, 5);
			// TODO: increment modification number...
			volDescr.repItem();
			volDescr.sync();
		}
		DiskUnit[] units = new DiskUnit[6];
		ctri = getSome(nItm, 22, 4);
		// get *VOLALLOC* item(s)...
		for (int y = 0; y < units.length; ++y) {
			if ((ctri[0] != 0 || ctri[1] != 0 || ctri[2] != 0 || ctri[3] != 0) &&
					!volAlloc.seek(ctri)) {
				error = volAlloc.getError();
				return null;
			}
			if (!volAlloc.getItem(aItm, 0)) {
				error = volAlloc.getError();
				return null;
			}
			int sts = aItm.readChar(0);
			if (sts == 077) { // error (?)
				error = 00105;	// *VOLALLOC* corrupt
				return null;
			}
			int[] ctct = getSome(aItm, 4, 4);
			units[y] = new DiskUnit(ctct);
			if (sts == 040) {
				break;
			}
			if (sts != 060) {
				// TODO: support multi-volume and overflow
				error = 00123;	// File type unknown
				return null;
			}
			ctri = getSome(aItm, 12, 4);
		}
		if (type == DiskFile.SEQUENTIAL) {
			return new SequentialFile(dsk, unit, name, mode,
					blkBuf, blkBufAdr,
					desc[0], desc[1], desc[4], desc[3],
					units);
		} else if (type == DiskFile.PART_SEQ) {
			return new PartitionedSeqFile(dsk, unit, name, mode,
					blkBuf, blkBufAdr,
					desc[0], desc[1], desc[4], desc[3],
					idxLen, mmbIdxLen,
					units);
		} else {
			// TODO: support other file organizations
			error = 00123;	// File type unknown
			return null;
		}
	}

	// Get one or more 2-char integers from a buffer.
	static public int[] getSome(CoreMemory buf, int start, int num) {
		int z = start;
		int[] ints = new int[num];
		for (int x = 0; x < num; ++x) {
			ints[x] = buf.readChar(z++) << 6;
			ints[x] |= buf.readChar(z++);
		}
		return ints;
	}

	// Put one or more 2-char integers into a buffer.
	static public void putSome(int[] ints, CoreMemory buf, int start) {
		int z = start;
		for (int x = 0; x < ints.length; ++x) {
			buf.writeChar(z++, (byte)(ints[x] >> 6));
			buf.writeChar(z++, (byte)ints[x]);
		}
	}
	static public void putOne(int val, CoreMemory buf, int start) {
		int z = start;
		buf.writeChar(z++, (byte)(val >> 6));
		buf.writeChar(z++, (byte)val);
	}

	static public void putNum(int val, CoreMemory buf, int start, int num) {
		while (--num >= 0) {
			buf.writeChar(start + num, (byte)val);
			val >>= 6;
		}
	}

	static public int getNum(CoreMemory buf, int start, int num) {
		int val = 0;
		while (num-- > 0) {
			val <<= 6;
			val |= buf.readChar(start++);
		}
		return val;
	}

	static public void initVOL(CoreMemory rec, byte[] name, byte[] snum) {
		rec.zero(0, -1);
		rec.copyIn(0, _1VOL, 0, _1VOL.length);
		rec.copyIn(5, name, 0, 6);
		rec.copyIn(12, snum, 0, 6);
		rec.writeChar(11, (byte)013); // TODO: this is model 274? need 278?
		// TODO: *VOLNAMES* starting cyl/trk?
	}

	static public void setEOD(CoreMemory var, int start) {
		var.copyIn(start, _EOD_, 0, _EOD_.length);
	}

	static public boolean isEOD(CoreMemory var, int start) {
		return compare(_EOD_, var, start);
	}

	private boolean compare(byte[] con, byte[] var) {
		if (var.length < con.length) {
			return false;
		}
		for (int x = 0; x < con.length; ++x) {
			if (con[x] != var[x]) {
				return false;
			}
		}
		return true;
	}

	static public boolean compare(byte[] con, CoreMemory var, int start) {
		for (int x = 0; x < con.length; ++x) {
			if (con[x] != var.readChar(x + start)) {
				return false;
			}
		}
		return true;
	}
}
