// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;

public class FileVolSupport {
	static private int error = 0;

	// param 40 = directory exit (01)
	// param 41 = index exit     (02)
	// param 42 = not used       (03)
	// param 43 = data exit      (04)
	// param 44 = device exit    (05)
	static private Map<Integer, String> errmsg;
	static {
		// This represents all documented EXITs, many are not used.
		//
		// These are errors outside of MIOC (MOD1MSIO never uses).
		// Mainly used between FileVolSupport and HW2000FrontPanel.
		errmsg = new HashMap<Integer, String>();
		errmsg.put(00001, "Directory full");
		errmsg.put(00002, "Allocation conflict");
		errmsg.put(00003, "File exists");
		errmsg.put(00005, "Invalid Action");	// <<<<< catch-all
		//
		// "Directory" errors - MSOPEN, MSCLOS
		errmsg.put(00101, "OPEN CALLOUT 1");
		errmsg.put(00103, "File Not Found");
		errmsg.put(00104, "U-A Table Too Small");
		errmsg.put(00105, "Error in *VOLALLOC*");
		errmsg.put(00111, "CLOSE CALLOUT");
		errmsg.put(00113, "Volume Sequence Number Error");
		errmsg.put(00114, "Password Error");	// password incorrect
		errmsg.put(00121, "Dismount Previous Volume");	// Open callout 2
		errmsg.put(00123, "File Type Unknown");	// NODOC
		errmsg.put(00124, "Password Error");	// no password given
		errmsg.put(00133, "Device Table Too Small");
		errmsg.put(00134, "Mount Next Volumes");
		errmsg.put(00143, "Volume Sequence Number Error");
		errmsg.put(00153, "No Data On File-Volume");
		// "Index" errors - Partitioned or Indexed files
		errmsg.put(00203, "Member Not Found");	// SETM
		errmsg.put(00213, "Member Not Found");	// MALTER
		errmsg.put(00204, "Member Index Full");
		errmsg.put(00214, "Member Cannot Be Output Only");
		errmsg.put(00224, "Member Cannot Be Deleted");
		// "Every Index Entry" - non-fatal/advisory
		errmsg.put(00301, "SETM CALLOUT");
		// "Data" errors -
		errmsg.put(00401, "End File (Input)");
		errmsg.put(00411, "End File (Output)");
		errmsg.put(00412, "MSINS CALLOUT 1");
		errmsg.put(00422, "MSINS CALLOUT 2");
		errmsg.put(00434, "No Space For New Member");
		errmsg.put(00403, "Item Not Found");
		errmsg.put(00413, "No Space To Insert Item");
		errmsg.put(00404, "Invalid Bucket");
		errmsg.put(00423, "No Space For Moved Item");
		errmsg.put(00414, "Key Verification Failure");
		errmsg.put(00424, "Duplicate Item");
		// "Device" (hardware) errors
		errmsg.put(00501, "Inoperable");
		errmsg.put(00502, "Inoperable");	// Protection violation
		errmsg.put(00503, "Positioning Error");	// seek error
		errmsg.put(00504, "Write Error");	// format violation or overflow
		errmsg.put(00505, "Positioning Error");	// record not found
		errmsg.put(00506, "Read Error");	// data xfer
		errmsg.put(00507, "Read Error");	// no data xfer
		errmsg.put(00510, "Write Error");
		errmsg.put(00511, "Miscellaneous");	// Track-linking record
		errmsg.put(00512, "Read Error");	// Track-Linking error
	}

	static public String getError() {
		return getError(error);
	}

	static public String getError(int err) {
		if (errmsg.containsKey(err)) {
			return errmsg.get(err);
		} else {
			return String.format("Unknown error %04o", err);
		}
	}

	static int nTrk;
	static int nCyl;

	static public boolean initVolume(RandomRecordIO dsk, int unit,
			// TODO: any other parameters?
			byte[] volnam, byte[] volser) {
		// TODO: system files/records use A-file protection?
		// TODO: allow custom cylinder/track where *VOLNAMES* begins?
		nCyl = dsk.numCylinders();
		nTrk = dsk.numTracks();
		error = 0;
		int flag = 0;
		// Note, for internal users 'mode' is don't-care.
		DiskFile volNames = DiskVolume.newVolNames(dsk, unit, 0, -1, -1);
		DiskFile volDescr = DiskVolume.newVolDescr(dsk, unit, 0, -1, -1);
		DiskFile volAlloc = DiskVolume.newVolAlloc(dsk, unit, 0, -1, -1);
		if (!dsk.begin(unit)) {
			error = dsk.getError();
			return false;
		}
		try {
			boolean first = true;
			boolean ok = false;
			int pCyl = 0;
			int pTrk = 0;
			// We could only format (part of) cylinder 0 here,
			// since in most cases each file re-formats the cylinders
			// assigned to it. But this avoids "sparse" formatting.
			for (int cyl = 0; cyl < nCyl; ++cyl) {
				for (int trk = 0; trk < nTrk; ++trk) {
					if (!first) {
						ok = dsk.initTrack(flag, pCyl, pTrk,
								DiskVolume.def_reclen, -1,
								cyl, trk);
						if (!ok) {
							error = dsk.getError();
							return false;
						}
					}
					pTrk = trk;
					pCyl = cyl;
					first = false;
				}
			}
			// TODO: where does last track on disk point TLR to?
			ok = dsk.initTrack(flag, pCyl, pTrk, DiskVolume.def_reclen, -1, 1, 0);
			if (!ok) {
				error = dsk.getError();
				return false;
			}
			CoreMemory rec = new BufferMemory(DiskVolume.def_reclen);
			DiskVolume.initVOL(rec, volnam, volser);
			dsk.seekRecord(0, 1, 0);
			dsk.writeRecord(rec, 0, -1);
			// Init *VOLNAMES*
			CoreMemory itmNames = new BufferMemory(volNames.itemLen());
			itmNames.zero(0, -1);
			itmNames.writeChar(0, (byte)077); // "unused/free/deleted"
			for (int i = 0; i < 100; ++i) {
				ok = volNames.putItem(itmNames, 0);
				if (!ok) {
					error = volNames.getError();
					return false;
				}
			}
			// close() will add _EOD_ item...
			ok = volNames.close();
			if (!ok) {
				error = volNames.getError();
				return false;
			}
			// Init *VOLDESCR*
			CoreMemory itmDescr = new BufferMemory(volDescr.itemLen());
			itmDescr.zero(0, -1);
			itmDescr.writeChar(0, (byte)077); // "unused/free/deleted"
			for (int i = 0; i < 100; ++i) {
				ok = volDescr.putItem(itmDescr, 0);
				if (!ok) {
					error = volDescr.getError();
					return false;
				}
			}
			// close() will add _EOD_ item...
			ok = volDescr.close();
			if (!ok) {
				error = volDescr.getError();
				return false;
			}
			// Init *VOLALLOC*
			CoreMemory itmAlloc = new BufferMemory(volAlloc.itemLen());
			itmAlloc.zero(0, -1);
			itmAlloc.writeChar(0, (byte)077); // "unused/free/deleted"
			// 6 allocation units per file, max
			for (int i = 0; i < 600; ++i) {
				ok = volAlloc.putItem(itmAlloc, 0);
				if (!ok) {
					error = volAlloc.getError();
					return false;
				}
			}
			// close() will add _EOD_ item...
			ok = volAlloc.close();
			if (!ok) {
				error = volAlloc.getError();
				return false;
			}
		} finally {
			dsk.end();
		}
		return true;
	}

	static public String hwToString(byte[] in, int start, int len,
					CharConverter cvt) {
		String ret = "";
		for (int x = 0; x < len; ++x) {
			ret += cvt.hwToLP(in[start + x]);
		}
		return ret;
	}

	static public String hwToString(CoreMemory in, int start, int len,
					CharConverter cvt) {
		String ret = "";
		for (int x = 0; x < len; ++x) {
			ret += cvt.hwToLP(in.readChar(start + x));
		}
		return ret;
	}

	static private void mapTracks(int[][] map, int fno, DiskUnit[] units) {
		for (int u = 0; u < units.length && units[u] != null; ++u) {
			mapTracks(map, fno, units[u].sCyl, units[u].sTrk,
					units[u].eCyl, units[u].eTrk);
		}
	}

	static private void mapTracks(int[][] map, int fno,
				int sCyl, int sTrk, int eCyl, int eTrk) {
		for (int cyl = sCyl; cyl <= eCyl; ++cyl) {
			if (map[cyl] == null) {
				map[cyl] = new int[nTrk];
				Arrays.fill(map[cyl], 0);
			}
			for (int trk = sTrk; trk <= eTrk; ++trk) {
				if (map[cyl][trk] != 0) {
					if (fno != 0x20000) {
						map[cyl][trk] |= 0x40000;
					}
				} else {
					map[cyl][trk] = fno;
				}
			}
		}
	}

	static private String getMapHdr() {
		String ret = "";
		for (int trk = 0; trk < nTrk; ++trk) {
			ret += String.format(" %02d", trk);
		}
		return ret;
	}

	static private String getMap(int[] map) {
		String ret = "";
		for (int trk = 0; trk < nTrk; ++trk) {
			if (map == null) {
				ret += "  .";
				continue;
			}
			int fno = map[trk] & 0x3ffff;
			boolean err = (map[trk] & 0x40000) != 0;
			String hash;
			if (fno == 0) {
				hash = "  .";
			} else if (fno == 0x10000) {
				hash = " $$";
			} else if (fno == 0x20000) {
				hash = " ??";
			} else {
				hash = String.format("%3s", Integer.toString(fno, 36));
			}
			if (err) {
				ret += hash.replace(' ', '*');
			} else {
				ret += hash;
			}
		}
		return ret;
	}

	static private int totalTracks(int[][] map) {
		int tracks = 0;
		for (int cyl = 0; cyl < nCyl; ++cyl) {
			if (map[cyl] == null) {
				continue;
			}
			for (int trk = 0; trk < nTrk; ++trk) {
				if (map[cyl][trk] != 0) {
					++tracks;
				}
			}
		}
		return tracks;
	}

	static private void mapMembers(HW2000 sys, DiskFile ps, int idxBlk) {
		BufferMemory buf = new BufferMemory(ps.itemLen());
		int numItms = idxBlk * (ps.blockLen() / ps.itemLen());
		if (!ps.rewind()) {
			return;
		}
		for (int x = 0; x < numItms; ++x) {
			if (!ps.getItem(buf, 0)) {
				return;
			}
			String ret = "";
			int sts = buf.readChar(24);
			int[] ctr = DiskVolume.getSome(buf, 15, 3);
			int num = DiskVolume.getNum(buf, 21, 3);
			ret += String.format("    %14s",
					hwToString(buf, 0, 14, sys.pdc.cvt));
			ret += String.format(" %02o", sts);
			ret += String.format(" %3d-%3d-%3d ", ctr[0], ctr[1], ctr[2]);
			ret += String.format(" %6d", num);
			ret += '\n';
			sys.listOut(ret);
			if (sts == PartitionedSeqFile._END_) {
				break;
			}
		}
		ps.close();
	}

	// TODO: how best to deliver volume map...
	// 'sys' needed for printer output and CharConverter.
	static public boolean mapVolume(RandomRecordIO dsk, int unit, HW2000 sys,
				boolean cylmap, boolean mmblst) {
		// TODO: make disk map optional.
		nCyl = dsk.numCylinders();
		nTrk = dsk.numTracks();
		error = 0;
		DiskVolume vol = new DiskVolume(dsk, unit);
		if (!vol.mount()) {
			error = vol.getError();
			return false;
		}
		sys.listOut(String.format("Volume name %s serial %s\n",
					hwToString(vol.getName(), 0, 6, sys.pdc.cvt),
					hwToString(vol.getSerial(), 0, 6, sys.pdc.cvt)));
		int free = 0;
		int total = 0;
		CoreMemory names = new BufferMemory(vol.getVolNames().itemLen());
		CoreMemory descr = new BufferMemory(vol.getVolDescr().itemLen());
		CoreMemory alloc = new BufferMemory(vol.getVolAlloc().itemLen());
		int[][] map = new int[nCyl][];
		nCyl -= 3;	// reserved...
		// TODO: get actual allocated tracks...
		mapTracks(map, 0x10000, 0, 0, 0, 1);	// boot track and volume header
		mapTracks(map, 0x10000, vol.getVolNames().getAlloc());
		mapTracks(map, 0x10000, vol.getVolDescr().getAlloc());
		mapTracks(map, 0x10000, vol.getVolAlloc().getAlloc());
		vol.getVolNames().rewind();
		sys.listOut("ID  NAME       O ITM REC I/B R/B R/T B/X   CAPACITY\n");
		while (true) {
			String ret = "";
			if (!vol.getVolNames().getItem(names, 0)) {
				if (vol.getVolNames().isEOF()) {
					break;
				}
				error = vol.getVolNames().getError();
				return false;
			}
			++total;
			String hash = Integer.toString(total, 36);
			if (names.readChar(0) == 077) {
				++free;
				continue;
			}
			ret += String.format("%2s: %10s ", hash,
					hwToString(names, 0, 10, sys.pdc.cvt));
			int[] ctri = vol.getSome(names, 14, 4);
			vol.getVolDescr().seek(ctri);
			// TODO: how to detect past EOD
			vol.getVolDescr().getItem(descr, 0);
			// TODO: check errors, EOD
			int type = descr.readChar(0);
			switch (type) {
			case DiskFile.SEQUENTIAL: ret += 'S'; break;
			case DiskFile.DIRECT: ret += 'D'; break;
			case DiskFile.INDEXED_SEQ: ret += 'I'; break;
			case DiskFile.PART_SEQ: ret += 'P'; break;
			case 070: ret += 'N'; break;
			default: ret += '?'; break;
			}
			int[] fd = vol.getSome(descr, 1, 5);
			int itmLen = fd[0];
			int recLen = fd[1];
			int itmBlk = fd[2];
			int recBlk = fd[3];
			int recTrk = fd[4];
			ret += String.format(" %3d %3d %3d %3d %3d ",
					itmLen, recLen, itmBlk, recBlk, recTrk);
			if (type == DiskFile.PART_SEQ) {
				int[] xx = vol.getSome(descr, 63, 1);
				ret += String.format("%3d ", xx[0]);
			} else {
				ret += "    ";
			}
			ctri = vol.getSome(names, 22, 4);
			vol.getVolAlloc().rewind();
			int tracks = 0;
			int[] psau = null;
			for (int u = 0; u < 6; ++u) {
				if (ctri[0] != 0 || ctri[1] != 0 ||
						ctri[2] != 0 || ctri[3] != 0) {
					vol.getVolAlloc().seek(ctri);
					// TODO: hot to detect past EOD
				}
				vol.getVolAlloc().getItem(alloc, 0);
				// TODO: check errors, EOD
				fd = vol.getSome(alloc, 4, 4);
				if (type == DiskFile.PART_SEQ && psau == null) {
					psau = fd;
				}
				tracks += (fd[2] - fd[0] + 1) * (fd[3] - fd[1] + 1);
				mapTracks(map, total, fd[0], fd[1], fd[2], fd[3]);
				int t = alloc.readChar(0);
				if (t == 040) {
					break;
				}
				if (t != 060) {
					break;	// error
				}
				ctri = vol.getSome(alloc, 12, 4);
			}
			ret += String.format("= %d trks/%d recs/%d blks/%d items",
				tracks, tracks * recTrk, (tracks * recTrk) / recBlk,
				((tracks * recTrk) / recBlk) * itmBlk);
			ret += '\n';
			sys.listOut(ret);
			if (mmblst && type == DiskFile.PART_SEQ) {
				int xlen = vol.getNum(descr, 63, 2);
				int xitm = descr.readChar(68);
				// use a Sequential view, it's easier
				// TODO: what if recLen < xitm?
				byte[] nm = new byte[10];
				names.copyOut(0, nm, 0, 10);
				DiskFile ps = new SequentialFile(dsk, unit, nm, 0,
					xitm, recLen, recTrk, 1,
					psau[0], psau[1], psau[2], psau[3]);
				mapMembers(sys, ps, xlen);
			}
		}
		// Go through *VOLALLOC* looking for orphaned allocations.
		vol.getVolAlloc().rewind();
		while (true) {
			vol.getVolAlloc().getItem(alloc, 0);
			if (vol.getVolAlloc().isEOF()) {
				break;
			}
			int t = alloc.readChar(0);
			if (t != 040 && t != 060) {
				continue;
			}
			int[] fd = vol.getSome(alloc, 4, 4);
			mapTracks(map, 0x20000, fd[0], fd[1], fd[2], fd[3]);
		}
		int tracks = totalTracks(map);
		sys.listOut(String.format("Total *VOLNAMES* entries: %d  free: %d" +
				"  Tracks used: %d/%d %d%%\n",
				total, free, tracks, (nCyl * nTrk),
				(tracks * 100 + 50) / (nCyl * nTrk)));
		if (cylmap) {
			sys.listOut("   " + getMapHdr() + " :    " + getMapHdr() + '\n');
			for (int cyl = 0; cyl < nCyl / 2; ++cyl) {
				String ret = String.format("%03d", cyl) +
					getMap(map[cyl]) +
					String.format(" : %03d", cyl + nCyl / 2) +
					getMap(map[cyl + nCyl / 2]);
				sys.listOut(ret + '\n');
			}
		}
		// TODO: look for orphaned allocations
		return true;
	}

	static private CoreMemory findName(DiskVolume vol, DiskFile file, byte[] name) {
		CoreMemory buf = new BufferMemory(file.itemLen());
		file.rewind();
		while (true) {
			if (!file.getItem(buf, 0)) {
				if (file.isEOF()) {
					error = 00103;	// No file
				} else {
					error = file.getError();
				}
				return null;
			}
			if (buf.readChar(0) == 077) {
				continue;
			}
			if (vol.compare(name, buf, 0)) {
				break;
			}
		}
		return buf;
	}

	static private CoreMemory findFree(DiskVolume vol, DiskFile file) {
		CoreMemory buf = new BufferMemory(file.itemLen());
		file.rewind();
		while (true) {
			if (!file.getItem(buf, 0)) {
				if (file.isEOF()) {
					error = 00001;
				} else {
					error = file.getError();
				}
				return null;
			}
			if (buf.readChar(0) == 077) {
				break;
			}
		}
		return buf;
	}

	static private boolean chkAlloc(DiskUnit[] units, int[] ctct) {
		DiskUnit alloc = new DiskUnit(ctct);
		for (int u = 0; u < units.length; ++u) {
			if (units[u] == null) {
				break;
			}
			if (units[u].intersects(alloc)) {
				return true;
			}
		}
		return false; // no conflict
	}

	static private CoreMemory findFreeAlloc(DiskVolume vol, DiskFile file,
						DiskUnit[] units) {
		int[] free = null;
		int[] ctct = null;
		CoreMemory buf = new BufferMemory(file.itemLen());
		file.rewind();
		while (true) {
			if (!file.getItem(buf, 0)) {
				if (file.isEOF()) {
					if (free != null) {
						break;
					}
					error = 00001; // no space
				} else {
					error = file.getError();
				}
				return null;
			}
			byte sts = buf.readChar(0);
			switch (sts) {
			case 077:
				if (free == null) {
					free = file.getAddress();
				}
				break;
			case 040:
			case 060:
				ctct = vol.getSome(buf, 4, 4);
				if (chkAlloc(units, ctct)) {
					error = 00002;	// space already used
					return null;
				}
				break;
			}
		}
		file.seek(free);
		file.getItem(buf, 0);
		return buf;
	}

	static public boolean initFile(RandomRecordIO dsk, int unit, int flag,
			byte[] name, int type,
			int itmLen, int recLen, int recTrk, int recBlk,
			int blkIdx, DiskUnit[] units) {
		error = 0;
		boolean ok;
		int itmBlk = (recBlk * recLen) / itmLen;
		int mmbItm = 25; // Hard-coded for Part. Seq. files
		DiskVolume vol = new DiskVolume(dsk, unit);
		if (!vol.mount()) {
			error = vol.getError();
			return false;
		}
		CoreMemory names = findName(vol, vol.getVolNames(), name);
		if (names != null) {
			error = 00003;	// File exists
			return false;
		}
		names = findFree(vol, vol.getVolNames());
		if (names == null) {
			return false;
		}
		// found a free slot for new file...
		// Now find space in *VOLDESCR* and *VOLALLOC*.
		// We make *VOLALLOC* simpler by always taking 6 slots.
		CoreMemory descr = findFree(vol, vol.getVolDescr());
		if (descr == null) {
			return false;
		}
		// This also checks for conflicts...
		CoreMemory alloc = findFreeAlloc(vol, vol.getVolAlloc(), units);
		if (alloc == null) {
			return false;
		}
		int nTrks = 0;
		for (int a = 0; a < 6 && units[a] != null; ++a) {
			nTrks += units[a].size();
		}
		names.copyIn(0, name, 0, 10);
		names.writeChar(10, (byte)0);	// first (only) volume
		names.writeChar(11, (byte)0);	// MOD1
		vol.putSome(vol.getVolDescr().getAddress(), names, 14);
		vol.putSome(vol.getVolAlloc().getAddress(), names, 22);
		ok = vol.getVolNames().repItem(names, 0);
		ok = vol.getVolNames().sync();
		if (!ok) {
			error = vol.getVolNames().getError();
			return false;
		}
		// TODO: use setDescr() method on file... must construct first.
		descr.writeChar(0, (byte)type);
		vol.putOne(itmLen, descr, 1);
		vol.putOne(recLen, descr, 3);
		vol.putOne(itmBlk, descr, 5);
		vol.putOne(recBlk, descr, 7);
		vol.putOne(recTrk, descr, 9);
		if (type == DiskFile.SEQUENTIAL || type == DiskFile.PART_SEQ) {
			vol.putNum(((nTrks * recTrk) / recBlk), descr, 65, 3);
		}
		if (type == DiskFile.PART_SEQ) {
			vol.putOne(blkIdx, descr, 63);
			descr.writeChar(68, (byte)mmbItm);
		}
		// TODO: populate other fields...
		ok = vol.getVolDescr().repItem(descr, 0);
		ok = vol.getVolDescr().sync();
		if (!ok) {
			error = vol.getVolDescr().getError();
			return false;
		}
		int e = 0;
		// units[*] were checked for free in findFreeAlloc()...
		for (int a = 0; a < 6; ++a) {
			if (a > 0) {
				ok = vol.getVolAlloc().getItem(alloc, 0);
			}
			if (units[a] == null) {
				// Make all unused entries appear "not free"
				alloc.writeChar(0, (byte)076); // extension to MOD1
			} else if (a == 5 || units[a + 1] == null) {
				e = a;
				alloc.writeChar(0, (byte)040); // last unit
			} else {
				alloc.writeChar(0, (byte)060); // more units follow
			}
			if (units[a] != null) {
				vol.putSome(units[a].get(), alloc, 4);
			}
			alloc.zero(12, 20);
			ok = vol.getVolAlloc().repItem(alloc, 0);
			if (!ok) {
				error = vol.getVolAlloc().getError();
				return false;
			}
		}
		ok = vol.getVolAlloc().sync();
		if (!ok) {
			error = vol.getVolAlloc().getError();
			return false;
		}
		// Now (re-)format file allocation units tracks...
		boolean first = true;
		int pCyl = -1;
		int pTrk = -1;
		int recs = 0;
		for (int u = 0; u < units.length && units[u] != null; ++u) {
			for (int cyl = units[u].sCyl; cyl <= units[u].eCyl; ++cyl) {
				for (int trk = units[u].sTrk; trk <= units[u].eTrk; ++trk) {
					if (!first) {
						ok = dsk.initTrack(flag, pCyl, pTrk,
								recLen, recTrk,
								cyl, trk);
						if (!ok) {
							error = dsk.getError();
							return false;
						}
						recs += recTrk;
					}
					pTrk = trk;
					pCyl = cyl;
					first = false;
				}
			}
		}
		// Last track in file points to next physical track on disk...
		// For simplicity, we just point to next cylinder.
		ok = dsk.initTrack(flag, pCyl, pTrk, recLen, recTrk, pCyl + 1, 0);
		if (!ok) {
			error = dsk.getError();
			return false;
		}
		recs += recTrk;
		// Type-specific file initialization...
		// TODO: handle other types
		if (type == DiskFile.SEQUENTIAL) {
			DiskFile file = new SequentialFile(dsk, unit, name,
					DiskFile.IN_OUT, null, 0,
					itmLen, recLen, recTrk, recBlk,
					units);
			if (!file.release()) {
				error = file.getError();
				return false;
			}
			ok = file.close();
		} else if (type == DiskFile.PART_SEQ) {
			DiskFile file = new PartitionedSeqFile(dsk, unit, name,
					DiskFile.UPDATE, null, 0,
					itmLen, recLen, recTrk, recBlk,
					blkIdx, mmbItm,
					units);
			if (!file.release()) {
				error = file.getError();
				return false;
			}
			ok = file.close();
		}
		return true;
	}

	static public boolean releaseFile(RandomRecordIO dsk, int unit, byte[] name) {
		error = 0;
		DiskVolume vol = new DiskVolume(dsk, unit);
		if (!vol.mount()) {
			// Volume not initialized or other error
			error = vol.getError();
			return false;
		}
		try {
			boolean ok;
			CoreMemory names = findName(vol, vol.getVolNames(), name);
			CoreMemory descr = new BufferMemory(vol.getVolDescr().itemLen());
			CoreMemory alloc = new BufferMemory(vol.getVolAlloc().itemLen());
			if (names == null) {
				// error already set
				return false;
			}
			names.writeChar(0, (byte)077);
			ok = vol.getVolNames().repItem(names, 0);
			if (!ok) {
				error = vol.getVolNames().getError();
				return false;
			}
			ok = vol.getVolNames().sync();
			if (!ok) {
				error = vol.getVolNames().getError();
				return false;
			}
			int[] ctri = vol.getSome(names, 14, 4);
			ok = vol.getVolDescr().seek(ctri);
			if (!ok) {
				error = vol.getVolDescr().getError();
				return false;
			}
			ok = vol.getVolDescr().getItem(descr, 0);
			if (!ok) {
				error = vol.getVolDescr().getError();
				return false;
			}
			descr.writeChar(0, (byte)077);
			ok = vol.getVolDescr().repItem(descr, 0);
			if (!ok) {
				error = vol.getVolDescr().getError();
				return false;
			}
			ok = vol.getVolDescr().sync();
			if (!ok) {
				error = vol.getVolDescr().getError();
				return false;
			}
			ctri = vol.getSome(names, 22, 4);
			ok = vol.getVolAlloc().seek(ctri);
			if (!ok) {
				error = vol.getVolAlloc().getError();
				return false;
			}
			// assume all contiguous
			for (int u = 0; u < 6; ++u) {
				ok = vol.getVolAlloc().getItem(alloc, 0);
				if (!ok) {
					error = vol.getVolAlloc().getError();
					return false;
				}
				alloc.writeChar(0, (byte)077);
				ok = vol.getVolAlloc().repItem(alloc, 0);
				if (!ok) {
					error = vol.getVolAlloc().getError();
					return false;
				}
			}
			ok = vol.getVolAlloc().sync();
			if (!ok) {
				error = vol.getVolAlloc().getError();
				return false;
			}
		} finally {
			vol.unmount();
		}
		return true;
	}
}
