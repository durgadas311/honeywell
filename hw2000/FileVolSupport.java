// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;

public class FileVolSupport {
	static public String error = null;

	static public boolean initVolume(RandomRecordIO dsk, int unit,
			// TODO: any other parameters?
			byte[] volnam, byte[] volser) {
		// TODO: system files/records use A-file protection?
		error = null;
		int flag = 0;
		DiskFile volNames = DiskVolume.newVolNames(dsk, unit, -1, -1);
		DiskFile volDescr = DiskVolume.newVolDescr(dsk, unit, -1, -1);
		DiskFile volAlloc = DiskVolume.newVolAlloc(dsk, unit, -1, -1);
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
			for (int cyl = 0; cyl < 203; ++cyl) {
				for (int trk = 0; trk < 20; ++trk) {
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
			DiskVolume.setEOD(itmNames, 0);
			ok = volNames.putItem(itmNames, 0);
			if (!ok) {
				error = volNames.getError();
				return false;
			}
			volNames.close();
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
			DiskVolume.setEOD(itmDescr, 0);
			ok = volDescr.putItem(itmDescr, 0);
			if (!ok) {
				error = volDescr.getError();
				return false;
			}
			volDescr.close();
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
			DiskVolume.setEOD(itmAlloc, 0);
			ok = volAlloc.putItem(itmAlloc, 0);
			if (!ok) {
				error = volAlloc.getError();
				return false;
			}
			volAlloc.close();
		} finally {
			dsk.end();
		}
		return true;
	}

	static private String hwToString(byte[] in, int start, int len,
					CharConverter cvt) {
		String ret = "";
		for (int x = 0; x < len; ++x) {
			ret += cvt.hwToLP(in[start + x]);
		}
		return ret;
	}

	static private String hwToString(CoreMemory in, int start, int len,
					CharConverter cvt) {
		String ret = "";
		for (int x = 0; x < len; ++x) {
			ret += cvt.hwToLP(in.readChar(start + x));
		}
		return ret;
	}

	static private void mapTracks(int[][] map, int fno,
				int sCyl, int sTrk, int eCyl, int eTrk) {
		for (int cyl = sCyl; cyl <= eCyl; ++cyl) {
			if (map[cyl] == null) {
				map[cyl] = new int[20];
				Arrays.fill(map[cyl], 0);
			}
			for (int trk = sTrk; trk <= eTrk; ++trk) {
				map[cyl][trk] = fno;
			}
		}
	}

	static private String getMap(int[] map) {
		if (map == null) {
			return String.format("%60s", "");
		}
		String ret = "";
		for (int trk = 0; trk < 20; ++trk) {
			if (map[trk] == 0) {
				ret += "   ";
			} else {
				String hash = Integer.toString(map[trk], 36);
				ret += String.format(" %2s", hash);
			}
		}
		return ret;
	}

	// TODO: how best to deliver volume map...
	static public boolean mapVolume(RandomRecordIO dsk, int unit, HW2000 sys) {
		error = null;
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
		int[][] map = new int[203][];
		vol.getVolNames().rewind();
		while (true) {
			String ret = "";
			if (!vol.getVolNames().getItem(names, 0)) {
				error = vol.getVolNames().getError();
				return false;
			}
			++total;
			String hash = Integer.toString(total, 36);
			if (names.readChar(0) == 077) {
				++free;
				continue;
			}
			if (vol.isEOD(names, 0)) {
				--total; // don't count EOD
				break;
			}
			ret += String.format("%2s: %10s ", hash,
					hwToString(names, 0, 10, sys.pdc.cvt));
			int[] ctri = vol.getSome(names, 14, 4);
			vol.getVolDescr().seek(ctri);
			// TODO: hot to detect past EOD
			vol.getVolDescr().getItem(descr, 0);
			// TODO: check errors, EOD
			switch (descr.readChar(0)) {
			case 001: ret += 'S'; break;
			case 002: ret += 'D'; break;
			case 003: ret += 'I'; break;
			case 011: ret += 'P'; break;
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
			ctri = vol.getSome(names, 22, 4);
			vol.getVolAlloc().rewind();
			int tracks = 0;
			for (int u = 0; u < 6; ++u) {
				if (ctri[0] != 0 || ctri[1] != 0 ||
						ctri[2] != 0 || ctri[3] != 0) {
					vol.getVolAlloc().seek(ctri);
					// TODO: hot to detect past EOD
				}
				vol.getVolAlloc().getItem(alloc, 0);
				// TODO: check errors, EOD
				fd = vol.getSome(alloc, 4, 4);
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
			ret += String.format("= %d tracks/%d records/%d blocks/%d items",
				tracks, tracks * recTrk, (tracks * recTrk) / recBlk,
				((tracks * recTrk) / recBlk) * itmBlk);
			ret += '\n';
			sys.listOut(ret);
		}
		sys.listOut(String.format("Total *VOLNAMES* entries: %d  free: %d\n",
						total, free));
		for (int cyl = 0; cyl < 100; ++cyl) {
			String ret = getMap(map[cyl]) + " : " + getMap(map[cyl + 100]);
			sys.listOut(ret + '\n');
		}
		// TODO: look for orphaned allocations
		return true;
	}

	static private CoreMemory findName(DiskVolume vol, DiskFile file, byte[] name) {
		CoreMemory buf = new BufferMemory(file.itemLen());
		file.rewind();
		while (true) {
			if (!file.getItem(buf, 0)) {
				error = file.getError();
				return null;
			}
			if (vol.isEOD(buf, 0)) {
				error = "No file";
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
				error = file.getError();
				return null;
			}
			if (vol.isEOD(buf, 0)) {
				error = "No space";
				return null;
			}
			if (buf.readChar(0) == 077) {
				break;
			}
		}
		return buf;
	}

	static public boolean initFile(RandomRecordIO dsk, int unit, int flag,
			byte[] name, int itmLen, int recLen, int recTrk, int recBlk,
			DiskUnit[] units) {
		error = null;
		boolean ok;
		int itmBlk = (recBlk * recLen) / itmLen;
		DiskVolume vol = new DiskVolume(dsk, unit);
		if (!vol.mount()) {
			error = vol.getError();
			return false;
		}
		CoreMemory names = findName(vol, vol.getVolNames(), name);
		if (names != null) {
			error = "File exists";
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
		CoreMemory alloc = findFree(vol, vol.getVolAlloc());
		if (alloc == null) {
			return false;
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
		descr.writeChar(0, (byte)001);	// Sequential - TODO: support other organizations
		vol.putOne(itmLen, descr, 1);
		vol.putOne(recLen, descr, 3);
		vol.putOne(itmBlk, descr, 5);
		vol.putOne(recBlk, descr, 7);
		vol.putOne(recTrk, descr, 9);
		// TODO: populate other fields...
		ok = vol.getVolDescr().repItem(descr, 0);
		ok = vol.getVolDescr().sync();
		if (!ok) {
			error = vol.getVolNames().getError();
			return false;
		}
		// TODO: verify units[*] are free...
		for (int a = 0; a < 6; ++a) {
			if (a > 0) {
				ok = vol.getVolAlloc().getItem(alloc, 0);
			}
			if (units[a] == null) {
				// Make all unused entries appear "not free"
				alloc.writeChar(0, (byte)075); // extension to MOD1
			} else if (a == 5 || units[a + 1] == null) {
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
				error = vol.getVolNames().getError();
				return false;
			}
		}
		ok = vol.getVolAlloc().sync();
		// Now (re-)format file allocation units tracks...
		boolean first = true;
		int pCyl = -1;
		int pTrk = -1;
		for (int u = 0; u < units.length && units[u] != null; ++u) {
			for (int cyl = units[u].sCyl; cyl <= units[u].eCyl; ++cyl) {
				pCyl = cyl;
				for (int trk = units[u].sTrk; trk <= units[u].eTrk; ++trk) {
					if (!first) {
						ok = dsk.initTrack(flag, pCyl, pTrk,
								recLen, recTrk,
								cyl, trk);
						if (!ok) {
							error = dsk.getError();
							return false;
						}
					}
					pTrk = trk;
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
		return true;
	}

	static public boolean releaseFile(RandomRecordIO dsk, int unit, byte[] name) {
		error = null;
		DiskVolume vol = new DiskVolume(dsk, unit);
		if (!vol.mount()) {
			// Volume not initialized or other error
			error = vol.getError();
			return false;
		}
		try {
			CoreMemory names = findName(vol, vol.getVolNames(), name);
			CoreMemory descr = new BufferMemory(vol.getVolDescr().itemLen());
			CoreMemory alloc = new BufferMemory(vol.getVolAlloc().itemLen());
			if (names == null) {
				return false;
			}
			names.writeChar(0, (byte)077);
			vol.getVolNames().repItem(names, 0);
			int[] ctri = vol.getSome(names, 14, 4);
			vol.getVolDescr().seek(ctri);
			vol.getVolDescr().getItem(descr, 0);
			descr.writeChar(0, (byte)077);
			vol.getVolDescr().repItem(descr, 0);
			ctri = vol.getSome(names, 22, 4);
			vol.getVolAlloc().seek(ctri);
			// assume all contiguous
			for (int u = 0; u < 6; ++u) {
				vol.getVolAlloc().getItem(alloc, 0);
				alloc.writeChar(0, (byte)077);
				vol.getVolAlloc().putItem(alloc, 0);
			}
		} finally {
			vol.unmount();
		}
		return true;
	}
}
