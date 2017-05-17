// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.Arrays;

// Honeywell "Disk Pack" Devices:
//
//	Model	plat	cap	cyls	hds
//	-----	----	----	----	---
//	170-2	6	4.6
//	173-2	6	9.2
//	171	6	4.6	100	10
//	172	6	9.2	200	10
//	258	6	4.6	100	10
//	258B	6	4.6	100	10
//	259	6	9.2	200	10
//	259B	6	9.2	200	10
//	273	11	18.4	200	20
//	274	11	18.4	200	20
//	275	11	18.4	200	20
//	276	11	37.4	200	20
//	277	11	64	?	20
//	278	11	35	200	20	"double density 274"
//	279	12	133.3	404	19	(12 disks = 22 surfaces... not 19...)
//	261	36/2	150	256	64
//	262	72/2	300	256	128
//	266	*
//	267	*
// plat = number of platters (disks)
// cap = capacity, formatted, million characters
//
// Semantics/Rationale For I/O are described in DiskSemantics.txt
//
// Disk Format:
//	Index is implied at track[0]
//	[index][AM][header][DM][data][AM]...[DM][data][EM][garbage]
//
public class P_Disk extends JFrame
		implements Peripheral, RandomRecordIO,
			ActionListener, WindowListener {
	static final byte AM = (byte)0304; // start of record header
	static final byte DM = (byte)0305; // start of record data
	static final byte EM = (byte)0306; // end of valid track formatting

	// Switches/indicators on control units...
	// Also, bits from MOD1 MCA parameter 31.
	static final byte PERMIT_FMT = (byte)001;	// Not record header
	static final byte PERMIT_DAT = (byte)002;	// Not record header
	static final byte HDR_XFR = (byte)002;		// Record header only
	static final byte PERMIT_A = (byte)004;
	static final byte PERMIT_B = (byte)010;
	static final byte PERMIT_AB = (byte)(PERMIT_A | PERMIT_B);
	static final byte HDR_BAD = (byte)020;		// Record header only
	static final byte HDR_TLR = (byte)040;		// Record header only

	// Based on Model 278 drives:
	static final int num_trk = 20;
	static final int num_cyl = 203; // 200-202 reserved?
	//
	// NOTE: period documentation indicates storage capacity per track of
	// between 8760 and 9725 (6-bit) chars/trk (IBM: 7294 bytes, HW: 8760 chars;
	// seems to be formatted). HW Data rate 416000 chars/sec (2496000 bits/sec),
	// rotational latency 12.5msec (25msec full rotation, 2400 RPM or 40 RPS).
	// This suggests 62400 raw bits (10400 chars) per track - implying 6-16%
	// overhead for formatting. Typical HW default record size is 250 characters
	// (1500 bits), with known overhead of at least 15 characters per record,
	// plus TLR record and INDEX. HW Quoted capacity is 35.0 million chars/disk,
	// which at 250 chars/rec would be 140000 recs/disk, at 200 cyls that is
	// 700 recs/cyl, at 20 trk/cyl that is 35 rec/trk. And 35 rec/trk at
	// 250 char/rec is 8750 data char/trk. Adding TLR would make that about 8756
	// (plus overhead for 36 records). Overhead for 36 records and IM would bring
	// that to 9297 char/trk. There are 72 IRGs (plus the last one). This works
	// out to an average IRG size of 15 chars. Total IRG overhead is 1095 char.
	//
	// Since this implementation does not use IRGs, the trk_len value need not
	// reflect actual physical disk characteristics. However, a value of 10000
	// should allow for realistic expectations when using smaller record lengths.
	// This implementation does not use IM but does use an EM to ensure
	// uninitialized data after last record is ignored. AM, DM, and EM are
	// 8-bit values, all other data is strictly 6-bit.
	//
	// Physical disk format (original hardware):
	//
	//     IM IRG AM HDR IRG DM DATA IRG AM HDR IRG ... DM TLR IRG*
	//
	// Each of IM, AM, and DM are assumed to be one character.
	// HDR, DATA, and TLR each include a 2-character checksum.
	// HDR is 9 characters. TLR is 6 characters. (DATA default 250 char)
	// IRG* (gap from end of last record to IM) is of whatever length
	// is leftover.
	//
	static final int raw_len = 10400; // theoretical real track capacity, chars
	static final int trk_len = 10000; // virtual track size used.
	static final int cyl_len = trk_len * num_trk;

	private class DiskStatus {
		RandomAccessFile dev;
		int cyl;
		boolean busy;
		byte flag;
		JButton a_bt;
		JButton b_bt;
		JButton f_bt;
		JButton d_bt;
		JLabel cyl_pn;
		JLabel mnt_pn;
		public DiskStatus() {
			dev = null;
			cyl = 0;
			busy = false;
			flag = 0;
		}
	}

	// These are all stored by io() and used during run()...
	int unit = 0; // this would help BOOTSTRAP
	boolean busy;
	boolean fmt_allow = false; // switch setting, per drive?
	boolean format = false;
	boolean extended = false;
	boolean verify = false;
	boolean initial = false;
	boolean search = false;
	boolean next = false;
	boolean error = false;
	String errMsg = null;
	HW2000 sys;

	DiskStatus[] sts;

	File _last = null;
	boolean isOn = false;

	// The address register:
	int adr_cyl;
	int adr_trk;
	int adr_rec;
	// Current head position:
	int curr_pos;
	int curr_end; // points to end of data, after searchData()
	int revs;
	int curr_cyl; // as read from header
	int curr_trk; // as read from header
	int curr_rec; // as read from header
	int curr_len; // as read from header
	byte curr_flg; // as read from header

	// cache:
	byte[] track;
	int track_unit;
	int track_cyl;
	int track_trk;
	long track_pos;
	boolean track_dirty;

	int vUnit;
	int vCyl;
	int vTrk;
	int vRec;
	int vLen;
	boolean vOK;

	// 11 platters, 20 usable surfaces
	// 200 cyls (0000-0312 or 203?)
	// 10400 char/trk
	public P_Disk() {
		super("H278 Disk Pack Devices");
		Font smallFont = new Font("Sans-Serif", Font.PLAIN, 8);
		_last = new File(System.getProperty("user.dir"));
		busy = false;
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);

		sts = new DiskStatus[8];
		track = new byte[trk_len];
		track_cyl = -1;
		track_trk = -1;
		track_unit = -1;

		for (int x = 0; x < 8; ++x) {
			sts[x] = new DiskStatus();
			JPanel pn = new JPanel();
			pn.setLayout(new FlowLayout());
			JButton bt = new JButton(String.format("%03o", x));
			bt.setActionCommand(String.format("%d", x));
			bt.addActionListener(this);
			pn.add(bt);
			bt = new JButton("A");
			bt.setActionCommand(String.format("A%d", x));
			bt.addActionListener(this);
			bt.setBackground(HW2000FrontPanel.btnWhiteOff);
			bt.setMargin(new Insets(0, 0, 0, 0));
			bt.setPreferredSize(new Dimension(25, 25));
			pn.add(bt);
			sts[x].a_bt = bt;
			bt = new JButton("B");
			bt.setActionCommand(String.format("B%d", x));
			bt.addActionListener(this);
			bt.setBackground(HW2000FrontPanel.btnWhiteOff);
			bt.setMargin(new Insets(0, 0, 0, 0));
			bt.setPreferredSize(new Dimension(25, 25));
			pn.add(bt);
			sts[x].b_bt = bt;
			bt = new JButton("FMT");
			bt.setFont(smallFont);
			bt.setActionCommand(String.format("F%d", x));
			bt.addActionListener(this);
			bt.setBackground(HW2000FrontPanel.btnWhiteOff);
			bt.setMargin(new Insets(0, 0, 0, 0));
			bt.setPreferredSize(new Dimension(25, 25));
			pn.add(bt);
			sts[x].f_bt = bt;
			bt = new JButton("DAT");
			bt.setFont(smallFont);
			bt.setActionCommand(String.format("D%d", x));
			bt.addActionListener(this);
			bt.setBackground(HW2000FrontPanel.btnWhiteOff);
			bt.setMargin(new Insets(0, 0, 0, 0));
			bt.setPreferredSize(new Dimension(25, 25));
			pn.add(bt);
			sts[x].d_bt = bt;
			sts[x].cyl_pn = new JLabel();
			sts[x].cyl_pn.setPreferredSize(new Dimension(50, 20));
			sts[x].cyl_pn.setOpaque(true);
			sts[x].cyl_pn.setBackground(Color.white);
			sts[x].mnt_pn = new JLabel();
			sts[x].mnt_pn.setPreferredSize(new Dimension(400, 20));
			sts[x].mnt_pn.setBackground(Color.white);
			sts[x].mnt_pn.setOpaque(true);
			sts[x].mnt_pn.setText("No Disk Pack");
			pn.add(sts[x].cyl_pn);
			pn.add(sts[x].mnt_pn);
			add(pn);
		}

		addWindowListener(this);
		pack();
	}

	public void reset() {
		cacheTrack(-1, -1);
		unit = 0;
	}

	public void setInterrupt(HW2000 sys) {
	}

	private void autoVisible(boolean on) {
		if (on != isOn) {
			isOn = on;
			setVisible(on);
		}
	}

	public void visible(boolean on) {
		autoVisible(on);
		if (on) {
			toFront();
		}
	}

	private boolean cacheTrack(int cyl, int trk) {
		if (track_unit >= 0 && track_cyl >= 0 && track_trk >= 0 &&
		    track_unit == unit && track_cyl == cyl && track_trk == trk) {
			return true;
		}
		if (track_dirty && sts[track_unit].dev != null) {
			try {
				sts[track_unit].dev.seek(track_pos);
				sts[track_unit].dev.write(track);
			} catch (Exception ee) {
				//ee.printStackTrace();
				error = true;
			}
		}
		track_dirty = false;
		// already know track_cyl != cyl || track_trk != trk...
		if (cyl < 0 || trk < 0) {
			return true;
		}
		int n = -1;
		if (sts[unit].dev != null) {
			try {
				track_pos = (cyl * cyl_len) + (trk * trk_len);
				sts[unit].dev.seek(track_pos);
				n = sts[unit].dev.read(track);
				if (n <= 0) {
					n = track.length;
					Arrays.fill(track, (byte)0);
				}
			} catch (Exception ee) {
				//ee.printStackTrace();
			}
		}
		if (n < 0) {
			error = true;
			return false;
		}
		sts[unit].cyl = cyl;
		sts[unit].cyl_pn.setText(String.format("%03d-%02d", cyl, trk));
		track_unit = unit;
		track_cyl = cyl;
		track_trk = trk;
		curr_pos = 0;
		curr_rec = -1;
		curr_len = 0;
		return true;
	}

	// does not advance curr_pos! does not check bounds!
	private void getHeader(int p) {
		curr_flg = track[p++];
		curr_cyl = (track[p++] & 077) << 6;
		curr_cyl |= (track[p++] & 077);
		curr_trk = (track[p++] & 077) << 6;
		curr_trk |= (track[p++] & 077);
		curr_rec = (track[p++] & 077) << 6;
		curr_rec |= (track[p++] & 077);
		curr_len = (track[p++] & 077) << 6;
		curr_len |= (track[p++] & 077);
	}

	// This only loads the "address register", does no formatting to disk.
	// This is only used in the FORMAT WRITE path.
	private void getHeaderMem(RWChannel rwc) {
		int a = rwc.getCLC() + 1; // CLC points to FLAG...
		adr_cyl = (rwc.sys.rawReadMem(a++) & 077) << 6;
		adr_cyl |= (rwc.sys.rawReadMem(a++) & 077);
		adr_trk = (rwc.sys.rawReadMem(a++) & 077) << 6;
		adr_trk |= (rwc.sys.rawReadMem(a++) & 077);
		adr_rec = (rwc.sys.rawReadMem(a++) & 077) << 6;
		adr_rec |= (rwc.sys.rawReadMem(a++) & 077);
	}

	private boolean searchHeader() {
		// we must not be in middle of record data...
		int r = revs;
		// should never require searching, except for EM case.
		while (track[curr_pos] != AM && revs - r < 2) {
			if (track[curr_pos] == EM) {
				curr_pos = 0;
				++revs;
			} else {
				incrPos(1);
			}
		}
		return true;
	}

	private void incrPos(int incr) {
		curr_pos += incr;
		if (curr_pos >= track.length) {
			++revs;
			curr_pos = 0;
		}
	}
		

	private boolean searchRecord() {
		revs = 0;
		do {
			if (!searchHeader()) {
				break;
			}
			// now pointing at AM
			// we rely on format being sane...
			// just in case curr_pos was bogus and inside a record.
			getHeader(curr_pos + 1);
			incrPos(10);
			// now pointing at DM
			if (track[curr_pos] != DM) {
				// something is wrong...
				// but, abort or skip?
				return false;
			}
			if (curr_cyl == adr_cyl && curr_trk == adr_trk && curr_rec == adr_rec) {
				// still pointing to DM, not data.
				return true;
			}
			incrPos(curr_len + 1);
			// should point to AM or EM...
		} while (revs < 2);
		return false;
	}

	private boolean searchData() {
		revs = 0;
		// should never require actual search.
		while (track[curr_pos] != DM) {
			if (track[curr_pos] == AM) {
				getHeader(curr_pos + 1);
				incrPos(10);
			} else {
				incrPos(1);
			}
			if (revs >= 2) {
				return false;
			}
		}
		++curr_pos;
		curr_end = curr_pos + curr_len; // should point to next AM, or EM
		return true;
	}

	private boolean newRecord() {
		if (curr_pos + 11 >= track.length) {
			return false;
		}
		int save_pos = curr_pos;
		int save_rec = curr_rec + 1;
		track[save_pos++] = AM;
		track[save_pos++] = curr_flg;
		track[save_pos++] = (byte)((curr_cyl >> 6) & 077);
		track[save_pos++] = (byte)(curr_cyl & 077);
		track[save_pos++] = (byte)((curr_trk >> 6) & 077);
		track[save_pos++] = (byte)(curr_trk & 077);
		track[save_pos++] = (byte)((save_rec >> 6) & 077);
		track[save_pos++] = (byte)(save_rec & 077);
		track[save_pos++] = (byte)((curr_len >> 6) & 077);
		track[save_pos++] = (byte)(curr_len & 077);
		track[save_pos++] = DM;
		if (save_pos + curr_len + 1 >= track.length) {
			return false;
		}
		curr_pos = save_pos;
		curr_rec = save_rec;
		curr_end = curr_pos + curr_len;
		return true;
	}

	private boolean initTLR(int cyl, int trk, int rec) {
		curr_flg |= HDR_TLR;
		curr_len = 6;
		if (!newRecord()) {
			return false;
		}
		if (curr_pos + 7 >= track.length) {
			return false;
		}
		track[curr_pos++] = (byte)((cyl >> 6) & 077);
		track[curr_pos++] = (byte)(cyl & 077);
		track[curr_pos++] = (byte)((trk >> 6) & 077);
		track[curr_pos++] = (byte)(trk & 077);
		track[curr_pos++] = (byte)((rec >> 6) & 077);
		track[curr_pos++] = (byte)(rec & 077);
		return true;
	}

	private boolean searchTLR() {
		// Must be same cylinder...
		// pointing to DM of TLR right now...
		++curr_pos;
		adr_cyl = (track[curr_pos++] & 077) << 6;
		adr_cyl |= (track[curr_pos++] & 077);
		adr_trk = (track[curr_pos++] & 077) << 6;
		adr_trk |= (track[curr_pos++] & 077);
		adr_rec = (track[curr_pos++] & 077) << 6;
		adr_rec |= (track[curr_pos++] & 077);
		// TODO: confirm EM?
		if (!cacheTrack(sts[unit].cyl, adr_trk)) {
			return false;
		}
		if (!searchRecord()) {
			// We land here if not same cylinder...
			return false;
		}
		return true;
	}

	// At the very least, this puts curr_pos at the first data char.
	private boolean findRecord() {
		if (!cacheTrack(sts[unit].cyl, adr_trk)) {
			error = true;
			return false;
		}
		// TODO: check TLR and skip to new track
		if (initial) {
			curr_pos = 0;
		} else if (search) {
			if (next) {
				++adr_rec;
			}
			if (!searchRecord()) {
				// This is not an error, unless
				// something actually failed.
				// Return 0 characters on "record not found"
				// (but no disk error).
				return false;
			}
			if ((next || extended) && (curr_flg & HDR_TLR) != 0) {
				if (!searchTLR()) {
					return false;
				}
			}
		}
		if (!searchData()) {
			error = true;
			return false;
		}
		return true;
	}

	private void doAdrReg(RWChannel rwc) {
		if (rwc.isInput()) {
			rwc.writeChar((byte)(adr_cyl >> 6));
			rwc.incrCLC();
			rwc.writeChar((byte)(adr_cyl));
			rwc.incrCLC();
			rwc.writeChar((byte)(adr_trk >> 6));
			rwc.incrCLC();
			rwc.writeChar((byte)(adr_trk));
			rwc.incrCLC();
			rwc.writeChar((byte)(adr_rec >> 6));
			rwc.incrCLC();
			rwc.writeChar((byte)(adr_rec));
			rwc.incrCLC();
		} else {
			adr_cyl = (rwc.readMem() & 077) << 6;
			rwc.incrCLC();
			adr_cyl |= (rwc.readMem() & 077);
			rwc.incrCLC();
			adr_trk = (rwc.readMem() & 077) << 6;
			rwc.incrCLC();
			adr_trk |= (rwc.readMem() & 077);
			rwc.incrCLC();
			adr_rec = (rwc.readMem() & 077) << 6;
			rwc.incrCLC();
			adr_rec |= (rwc.readMem() & 077);
			rwc.incrCLC();
		}
	}

	public void io(RWChannel rwc) {
		this.sys = rwc.sys;
		// C3:
		//	04	Store/Load address reg
		//	00/10	Read/Write initial
		//	20/30	Extended Read/Write initial
		//	01/11	Read/Write
		//	21/31	Extended Read/Write
		//	02/12	Search and Read/Write
		//	22/32	Extended Search and Read/Write
		//	03/13	Search and Read/Write Next
		//	23/33	Extended Search and Read/Write Next
		// Perform load/store address register now...
		if (rwc.c3 == 004) {
			rwc.startCLC();
			doAdrReg(rwc);
			return;
		}
		if (sts[unit].dev == null) {
			error = true;
			return;
		}
		sts[unit].busy = true;
		busy = true;
		extended = ((rwc.c3 & 020) == 020);
		verify = ((rwc.c3 & 010) == 010);
		format = ((rwc.c3 & 002) == 000);
		initial = ((rwc.c3 & 003) == 000);
		search = ((rwc.c3 & 002) == 002);
		next = ((rwc.c3 & 003) == 003);
	}

	// NOTE: when writing, transfer ends before RM.
	//	when reading, transfer last char to RM location
	//	(in case record length exceeds user data)
	public void run(RWChannel rwc) {
		if (!busy) {
			return;
		}
		if (rwc.isInput()) {
			doIn(rwc);
		} else {
			doOut(rwc);
		}
		// cyl not changed by run()...
		// but if we track record/sector...
		sts[unit].busy = false;
		busy = false;
	}

	private int readChar() {
		if (curr_pos >= curr_end) { // end of record
			return -1;
		}
		int c = track[curr_pos] & 0377;
		++curr_pos;
		return c;
	}

	private int writeChar(byte c) {
		if (curr_pos >= curr_end) { // end of record
			return -1;
		}
		track[curr_pos] = c;
		track_dirty = true;
		++curr_pos;
		return 0;
	}

	private void doIn(RWChannel rwc) {
		boolean header = false; // only valid if 'format'
		rwc.startCLC();
		if (format) {
			cacheTrack(sts[unit].cyl, adr_trk);
			if (initial) {
				curr_pos = 0;
			}
			if (!searchHeader()) {
				error = true;
				return;
			}
			++curr_pos;	// skip AM
			getHeader(curr_pos);
			// transfer header data first...
			header = true;
			curr_end = curr_pos + 9;
		} else if (!findRecord()) {
			// error set by findRecord()...
			return;
		}
		// 'curr_pos' points to first data byte.
		while (true) {
			int a = 0;
			int b = rwc.readMem() & 0300;
			if (format) {
				// TODO: what does "read format" do?
				// Should be symmetrical with "write format",
				// Currently then:
				//	transfer header data and record data.
				//	stop after one record.
				// check this in case track unformatted
				if (curr_pos >= track.length) {
					curr_pos = 0;
					break;
				}
				if (curr_pos >= curr_end) {
					if (header) {
						if (!searchData()) {
							error = true;
							return;
						}
					} else if (!extended || track[curr_pos] == EM) {
						// must be end of data...
						break;
					} else if (!searchHeader()) {
						error = true;
						return;
					}
					// keep going until end of track or RM
					header = !header;
				}
				a = track[curr_pos++];
			} else {
				// If disk record longer than RM,
				// need to place last char at RM.
				a = readChar(); // has side affects
				if (a < 0) { // no more data in record
					if (!extended) {
						break;
					}
					++adr_rec;
					if (!searchRecord()) {
						break;
					}
					if ((curr_flg & HDR_TLR) != 0) {
						if (!searchTLR()) {
							break;
						}
					}
					a = readChar();
					if (a < 0) {
						break;
					}
				}
			}
			// TODO: support 8-bit transfers?
			rwc.writeChar((byte)(a & 077));
			if (rwc.incrCLC()) {
				break;
			}
			if (b == 0300) {
				// Ensure we go no farther than RM.
				curr_pos = curr_end;
				break;
			}
		}
	}

	public void doOut(RWChannel rwc) {
		boolean header = false; // only valid if 'format'
		rwc.startCLC();
		if (format) {
			if ((sts[unit].flag & PERMIT_FMT) == 0) {
				error = true;
				return;
			}
			getHeaderMem(rwc); // loads adr_*
			// TODO: do we enforce match between FLAG and PERMIT switches?
			// or is it just "caveat emptor"?
			//if (((sts[unit].flag ^ adr_flg) & PERMIT_AB) != 0) {
			//	error = true;
			//	return;
			//}
			// if caller did not seek cyl, too bad for them...
			cacheTrack(sts[unit].cyl, adr_trk);
			track_dirty = true;
			if (initial) {
				curr_pos = 0;
			} else {
				// else search for EM?
				// if (track[curr_pos] != EM) error...
			}
			if (curr_pos + 11 >= track.length) {
				error = true;
				return;
			}
			track[curr_pos++] = AM;
			curr_end = curr_pos + 9;
			header = true;
////
			// This goes back over data loaded in getHeaderMem(),
			// but copies to disk record header now.
			for (int x = 0; x < 9; ++x) {
				// TODO: strip punc, check RM?
				track[curr_pos++] = (byte)(rwc.readMem() & 077);
				if (rwc.incrCLC()) {
					error = true;
					return;
				}
			}
			// This now loads curr_* vars with same data
			// gotten by getHeaderMem() and record header loop.
			// All of this could be cleaned up.
			getHeader(curr_pos - 9); // loads curr_* variables
			track[curr_pos++] = DM;
			curr_end = curr_pos + curr_len;
			if (curr_end + 1 >= track.length) {
				error = true;
				return;
			}
		} else {
			if ((sts[vUnit].flag & PERMIT_DAT) == 0) {
				error = true;
				return;
			}
			if (!findRecord()) {
				// error set by findRecord()...
				return;
			}
			// Check HEADER FLAG against PERMIT switches...
			int f = sts[unit].flag ^ PERMIT_AB;	// invert A/B bits
			f &= curr_flg;	// mask NOT A/B bits
			if ((f & PERMIT_AB) != 0) {	// if NOT A/B bit is 1, prot error...
				error = true;
				// TODO: OK to leave pointer at DM?
				return;
			}
		}
		while (!error) {
			byte a = rwc.readMem();
			// TODO: how does extended fit with RM check?
			if ((a & 0300) == 0300) {
				if (format) {
					while (curr_pos < curr_end) {
						track[curr_pos++] = 0;
					}
					// break will set EM...
					// regardless of 'header', EM will be detected.
				} else {
					curr_pos = curr_end;
				}
				break;
			}
			int e;
			// TODO: support 8-bit transfers?
			a &= 077;
			if (format) {
				// TODO: force stop based on header DL?
				if (curr_pos >= track.length) {
					error = true;
					curr_pos = 0;
					break;
				}
				if (curr_pos >= curr_end) {
					if (header) {
						getHeader(curr_pos - 9); // loads curr_* variables
						if (curr_pos + curr_len + 2 >=
								track.length) {
							error = true;
							break;
						}
						track[curr_pos++] = DM;
						curr_end = curr_pos + curr_len;
					} else if (!extended) {
						break;
					} else {
						if (curr_pos + 11 >= track.length) {
							error = true;
							break;
						}
						track[curr_pos++] = AM;
						curr_end = curr_pos + 9;
					}
					header = !header;
				}
				track[curr_pos++] = a;
				e = 0;
			} else {
				e = writeChar(a);
				if (e < 0 && extended) {
					// char not written...
					++adr_rec;
					if (!searchRecord()) {
						error = true;
						break;
					}
					if ((curr_flg & HDR_TLR) != 0) {
						if (!searchTLR()) {
							break;	// error?
						}
					}
					e = writeChar(a);
				}
			}
			if (e < 0) { // no more space in record...
				// char not written...
				error = true; // TODO: not an error?
				break;
			}
			if (rwc.incrCLC()) {
				break;
			}
		}
		if (format) {
			// need to replace this next record with AM,
			// if followed by another write: do not increment.
			track[curr_pos] = EM;
		}
		// TODO: optimize this out?
		cacheTrack(-1, -1);
	}

	public boolean busy(byte c2) {
		return busy;
	}

	public boolean ctl(RWChannel rwc) {
		// C3-Cn:
		//	xxxDDD = Tape Drive/Unit DDD
		// Branch to A if device busy, else...
		// (in) 011xxx = Restore to cyl 0
		//(out) 010xxx = (C4=00) Seek cyl C5,C6
		// Always...
		//(out) 001000 = Branch if control busy
		//(out) 000xxx = (C4=00) Branch if device busy
		//(out) 101000 = Branch if general exception during last PDT
		//(out) 110000 = Branch if TLR flag set
		//(out) 100000 = Override FORMAT PERMIT
		//(out) 111000 = Control allow OFF
		//(out) 111001 = Control allow ON
		//(out) 111010 = Control drive OFF
		//(out) 111011 = Control drive ON
		//(out) 111100 = Control interrupt OFF
		//(out) 111101 = Branch if control interrupt ON
		//(out) 111110 = Drive interrupt OFF
		//(out) 111111 = Branch if device interrupt ON

		boolean branch = false;
		boolean in = rwc.isInput();
		switch(rwc.c3 & 070) {
		case 000:
			if (in) {
				break;
			}
			// device busy...
			unit = rwc.c3 & 007;
			if (sts[unit].busy) {
				branch = true;
			}
			break;
		case 010:
			if (in) {
				break;
			}
			// control busy...
			if (busy) {
				branch = true;
			}
			break;
		case 020:
			if (in) {
				break;
			}
			unit = rwc.c3 & 007;
			if (sts[unit].busy) {
				branch = true;
			} else {
				sts[unit].cyl = (rwc.c5 << 6) | rwc.c6;
				sts[unit].cyl_pn.setText(String.format("%03d-00", sts[unit].cyl));
			}
			break;
		case 030:
			if (!in) {
				break;
			}
			unit = rwc.c3 & 007;
			if (sts[unit].busy) {
				branch = true;
			} else if (in) {
				sts[unit].cyl = 0;
				sts[unit].cyl_pn.setText("000-00");
			}
			break;
		case 040:
			if (in) {
				break;
			}
			// what resets this?
			fmt_allow = true;
			break;
		case 050:
			if (in) {
				break;
			}
			if (error) {
				branch = true;
				error = false;
			}
			break;
		case 060:
			if (in) {
				break;
			}
			if ((curr_flg & HDR_TLR) != 0) {
				branch = true;
			}
			break;
		case 070:
			// TODO: handle interrupt control
			if (in) {
				break;
			}
			break;
		}
		return branch;
	}

	private File pickFile(String purpose) {
		File file = null;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{"dpi"}, new String[]{"Disk Pack Img"}, _last, null);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
			_last = file; // or use dev[unit]?
		}
		return file;
	}

	private void updateFlags(DiskStatus unit, int flg) {
		unit.flag = (byte)flg;
		unit.a_bt.setBackground(
			(unit.flag & PERMIT_A) == 0 ?
			HW2000FrontPanel.btnWhiteOff :
			HW2000FrontPanel.btnWhiteOn);
		unit.b_bt.setBackground(
			(unit.flag & PERMIT_B) == 0 ?
			HW2000FrontPanel.btnWhiteOff :
			HW2000FrontPanel.btnWhiteOn);
		unit.f_bt.setBackground(
			(unit.flag & PERMIT_FMT) == 0 ?
			HW2000FrontPanel.btnWhiteOff :
			HW2000FrontPanel.btnWhiteOn);
		unit.d_bt.setBackground(
			(unit.flag & PERMIT_DAT) == 0 ?
			HW2000FrontPanel.btnWhiteOff :
			HW2000FrontPanel.btnWhiteOn);
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JButton)) {
			return;
		}
		JButton b = (JButton)e.getSource();
		String a = b.getActionCommand();
		if (a.charAt(0) == 'A') {
			int c = a.charAt(1) - '0';
			updateFlags(sts[c], sts[c].flag ^ PERMIT_A);
		} else if (a.charAt(0) == 'B') {
			int c = a.charAt(1) - '0';
			updateFlags(sts[c], sts[c].flag ^ PERMIT_B);
		} else if (a.charAt(0) == 'F') {
			int c = a.charAt(1) - '0';
			updateFlags(sts[c], sts[c].flag ^ PERMIT_FMT);
		} else if (a.charAt(0) == 'D') {
			int c = a.charAt(1) - '0';
			updateFlags(sts[c], sts[c].flag ^ PERMIT_DAT);
		} else {
			int c = a.charAt(0) - '0';
			String s = String.format("Mount %03o", c);
			if (sts[c].dev != null) {
				try {
					sts[c].dev.close();
				} catch (Exception ee) {}
				sts[c].dev = null;
				sts[c].cyl = 0;
				sts[c].cyl_pn.setText("");
				sts[c].mnt_pn.setText("No Disk Pack");
			}
			updateFlags(sts[c], 0);
			File f = pickFile(s);
			if (f == null) {
				return;
			}
			try {
				sts[c].dev = new RandomAccessFile(f, "rw");
				sts[c].cyl = 0;
				sts[c].cyl_pn.setText("000-00");
				sts[c].mnt_pn.setText(f.getName());
				return;
			} catch (Exception ee) {
				HW2000FrontPanel.warning(this, s, ee.toString());
			}
		}
	}

	public void windowActivated(WindowEvent e) { }
	public void windowClosed(WindowEvent e) { }
	public void windowIconified(WindowEvent e) { }
	public void windowOpened(WindowEvent e) { }
	public void windowDeiconified(WindowEvent e) { }
	public void windowDeactivated(WindowEvent e) { }
	public void windowClosing(WindowEvent e) {
		isOn = false;
		setVisible(false);
	}

	public boolean begin(int unit) {
		errMsg = null;
		// TODO: mutex with PDT/PCB...
		if (unit < 0 || unit > sts.length) {
			errMsg = "Invalid Unit Number";
			return false;
		}
		if (sts[unit].dev == null) {
			errMsg = "No disk pack";
			return false;
		}
		vUnit = unit;
		vOK = false;
		return true;
	}
	// Always returns TLR as regular data... caller needs info anyway.
	// Return value is -1 for error, else FLAG character...
	public int seekRecord(int cyl, int trk, int rec) {
		vCyl = cyl;
		vTrk = trk;
		vRec = rec;
		// 'cacheTrack' updates display
		if (!cacheTrack(vCyl, vTrk)) {
			errMsg = "Failed to access track";
			return -1;
		}
		// NOTE: adr_flg not used, switches are tested directly
		adr_cyl = vCyl;
		adr_trk = vTrk;
		adr_rec = vRec;
		if (!searchRecord() || !searchData()) {
			errMsg = "No record";
			return -1;
		}
		// curr_* matches reacord header data, incl. FLAG
		vLen = curr_len;
		vOK = true;
		return curr_flg;
	}
	public boolean readRecord(byte[] buf, int start, int len) {
		// TODO: OK to assume nothing has changed since seekRecord()?
		if (!vOK) {
			errMsg = "Sequence error";
			return false;
		}
		if (len < 0) {
			len = buf.length;
		}
		int p = 0;
		while (p < vLen && start + p < len) {
			buf[start + p++] = (byte)readChar();
		}
		vOK = false;
		return true;
	}
	public boolean writeRecord(byte[] buf, int start, int len) {
		// TODO: OK to assume nothing has changed since seekRecord()?
		if (!vOK) {
			errMsg = "Sequence error";
			return false;
		}
		if ((sts[vUnit].flag & PERMIT_DAT) == 0) {
			errMsg = "Data Protect";
			return false;
		}
		int f = sts[vUnit].flag ^ PERMIT_AB;	// invert A/B bits
		f &= curr_flg;	// mask NOT A/B bits
		if ((f & PERMIT_AB) != 0) {	// if NOT A/B bit is 1, prot error...
			errMsg = "A/B Protect";
			return false;
		}
		if (len < 0) {
			len = buf.length;
		}
		int p = 0;
		while (p < vLen && start + p < len) {
			writeChar(buf[start + p++]);
		}
		vOK = false;
		return true;
	}
	public boolean initTrack(int flg, int cyl, int trk, int reclen, int rectrk,
				int tCyl, int tTrk) {
		vOK = false;
		vCyl = cyl;
		vTrk = trk;
		if (rectrk < 0) {
			rectrk = numRecords(reclen);
		}
		// TODO: reduce duplicate code
		if (!cacheTrack(vCyl, vTrk)) {
			errMsg = "Failed to access track";
			return false;
		}
		if ((sts[vUnit].flag & PERMIT_FMT) == 0) {
			errMsg = "Format Protect";
			return false;
		}
		track_dirty = true;
		curr_pos = 0;
		curr_flg = (byte)(flg & PERMIT_AB);	// TODO: allow more bits?
		curr_cyl = vCyl;
		curr_trk = vTrk;
		curr_len = reclen;
		curr_rec = -1; // first record is 0
		// Just put as many records as will fit...
		for (int r = 0; r < rectrk; ++r) {
			if (!newRecord()) {
				errMsg = "Track overflow";
				return false;
			}
			Arrays.fill(track, curr_pos, curr_pos + curr_len, (byte)0);
			curr_pos += curr_len;
		}
		// Now set TLR... Initially, link to next physical track.
		// Allocated data will change to link to next allocated track.
		// TODO: what does last track on disk point to?
		// TODO: what does last track in cylinder point to?
		// NOTE: initTLR() trashes curr_* values...
		if (!initTLR(tCyl, tTrk, 0)) {
			// TODO: what to do?
			errMsg = "Track overflow";
			return false;
		}
		track[curr_pos] = EM;
		cacheTrack(-1, -1);
		return true;
	}
	public void end() {
		cacheTrack(-1, -1);
		vOK = false;
	}
	public int numRecords(int recLen) {
		// On a real disk, assume 10 chars per IRG.
		// HDR and REC have 2 char CRC. IM, AM, DM are 1 char.
		// IM has IRG before and after.
		// TLR+IM overhead is 52 characters (3 IRG).
		// Each record overhead is 35 chars (two IRG).
		// Compute how many 'recLen' records would have fit
		// on a real disk track:
		int recTrk = (raw_len - 52) / (recLen + 35);
		// The above number would always fit in our virtual track,
		// since overheads are less: TLR+IM = 18, ea REC = 11.
		// if (recTrk * (recLen + 11) + 18 > trk_len) error...
		return recTrk;
	}

	public String getError() { return errMsg; }
	public int numTracks() { return num_trk; }
	public int numCylinders() { return num_cyl; }
}
