import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.Arrays;

// Semantics/Rationale For I/O are described in DiskSemantics.txt
//
// Disk Format:
//	Index is implied at track[0]
//	[index][AM][header][DM][data][AM]...[DM][data][EM][garbage]
//
public class P_Disk extends JFrame
		implements Peripheral, ActionListener, WindowListener {
	static final byte AM = (byte)0304; // start of record header
	static final byte DM = (byte)0305; // start of record data
	static final byte EM = (byte)0306; // end of valid track formatting

	static final int trk_len = 4700;
	static final int num_trk = 20;
	static final int cyl_len = trk_len * num_trk;

	private class DiskStatus {
		RandomAccessFile dev;
		int cyl;
		boolean busy;
		byte flag;
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
	byte c2;
	byte c3;
	int unit;
	int clc, slc;
	boolean busy;
	boolean in;
	boolean format = false; // switch setting, per drive?
	boolean extended = false;
	boolean verify = false;
	boolean initial = false;
	boolean error = false;
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
	int track_cyl;
	int track_trk;
	long track_pos;
	boolean track_dirty;

	boolean prot; // only valid immediately after pickFile()

	// 11 platters, 20 surfaces
	// 200 cyls (0000-0312 or 203?)
	// 4602 char/trk
	public P_Disk() {
		super("H274 Disk Devices");
		_last = new File(System.getProperty("user.dir"));
		busy = false;
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);

		sts = new DiskStatus[8];
		track = new byte[trk_len];
		track_cyl = -1;
		track_trk = -1;

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
			bt = new JButton("B");
			bt.setActionCommand(String.format("B%d", x));
			bt.addActionListener(this);
			bt.setBackground(HW2000FrontPanel.btnWhiteOff);
			bt.setMargin(new Insets(0, 0, 0, 0));
			bt.setPreferredSize(new Dimension(25, 25));
			pn.add(bt);
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

	public void setOutput(OutputStream dev) {
	}

	public OutputStream getOutput() {
		return null;
	}

	public void setInput(InputStream dev) {
	}

	public InputStream getInput() {
		return null;
	}

	public void reset() {
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
		if (track_cyl >= 0 && track_trk >= 0 &&
				track_cyl == cyl && track_trk == trk) {
			return true;
		}
		int n = -1;
		if (sts[unit].dev != null) {
			try {
				// already know track_cyl != cyl || track_trk != trk...
				if (track_dirty) {
					sts[unit].dev.seek(track_pos);
					sts[unit].dev.write(track);
				}
				track_dirty = false;
				if (cyl < 0 || trk < 0) {
					return true;
				}
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
		sts[unit].cyl_pn.setText(String.format("%d.%d", cyl, trk));
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

	private void getHeaderMem(int a) {
		adr_cyl = (sys.rawReadMem(a++) & 077) << 6;
		adr_cyl |= (sys.rawReadMem(a++) & 077);
		adr_trk = (sys.rawReadMem(a++) & 077) << 6;
		adr_trk |= (sys.rawReadMem(a++) & 077);
		adr_rec = (sys.rawReadMem(a++) & 077) << 6;
		adr_rec |= (sys.rawReadMem(a++) & 077);
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

	private void newRecord() {
		if (curr_pos + 11 >= track.length) {
			error = true;
			return; // need to return error...
		}
		track[curr_pos++] = AM;
		++curr_rec;
		track[curr_pos++] = curr_flg;
		track[curr_pos++] = (byte)((curr_cyl >> 6) & 077);
		track[curr_pos++] = (byte)(curr_cyl & 077);
		track[curr_pos++] = (byte)((curr_trk >> 6) & 077);
		track[curr_pos++] = (byte)(curr_trk & 077);
		track[curr_pos++] = (byte)((curr_rec >> 6) & 077);
		track[curr_pos++] = (byte)(curr_rec & 077);
		track[curr_pos++] = (byte)((curr_len >> 6) & 077);
		track[curr_pos++] = (byte)(curr_len & 077);
		track[curr_pos++] = DM;
		curr_end = curr_pos + curr_len;
		if (curr_end + 1 >= track.length) {
			error = true;
			return; // need to return error...
		}
	}

	// At the very least, this puts curr_pos at the first data char.
	private boolean findRecord() {
		if (!cacheTrack(sts[unit].cyl, adr_trk)) {
			return false;
		}
		// TODO: check TLR and skip to new track
		switch(c3 & 007) {
		case 000: // Initial
			curr_pos = 0;
			break;
		case 001: // (Standard)
			break;
		case 003: // Search Next
			++adr_rec;
			// FALLTHROUGH
		case 002: // Search
			if (!searchRecord()) {
				return false;
			}
			break;
		}
		if (!searchData()) {
			return false;
		}
		return true;
	}

	private void doAdrReg() {
		if (in) {
			sys.rawWriteChar(sys.cr[clc], (byte)(adr_cyl >> 6));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			sys.rawWriteChar(sys.cr[clc], (byte)(adr_cyl));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			sys.rawWriteChar(sys.cr[clc], (byte)(adr_trk >> 6));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			sys.rawWriteChar(sys.cr[clc], (byte)(adr_trk));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			sys.rawWriteChar(sys.cr[clc], (byte)(adr_rec >> 6));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			sys.rawWriteChar(sys.cr[clc], (byte)(adr_rec));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
		} else {
			adr_cyl = (sys.rawReadMem(sys.cr[clc]) & 077) << 6;
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			adr_cyl |= (sys.rawReadMem(sys.cr[clc]) & 077);
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			adr_trk = (sys.rawReadMem(sys.cr[clc]) & 077) << 6;
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			adr_trk |= (sys.rawReadMem(sys.cr[clc]) & 077);
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			adr_rec = (sys.rawReadMem(sys.cr[clc]) & 077) << 6;
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			adr_rec |= (sys.rawReadMem(sys.cr[clc]) & 077);
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
		}
	}

	public void io(HW2000 sys) {
		this.sys = sys;
		clc = (byte)(sys.getXtra(0) & 027);
		slc = clc + 010;
		int x = 1;
		if (PeriphDecode.isEsc(sys.getXtra(1))) {
			++x;
		}
		c2 = sys.getXtra(x++);
		c3 = sys.getXtra(x++); // operation
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
		in = ((c2 & 040) == PeriphDecode.P_IN);
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		// Perform load/store address register now...
		if (c3 == 004) {
			sys.cr[clc] = sys.cr[slc];
			doAdrReg();
			return;
		}
		if (sts[unit].dev == null) {
			error = true;
			return;
		}
		sts[unit].busy = true;
		busy = true;
	}

	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		if (in) {
			doIn(sys);
		} else {
			doOut(sys);
		}
		// cyl not changed by run()...
		// but if we track record/sector...
		sts[unit].busy = false;
		busy = false;
	}

	private boolean checkEOR() {
		// can't depend on AM since data could be 8-bit.
		if (curr_pos >= curr_end) { // end of record
			if (!extended || (curr_flg & 040) != 0) {
				// If we started with a TLR, stop now.
				return false;
			}
			++adr_rec;
			if (!searchRecord()) {
				return false;
			}
			if ((curr_flg & 040) != 0) { // TLR
				// TODO: must be same cylinder...
				adr_cyl = (track[curr_pos++] & 077) << 6;
				adr_cyl |= (track[curr_pos++] & 077);
				adr_trk = (track[curr_pos++] & 077) << 6;
				adr_trk |= (track[curr_pos++] & 077);
				adr_rec = (track[curr_pos++] & 077) << 6;
				adr_rec |= (track[curr_pos++] & 077);
				if (!cacheTrack(adr_cyl, adr_trk)) {
					return false;
				}
				if (!searchRecord()) {
					return false;
				}
			}
		}
		return true;
	}

	private int readChar() {
		if (!checkEOR()) {
			return -1;
		}
		int c = track[curr_pos] & 0377;
		++curr_pos;
		return c;
	}

	private int writeChar(byte c) {
		if (!checkEOR()) {
			return -1;
		}
		track[curr_pos] = c;
		track_dirty = true;
		++curr_pos;
		return 0;
	}

	private void doIn(HW2000 sys) {
		extended = ((c3 & 020) == 020);
		verify = ((c3 & 010) == 010);
		format = ((c3 & 002) == 000);
		initial = ((c3 & 003) == 000);
		sys.cr[clc] = sys.cr[slc];
		if (format) {
			cacheTrack(adr_cyl, adr_trk);
			if (initial) {
				curr_pos = 0;
			}
			if (!searchHeader()) {
				error = true;
				return;
			}
			++curr_pos;
			getHeader(curr_pos);
			for (int x = 0; x < 9; ++x) {
				// TODO: strip punc, check RM?
				sys.rawWriteMem(sys.cr[clc], track[curr_pos++]);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					error = true;
					return;
				}
			}
			if (!searchData()) {
				error = true;
				return;
			}
		} else if (!findRecord()) {
			error = true;
			return;
		}
		// 'curr_pos' points to first data byte.
		while (true) {
			int a = 0;
			int b = sys.rawReadMem(sys.cr[clc]) & 0300;
			if (format) {
				if (curr_pos >= track.length) {
					curr_pos = 0;
					break;
				}
				if (curr_pos >= curr_end) {
					if (!extended) {
						break;
					}
					if (track[curr_pos] == EM) {
						break;
					}
					if (!searchHeader()) {
						error = true;
						return;
					}
					// TODO: getHeader?
					// assert curr_len same?
					// check TLR and stop?
					if (!searchData()) {
						error = true;
						return;
					}
				}
				a = track[curr_pos++];
			} else if (b != 0300) {
				a = readChar(); // has side affects
				if (a < 0) { // no more data in cylinder
					// need to signal short read?
					break;
				}
			}
			// TODO: support 8-bit transfers?
			sys.rawWriteChar(sys.cr[clc], (byte)(a & 077));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
				break;
			}
			if (b == 0300) {
				curr_pos = curr_end;
				break;
			}
		}
	}

	public void doOut(HW2000 sys) {
		extended = ((c3 & 020) == 020);
		verify = ((c3 & 010) == 010);
		format = ((c3 & 002) == 000);
		initial = ((c3 & 003) == 000);
		sys.cr[clc] = sys.cr[slc];
		if (format) {
			track_dirty = true;
			getHeaderMem(sys.cr[clc] + 1); // loads adr_*
			cacheTrack(adr_cyl, adr_trk);
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
			track[curr_pos++] = AM; // pad with gap?
			for (int x = 0; x < 9; ++x) {
				// TODO: strip punc, check RM?
				track[curr_pos++] = (byte)(sys.rawReadMem(sys.cr[clc]) & 077);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					error = true;
					return;
				}
			}
			getHeader(curr_pos - 9); // loads curr_* variables
			track[curr_pos++] = DM;
			curr_end = curr_pos + curr_len;
			if (curr_end + 1 >= track.length) {
				error = true;
				return;
			}
		} else if (!findRecord()) {
			error = true;
			return;
		}
		while (!error) {
			byte a = sys.rawReadMem(sys.cr[clc]);
			// TODO: how does extended fit with RM check?
			if ((a & 0300)  == 0300) {
				if (format) {
					while (curr_pos < curr_end) {
						track[curr_pos++] = 0;
					}
					track[curr_pos++] = DM;
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
					if (extended) {
						newRecord();
					} else {
						break;
					}
				}
				track[curr_pos++] = a;
				e = 0;
			} else {
				e = writeChar(a);
			}
			if (e < 0) { // no more space in cylinder...
				error = true; // not an error?
				break;
			}
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
				break;
			}
		}
		if (format) {
			// need to replace this next record with AM,
			// do not increment.
			track[curr_pos] = EM;
		}
		cacheTrack(-1, -1);
	}

	public void output(String s) {
	}

	public boolean busy(byte c2) {
		return busy;
	}

	public void ctl(HW2000 sys) {
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
		int x = 1;
		if (PeriphDecode.isEsc(sys.getXtra(1))) {
			++x;
		}
		boolean in = ((sys.getXtra(x++) & 040) == PeriphDecode.P_IN); // C2
		byte c3 = sys.getXtra(x);
		if ((c3 & 070) == 000) { // && sys.getXtra(x + 1) == 0 ?
			unit = c3 & 007;
			if (sts[unit].busy) {
				branch = true;
			}
		} else if ((c3 & 070) == 020) { // && sys.getXtra(x + 1) == 0 ?
			unit = c3 & 007;
			if (sts[unit].busy) {
				branch = true;
			} else {
				sts[unit].cyl = (sys.getXtra(x + 2) << 6) | sys.getXtra(x + 3);
				sts[unit].cyl_pn.setText(String.format("%d", sts[unit].cyl));
			}
		} else for (;x < sys.numXtra(); ++x) {
			byte cx = sys.getXtra(x);
			if (in) {
				if ((cx & 070) == 030) {
					unit = cx & 007;
					sts[unit].cyl = 0;
					sts[unit].cyl_pn.setText("0");
				}
				continue;
			}
			switch(cx & 070) {
			case 070:
				// handle interrupt control
				return;
			case 060:
				if ((curr_flg & 040) != 0) {
					branch = true;
				}
				break;
			case 050:
				if (error) {
					branch = true;
					error = false;
				}
				break;
			case 040:
				format = true;
				break;
			}
		}
		if (branch) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
	}

	private File pickFile(String purpose) {
		File file = null;
		SuffFileChooser ch = new SuffFileChooser(purpose, "dpi", "Disk Pack Img", _last, 2);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
			prot = ch.wantWrProt();
			_last = file; // or use dev[unit]?
		}
		return file;
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JButton)) {
			return;
		}
		JButton b = (JButton)e.getSource();
		String a = b.getActionCommand();
		if (a.charAt(0) == 'A') {
			int c = a.charAt(1) - '0';
			sts[c].flag ^= 004;
			if ((sts[c].flag & 004) == 0) {
				b.setBackground(HW2000FrontPanel.btnWhiteOff);
			} else {
				b.setBackground(HW2000FrontPanel.btnWhiteOn);
			}
		} else if (a.charAt(0) == 'B') {
			int c = a.charAt(1) - '0';
			sts[c].flag ^= 010;
			if ((sts[c].flag & 010) == 0) {
				b.setBackground(HW2000FrontPanel.btnWhiteOff);
			} else {
				b.setBackground(HW2000FrontPanel.btnWhiteOn);
			}
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
			prot = false;
			File f = pickFile(s);
			if (f == null) {
				return;
			}
			try {
				sts[c].dev = new RandomAccessFile(f, prot ? "r" : "rw");
				sts[c].cyl = 0;
				sts[c].cyl_pn.setText("0");
				sts[c].mnt_pn.setText(f.getName());
				return;
			} catch (Exception ee) {
				HW2000FrontPanel.warning(this, s, ee.getMessage());
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
}
