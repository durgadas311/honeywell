import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

public class P_Disk extends JFrame
		implements Peripheral, ActionListener, WindowListener {

	// These are all stored by io() and used during run()...
	byte c2;
	byte c3;
	int unit;
	int clc, slc;
	boolean busy;
	boolean in;
	File _last = null;
	boolean isOn = false;
	RandomAccessFile[] dev;
	int[] cyl;
	boolean[] bsy;

	// The address register:
	int adr_cyl;
	int adr_trk;
	int adr_rec;
	// Current head position:
	int curr_pos;
	int curr_rec;
	int curr_len;

	JLabel[] cyl_pn;
	JLabel[] mnt_pn;

	// cache:
	byte[] track;
	int track_cyl;
	int track_trk;

	// 11 platters, 20 surfaces
	// 200 cyls (0000-0312 or 203?)
	// 4602 char/trk
	public P_Disk() {
		super("H274 Disk Devices");
		_last = new File(System.getProperty("user.dir"));
		dev = new RandomAccessFile[8];
		busy = false;
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		cyl = new int[8];
		bsy = new boolean[8];
		cyl_pn = new JLabel[8];
		mnt_pn = new JLabel[8];
		track = new byte[4700];
		track_cyl = -1;
		track_trk = -1;

		for (int x = 0; x < 8; ++x) {
			JPanel pn = new JPanel();
			pn.setLayout(new FlowLayout());
			JButton bt = new JButton(String.format("%03o", x));
			bt.setActionCommand(String.format("%d", x));
			bt.addActionListener(this);
			cyl_pn[x] = new JLabel();
			cyl_pn[x].setPreferredSize(new Dimension(50, 20));
			cyl_pn[x].setOpaque(true);
			cyl_pn[x].setBackground(Color.white);
			mnt_pn[x] = new JLabel();
			mnt_pn[x].setPreferredSize(new Dimension(400, 20));
			mnt_pn[x].setBackground(Color.white);
			mnt_pn[x].setOpaque(true);
			mnt_pn[x].setText("No Disk Pack");
			pn.add(bt);
			pn.add(cyl_pn[x]);
			pn.add(mnt_pn[x]);
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
		// 'track' is never dirty - always write when changed
		if (track_cyl >= 0 && track_trk >= 0 &&
				track_cyl == cyl && track_trk == trk) {
			return true;
		}
		int n = -1;
		if (dev[unit] != null) {
			try {
				n = dev[unit].read(track);
			} catch (Exception ee) {}
		}
		if (n < 0) {
			return  false;
		}
		track_cyl = cyl;
		track_trk = trk;
		curr_pos = 0;
		curr_rec = -1;
		curr_len = 0;
	}

	private boolean findRecord() {
		boolean extended = ((c3 & 020) == 020);
		boolean verify = ((c3 & 010) == 010);
		if (!cacheTrack(cyl[unit], adr_trk)) {
			return false;
		}
		// TODO: check TLR and skip to new track
		switch(c3 & 007) {
		case 000: // Initial
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

	public void io(HW2000 sys) {
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
			doAdrReg();
			return;
		}
		if (dev[unit] == null) {
			error = true;
			return;
		}
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
	}

	private void doIn(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
		if (!findRecord()) {
			error = true;
			busy = false;
			return;
		}
		// 'curr_pos' points to first data byte.
		for (int x = 0; x < curr_len; ++x) {
			int a = track[curr_pos + x];
			// check for errors, like premature end?
			sys.rawWriteMem(sys.cr[clc], (byte)(a & 077));
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
				break;
			}
		}
		curr_pos += curr_len + 1;
		if (curr_pos >= track.length) {
			curr_pos = 0;
		}
		busy = false;
	}

	public void doOut(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
		try {
			while (true) {
				byte a = sys.rawReadMem(sys.cr[clc]);
				if ((a & 0300)  == 0300) {
					dev[unit].write(term);
					break;
				}
				a &= 077;
				dev[unit].write(a);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
			}
		} catch (Exception ee) {
			// TODO: handle exceptions? pass along?
		}
		busy = false;
	}

	public void output(String s) {
	}

	public boolean busy() {
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
			unit = cx & 007;
			if (bsy[unit]) {
				branch = true;
			}
		} else if ((c3 & 070) == 020) { // && sys.getXtra(x + 1) == 0 ?
			unit = cx & 007;
			if (bsy[unit]) {
				branch = true;
			} else {
				cyl[unit] = (sys.getXtra(x + 2) << 6) | sys.getXtra(x + 3);
				cyl_pn[unit].setText(String.format("%d", cyl[unit]));
			}
		} else for (;x < sys.numXtra(); ++x) {
			byte cx = sys.getXtra(x);
			if (in) {
				if ((cx & 070) == 030) {
					unit = cx & 007;
					cyl[unit] = 0;
					cyl_pn[unit].setText("0");
				}
				continue;
			}
			switch(cx & 070) {
			case 070:
				// handle interrupt control
				return;
			case 060:
				if (tlr) {
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
		SuffFileChooser ch = new SuffFileChooser(purpose, "mti", "Mag Tape Img", _last, 2);
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
		int c = b.getActionCommand().charAt(0) - '0';
		String s = String.format("Mount %03o", c);
		if (dev[c] != null) {
			try {
				dev[c].close();
			} catch (Exception ee) {}
			dev[c] = null;
			cyl_pn[c].setText("");
			mnt_pn[c].setText("No Tape");
		}
		prot = false;
		File f = pickFile(s);
		if (f == null) {
			return;
		}
		try {
			// TODO: allow write-protect
			dev[c] = new RandomAccessFile(f, prot ? "r" : "rw");
			end = false;
			beg = true;
			cyl_pn[c].setText("0");
			mnt_pn[c].setText(f.getName());
			return;
		} catch (Exception ee) {
			HW2000FrontPanel.warning(this, s, ee.getMessage());
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
