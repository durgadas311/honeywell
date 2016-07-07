import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

public class P_MagneticTape extends JFrame
		implements Peripheral, ActionListener, WindowListener {

	byte c4;
	byte c3;
	byte c2;
	int unit;
	int count;
	int clc, slc;
	boolean busy;
	boolean reverse;
	boolean backspace;
	boolean erase;
	boolean fwdspace;
	boolean in;
	boolean beg;
	boolean end;
	boolean prot; // only valid during file choosing
	File _last = null;
	boolean isOn = false;
	RandomAccessFile[] dev;

	JLabel[] stat_pn;
	JLabel[] mnt_pn;

	public P_MagneticTape() {
		super("H204 Magnetic Tape");
		_last = new File(System.getProperty("user.dir"));
		dev = new RandomAccessFile[8];
		busy = false;
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		stat_pn = new JLabel[8];
		mnt_pn = new JLabel[8];

		for (int x = 0; x < 8; ++x) {
			JPanel pn = new JPanel();
			pn.setLayout(new FlowLayout());
			JButton bt = new JButton(String.format("%03o", x));
			bt.setActionCommand(String.format("%d", x));
			bt.addActionListener(this);
			stat_pn[x] = new JLabel();
			stat_pn[x].setPreferredSize(new Dimension(75, 20));
			stat_pn[x].setOpaque(true);
			stat_pn[x].setBackground(Color.white);
			mnt_pn[x] = new JLabel();
			mnt_pn[x].setPreferredSize(new Dimension(400, 20));
			mnt_pn[x].setBackground(Color.white);
			mnt_pn[x].setOpaque(true);
			mnt_pn[x].setText("No Tape");
			pn.add(bt);
			pn.add(stat_pn[x]);
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

	public void io(HW2000 sys) {
		clc = (byte)(sys.getXtra(0) & 027);
		slc = clc + 010;
		int x = 1;
		if (PeriphDecode.isEsc(sys.getXtra(1))) {
			++x;
		}
		c2 = sys.getXtra(x++);
		in = ((c2 & 040) == 040);
		if (x < sys.numXtra()) {
			c3 = sys.getXtra(x++);
			c4 = sys.getXtra(x++);
		} else {
			// Special case defaults, for BOOTSTRAP
			c3 = (byte)060;	// Read forward
			c4 = (byte)000;	// Stop at Rec Mark, Std FMT
					//... or 023 "Load Mode"?
		}
		// C3:
		//	xxxDDD = Tape Drive/Unit DDD
		// (in) 110xxx = Read Forward
		// (in) 010xxx = Read Reverse *
		//(out) 010xxx = Write
		// (in) 100xxx = Space Forward
		// (in) 000xxx = Back Space
		//(out) 000xxx = Erase
		//	1xxxxx = C4 present
		//	11xxxx = C5,C6,C7 present (extended I/O)
		// C4:
		//	000000 = RM, 4x3 (char compression)
		//	000001 = RM, 2x1 (char compression)
		//	000010 = RM, ASCII (subset)
		//	000100 = RM, EBCDIC (subset)
		//	001000 = File Mark Search, 4x3
		//	001001 = File Mark Search, 2x1
		//	010000 = Count Field, 4x3
		//	010001 = Count Field, 2x1
		//	010010 = Count Field, ASCII
		//	010011 = Count Field, Load mode (1x1)
		//	010100 = Count Field, EBCDIC
		//	1xxxxx = 8-bit mode (N/A for tape?)
		unit = c3 & 007;
		reverse = false;
		backspace = false;
		erase = false;
		fwdspace = false;
		count = 0; // use (memory) record mark
		switch(c3 & 070) {
		case 000:
			if (in) {
				// backspace to C4
				backspace = true;
			} else {
				// erase to C4
				erase = true;
			}
			break;
		case 040:
			if (!in) {
				return;
			}
			// space to C4
			fwdspace = true;
			break;
		case 020: // READ REV or WRITE FWD
			if (in) {
				reverse = true;
			}
			break;
		case 060: // READ FWD
			if (!in) {
				return;
			}
			break;
		}
		switch(c4 & 070) {
		case 000:
		default:
			// use (memory) record mark
			break;
		case 010:
			// "file mark search"... what's that?
			// only for SPACE commands? ERASE?
			break;
		case 020:
			// not for SPACE commands? ERASE?
			int c5 = sys.getXtra(x++);
			int c6 = sys.getXtra(x++);
			int c7 = sys.getXtra(x++);
			count = (c5 << 12) | (c6 << 6) | c7;
			break;
		}
		switch(c4 & 007) {
		case 000:
			// std 6-bit char xfer
			break;
		case 001:
			// unknown "2x1" mode (8bits packed in 6+2?)
			break;
		case 002:
			// ASCII subset - TBD
			break;
		case 003: // only for count field...
			// "Load Mode" - "1x1" - TBD
			break;
		case 004:
			// EBCDIC subset - TBD
			break;
		}
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		if (dev[unit] == null) {
			// set error?
			return;
		}
		busy = true;
	}

	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		if ((c2 & 040) == PeriphDecode.P_OUT) {
			doOut(sys);
		} else {
			doIn(sys);
		}
		try {
			stat_pn[unit].setText(
				String.format("%d", dev[unit].getFilePointer()));
		} catch (Exception ee) {}
	}

	private void doIn(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
		int a = -1;
		long fp = 0;
		try {
			if (backspace) {
				fp = dev[unit].getFilePointer();
				if (fp == 0) {
					busy = false;
					return;
				}
			}
			do {
				if (backspace) {
					if (--fp == 0) {
						break;
					}
					dev[unit].seek(fp);
				}
				a = dev[unit].read();
				if (a < 0) {
					break;
				}
				if ((a & 0300) == 0300) {
					// caller must look at CLC (CLC - SLC).
					// (CLC == SLC) means EOF (File Mark)
					if (backspace) {
						// TODO: proper location to leave...
						dev[unit].seek(fp);
					}
					break;
				}
				if (fwdspace || backspace) {
					continue;
				}
				sys.rawWriteChar(sys.cr[clc], (byte)(a & 077));
				a = sys.rawReadMem(sys.cr[clc]) & 0300;
				if ((count == 0 && a == 0300) ||
						(count > 0 && --count == 0)) {
					do {
						a = dev[unit].read();
					} while (a >= 0 && (a & 0300) != 0300);
					break;
				}
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
			} while (true); // can't rely on memory contents?
		} catch (Exception ee) {
			// TODO: pass along EI/II exceptions
		}
		busy = false;
	}

	public void doOut(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
		try {
			while (true) {
				byte a = sys.rawReadMem(sys.cr[clc]);
				if (count == 0 && (a & 0300) == 0300) {
					// "zero-length record" just means EOF,
					// a.k.a. "File Mark".
					dev[unit].write((byte)0300);
					break;
				}
				a &= 077;
				dev[unit].write(a);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
				// This does not permit 0-char xfers
				if (count > 0 && --count == 0) {
					dev[unit].write((byte)0300);
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

	public boolean busy(byte c2) {
		return busy;
	}

	public void ctl(HW2000 sys) {
		// C3:
		//	xxxDDD = Tape Drive/Unit DDD
		// (in) 010xxx = Rewind, release
		//(out) 010xxx = Rewind
		// (in) 000xxx = Branch if read busy
		//(out) 000xxx = Branch if write busy
		//      100xxx = Branch if read/write error
		// (in) 110xxx = Branch if beginning
		//(out) 110xxx = Branch if end (phy?)
		//	111000 = Control allow OFF
		//	111001 = Control allow ON
		//	111100 = Control interrupt OFF
		//	111101 = Branch if control interrupt ON
		if (busy) { // always tested...
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
			return;
		}
		int x = 1;
		if (PeriphDecode.isEsc(sys.getXtra(1))) {
			++x;
		}
		if (x < sys.numXtra()) {
			c2 = sys.getXtra(x++);
			boolean in = ((c2 & 040) == PeriphDecode.P_IN);
			c3 = sys.getXtra(x++);
			unit = c3 & 007;
			switch(c3 & 070) {
			case 070:
				// nothing?
				return;
			case 060:
				if ((in && beg) || (!in && end)) {
					sys.BAR = sys.SR;
					sys.SR = sys.AAR;
					return;
				}
				break;
			case 040:
				// never any R/W errors?
				break;
			case 020:
				if (in) {
					// close, unmount
					try {
						dev[unit].close();
					} catch (Exception ee) {}
					dev[unit] = null;
				} else {
					// rewind
					try {
						dev[unit].seek(0L);
					} catch (Exception ee) {}
				}
				break;
			case 000:
				// never busy, from here?
				break;
			}
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
			stat_pn[c].setText("");
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
			stat_pn[c].setText("0");
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
