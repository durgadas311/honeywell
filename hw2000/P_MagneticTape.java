import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

public class P_MagneticTape extends JFrame
		implements Peripheral, ActionListener, WindowListener {

	private class MagTapeStatus {
		RandomAccessFile dev;
		boolean beg;
		boolean end;
		boolean in;
		int count;
		boolean busy;
		boolean reverse;
		boolean backspace;
		boolean erase;
		boolean fwdspace;
		JLabel stat_pn;
		JLabel mnt_pn;

		public MagTapeStatus() {
			busy = false;
			dev = null;
			beg = end = false;
			count = 0;
		}
	}
	MagTapeStatus[] sts;

	boolean prot; // only valid during file choosing
	File _last = null;
	boolean isOn = false;

	public P_MagneticTape() {
		super("H204 Magnetic Tape");
		_last = new File(System.getProperty("user.dir"));
		sts = new MagTapeStatus[8];
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);

		for (int x = 0; x < 8; ++x) {
			sts[x] = new MagTapeStatus();
			JPanel pn = new JPanel();
			pn.setLayout(new FlowLayout());
			JButton bt = new JButton(String.format("%03o", x));
			bt.setActionCommand(String.format("%d", x));
			bt.addActionListener(this);
			pn.add(bt);
			bt = new JButton("\u25c4\u25c4");
			bt.setActionCommand(String.format("R%d", x));
			bt.addActionListener(this);
			pn.add(bt);
			sts[x].stat_pn = new JLabel();
			sts[x].stat_pn.setPreferredSize(new Dimension(75, 20));
			sts[x].stat_pn.setOpaque(true);
			sts[x].stat_pn.setBackground(Color.white);
			pn.add(sts[x].stat_pn);
			sts[x].mnt_pn = new JLabel();
			sts[x].mnt_pn.setPreferredSize(new Dimension(400, 20));
			sts[x].mnt_pn.setBackground(Color.white);
			sts[x].mnt_pn.setOpaque(true);
			sts[x].mnt_pn.setText("No Tape");
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

	public void io(RWChannel rwc) {
		boolean in = rwc.isInput();
		if (rwc.sys.bootstrap) {
			// Special case defaults, for BOOTSTRAP
			rwc.c3 = (byte)060;	// Read forward, unit 0
			rwc.c4 = (byte)000;	// Stop at Rec Mark, Std FMT
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
		//	000001 = RM, 2x1 (char compression) **
		//	000010 = RM, ASCII (subset)
		//	000100 = RM, EBCDIC (subset)
		//	001000 = File Mark Search, 4x3
		//	001001 = File Mark Search, 2x1 **
		//	010000 = Count Field, 4x3
		//	010001 = Count Field, 2x1 **
		//	010010 = Count Field, ASCII
		//	010011 = Count Field, Load mode (1x1) ***
		//	010100 = Count Field, EBCDIC
		//	1xxxxx = 8-bit mode (N/A for tape?)
		//
		// * Special hardware option.
		// ** Packed-decimal? 2 digits per tape "byte"? signs? fields?
		// *** Full punctuation transfer?
		int unit = rwc.c3 & 007;
		sts[unit].reverse = false;
		sts[unit].backspace = false;
		sts[unit].erase = false;
		sts[unit].fwdspace = false;
		sts[unit].count = 0; // use (memory) record mark
		switch(rwc.c3 & 070) {
		case 000:
			if (in) {
				// backspace to C4
				sts[unit].backspace = true;
			} else {
				// erase to C4
				sts[unit].erase = true;
			}
			break;
		case 040:
			if (!in) {
				return;
			}
			// space to C4
			sts[unit].fwdspace = true;
			break;
		case 020: // READ REV or WRITE FWD
			if (in) {
				sts[unit].reverse = true;
			}
			break;
		case 060: // READ FWD
			if (!in) {
				return;
			}
			break;
		}
		switch(rwc.c4 & 070) {
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
			sts[unit].count = (rwc.c5 << 12) | (rwc.c6 << 6) | rwc.c7;
			break;
		}
		switch(rwc.c4 & 007) {
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
		if (sts[unit].dev == null) {
			// set error?
			return;
		}
		sts[unit].busy = true;
	}

	public void run(RWChannel rwc) {
		int unit = rwc.c3 & 007;
		if (!sts[unit].busy) {
			return;
		}
		if ((rwc.c2 & 040) == PeriphDecode.P_OUT) {
			doOut(rwc, sts[unit]);
		} else {
			doIn(rwc, sts[unit]);
		}
		try {
			long p = sts[unit].dev.getFilePointer();
			sts[unit].stat_pn.setText(String.format("%d", p));
			sts[unit].beg = (p == 0);
			if (sts[unit].beg) {
				sts[unit].end = false;
			}
		} catch (Exception ee) {}
		sts[unit].busy = false;
	}

	private void doIn(RWChannel rwc, MagTapeStatus unit) {
		rwc.startCLC();
		int a = -1;
		long fp = 0;
		try {
			if (unit.backspace) {
				fp = unit.dev.getFilePointer();
				if (fp == 0) {
					return;
				}
			}
			do {
				if (unit.backspace) {
					if (--fp == 0) {
						break;
					}
					unit.dev.seek(fp);
				}
				a = unit.dev.read();
				if (a < 0) {
					// EOF: end of tape
					unit.end = true;
					break;
				}
				if ((a & 0300) == 0300) {
					// caller must look at CLC (CLC - SLC).
					// (CLC == SLC) means EOF (File Mark)
					if (unit.backspace) {
						// TODO: proper location to leave...
						unit.dev.seek(fp);
					}
					break;
				}
				if (unit.fwdspace || unit.backspace) {
					continue;
				}
				rwc.writeChar((byte)a);
				a = rwc.readMem() & 0300;
				if (rwc.incrCLC()) {
					break;
				}
				if ((unit.count == 0 && a == 0300) ||
						(unit.count > 0 && --unit.count == 0)) {
					do {
						a = unit.dev.read();
					} while (a >= 0 && (a & 0300) != 0300);
					break;
				}
			} while (true);
		} catch (Exception ee) {
			// TODO: pass along EI/II exceptions
		}
	}

	public void doOut(RWChannel rwc, MagTapeStatus unit) {
		rwc.startCLC();
		try {
			while (true) {
				byte a = rwc.readMem();
				if (unit.count == 0 && (a & 0300) == 0300) {
					// "zero-length record" just means EOF,
					// a.k.a. "File Mark".
					unit.dev.write((byte)0300);
					break;
				}
				a &= 077;
				unit.dev.write(a);
				if (rwc.incrCLC()) {
					break;
				}
				// This does not permit 0-char xfers
				if (unit.count > 0 && --unit.count == 0) {
					unit.dev.write((byte)0300);
					break;
				}
			}
		} catch (Exception ee) {
			// TODO: handle exceptions? pass along?
		}
	}

	public String input(HW2000 sys) { return null; }

	public void output(String s) { }

	public boolean busy(byte c2) {
		// without unit number, we are not busy?
		return false;
	}

	public boolean ctl(RWChannel rwc) {
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

		boolean branch = false;
		if (rwc.cn < 2) {
			// no check of device?
			return branch;
		}
		boolean in = rwc.isInput();
		int unit;
		switch(rwc.c3 & 070) {
		case 070:
			switch(rwc.c3 & 007) {
			case 0:
				// allow OFF
				break;
			case 1:
				// allow ON
				break;
			case 4:
				// interrupt OFF
				break;
			case 5:
				// branch if interrupt ON
				break;
			}
			break;
		case 060:
			unit = rwc.c3 & 007;
			if ((in && sts[unit].beg) || (!in && sts[unit].end)) {
				branch = true;
			}
			break;
		case 040:
			unit = rwc.c3 & 007;
			// never any R/W errors?
			break;
		case 020:
			unit = rwc.c3 & 007;
			if (in) {
				// close, unmount
				try {
					sts[unit].dev.close();
				} catch (Exception ee) {}
				sts[unit].dev = null;
				sts[unit].stat_pn.setText("");
				sts[unit].mnt_pn.setText("No Tape");
				sts[unit].end = false;
				sts[unit].beg = false;
			} else {
				// rewind
				try {
					sts[unit].dev.seek(0L);
				} catch (Exception ee) {}
				sts[unit].stat_pn.setText("0");
				sts[unit].end = false;
				sts[unit].beg = true;
			}
			break;
		case 000:
			unit = rwc.c3 & 007;
			if (sts[unit].busy) {
				branch = true;
			}
			break;
		}
		return branch;
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
		char a = b.getActionCommand().charAt(0);
		if (a == 'R') {
			int c = b.getActionCommand().charAt(1) - '0';
			if (sts[c].dev != null) {
				try {
					sts[c].dev.seek(0L);
					sts[c].stat_pn.setText("0");
				} catch (Exception ee) {}
			}
		} else {
			int c = a - '0';
			String s = String.format("Mount %03o", c);
			if (sts[c].dev != null) {
				try {
					sts[c].dev.close();
				} catch (Exception ee) {}
				sts[c].dev = null;
				sts[c].stat_pn.setText("");
				sts[c].mnt_pn.setText("No Tape");
			}
			prot = false;
			File f = pickFile(s);
			if (f == null) {
				return;
			}
			try {
				// TODO: allow write-protect
				sts[c].dev = new RandomAccessFile(f, prot ? "r" : "rw");
				sts[c].end = false;
				sts[c].beg = true;
				sts[c].stat_pn.setText("0");
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
}
