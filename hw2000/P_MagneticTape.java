// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

// TODO: should we be using the same error codes as Disk?

// Basic 9-track tape stats:
//	Standard reel was 2400ft.
//	Small reel assumed to be 800ft.
//	"frame" is a single 1x9 bit value on tape.
// H204D:
//	4x6-bit chars stored in 3 frames.
//	800/1600 bpi (use 1600bpi here).
//	Data rate 105 ips (~6uS per frame) (204D-5).
//	Rewind speed unknown (300 ips assumed).
//	IRG is 0.6 in. (960 frames, 1280 H-chars).
// Raw length computations use "bits", i.e. H-chars * 6.

public class P_MagneticTape extends JFrame
		implements Peripheral, SequentialRecordIO,
			ActionListener, WindowListener {

	// Timing constants, based on stats.
	static final int UPF = 6; // uSecs/frame R/W speed
	static final int MPI = 4; // mSecs/inch rewind speed

	// We always assume 1600bpi recording density...
	static final int FPI = 1600; // frames/in a.k.a. "bpi"
	static final int BPI = FPI * 8; // bits per inch, 8x frames/inch
	static final int IRG = ((BPI * 6) / 10); // bits/gap (0.6in)

	private class MagTapeStatus {
		RandomAccessFile dev;
		long len; // approximate length of unspooled tape
		boolean beg;
		boolean end;
		boolean wrRing;
		boolean permit;
		boolean in;
		int count;
		javax.swing.Timer busy;
		boolean read;
		boolean write;
		boolean reverse;
		boolean backspace;
		boolean erase;
		boolean error;
		int errno;
		boolean fwdspace;
		JButton perm_bt;
		JLabel stat_pn;
		JLabel mnt_pn;

		public MagTapeStatus() {
			dev = null;
			beg = end = permit = wrRing = false;
			read = write = false;
			count = 0;
			len = 0;
		}
	}
	MagTapeStatus[] sts;
	int vUnit = 0;	// unit in-use by virtual client
	byte[] vBuf = null;
	boolean vWritten = false;

	JCheckBox wp;	// write *permit* (write-ring inserted)
	File _last = null;
	boolean isOn = false;
	HW2000 sys;
	int irq;
	boolean allow;
	boolean intr;
	boolean canceled;
	boolean active;

	public P_MagneticTape(int irq, HW2000 hw) {
		super("H204B Magnetic Tape Unit");
		java.net.URL url = getClass().getResource("icons/mti-96.png");
		if (url != null) {
			setIconImage(Toolkit.getDefaultToolkit().getImage(url));
		}
		_last = new File(System.getProperty("user.dir"));
		this.irq = irq;
		this.sys = hw;
		sts = new MagTapeStatus[8];
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		wp = new JCheckBox("Write Ring");

		for (int x = 0; x < 8; ++x) {
			sts[x] = new MagTapeStatus();
			sts[x].busy = new javax.swing.Timer(1, this);
			sts[x].busy.setActionCommand(String.format("%d", x));
			sts[x].busy.setRepeats(false);
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
			bt = new JButton("PERMIT");
			bt.setMargin(new Insets(2, 1, 2, 1));
			bt.setBackground(Peripheral.btnWhiteOff);
			bt.setActionCommand(String.format("P%d", x));
			bt.addActionListener(this);
			pn.add(bt);
			sts[x].perm_bt = bt;
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
		allow = false;
		intr = false;

		addWindowListener(this);
		pack();
	}

	// embedded version, no GUI
	public P_MagneticTape() {
		super("embedded");	// not used
	}

	public synchronized void cancel() {
		if (active) canceled = true;
	}

	public void reset() {
		allow = false;
		intr = false;
	}

	public void setInterrupt() {
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

	private void updateDisp(int unit) {
		try {
			long p = sts[unit].dev.getFilePointer();
			sts[unit].stat_pn.setText(String.format("%d", p));
			sts[unit].beg = (p == 0);
			if (sts[unit].beg) {
				sts[unit].end = false;
			}
		} catch (Exception ee) {}
	}

	public void io(RWChannel rwc) {
		boolean in = rwc.isInput();
		if (sys.bootstrap) {
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
		//      xx1xxx = Even Parity
		// ??? conflicts with Read Forward/Space Forward:
		//	1xxxxx = C4 present
		//	11xxxx = C4,C5,C6,C7 present (extended I/O)
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
		//		RM => num chars in "record header area"
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
		if (in) {
			sts[unit].read = true;
		} else {
			sts[unit].write = true;
		}
	}

	public void run(RWChannel rwc) {
		synchronized(this) {
			if (canceled) {
				canceled = false;
				return;
			}
			active = true;
		}
		int unit = rwc.c3 & 007;
		long cnt = sts[unit].len;
		if ((rwc.c2 & 040) == PeriphDecode.P_OUT) {
			doOut(rwc, sts[unit]);
			sts[unit].write = false;
		} else {
			doIn(rwc, sts[unit]);
			sts[unit].read = false;
		}
		if (cnt < sts[unit].len) {
			cnt = sts[unit].len - cnt;
		} else {
			cnt -= sts[unit].len;
		}
		cnt /= 8; // HW packs 4x6bit chars in 3x8bit frames
		if (cnt < 1) cnt = 1;
		cnt *= UPF;
		if (!canceled) {
			try {
				Thread.sleep(cnt / 1000, (int)(cnt % 1000));
			} catch (Exception ee) {}
			if (allow) {
				intr = true;
				sys.CTL.setPC(irq);
			}
		}
		updateDisp(unit);
		synchronized(this) {
			active = false;
			canceled = false;
		}
	}

	private void doIn(RWChannel rwc, MagTapeStatus unit) {
		if (!unit.read) {
			return;
		}
		rwc.startCLC();
		int a = -1;
		long fp = 0;
		try {
			// TODO: READ REVERSE: CLC/SLC is *rightmost* char,
			// and must rwc.decrCLC()... and RM is leftmost?
			if (unit.backspace) {
				// normally, fp will be char after IRG,
				// must backup before IRG to do proper
				// backspace. Tape should always be positioned
				// at first char of record (or next IRG for file mark)
				fp = unit.dev.getFilePointer();
				if (fp == 0) return;
				unit.dev.seek(--fp);
				if (fp == 0) return;	// TODO: might not be IRG?
				unit.len -= IRG;	// TODO: might not be IRG?
			}
			do {
				if (unit.backspace) {
					if (--fp == 0) {
						unit.len = 0;
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
						unit.len -= IRG;
						// right now, file points *after* IRG,
						// that's where we want it.
					} else {
						unit.len += IRG;
					}
					break;
				}
				if (unit.backspace) {
					unit.len -= 6;
				} else {
					unit.len += 6;
				}
				if (unit.fwdspace || unit.backspace) {
					continue;
				}
				a = (rwc.writeChar((byte)a) & 0300);
				if (rwc.incrCLC()) {
					break;
				}
				if ((unit.count == 0 && a == 0300) ||
						(unit.count > 0 && --unit.count == 0)) {
					do {
						a = unit.dev.read();
						unit.len += 6;
					} while (a >= 0 && (a & 0300) != 0300);
					unit.len += IRG;
					break;
				}
			} while (!canceled);
		} catch (Exception ee) {
			// TODO: pass along EI/II exceptions
		}
		if (unit.len < 0) unit.len = 0;
	}

	public void doOut(RWChannel rwc, MagTapeStatus unit) {
		if (!unit.write) {
			return;
		}
		rwc.startCLC();
		if (!unit.wrRing || !unit.permit) {
			unit.errno = 00502;
			unit.error = true;
			return;
		}
		try {
			while (!canceled) {
				byte a = rwc.readMem();
				if (unit.count == 0 && (a & 0300) == 0300) {
					// "zero-length record" just means EOF,
					// a.k.a. "File Mark".
					unit.len += IRG;
					unit.dev.write((byte)0300);
					break;
				}
				unit.len += 6;
				a &= 077;
				unit.dev.write(a);
				if (rwc.incrCLC()) {
					break;
				}
				// This does not permit 0-char xfers
				if (unit.count > 0 && --unit.count == 0) {
					unit.len += IRG;
					unit.dev.write((byte)0300);
					break;
				}
			}
		} catch (Exception ee) {
			// TODO: handle exceptions? pass along?
		}
	}

	public boolean busy(byte c2) {
		// TODO: can we provide this?
		return false;
	}

	private boolean chkBusy(int unit, boolean in) {
		// TODO: does rewind get checked?
		//if (sts[unit].busy.isRunning()) {
		//	return true;
		//}
		if (in && sts[unit].read) {
			return true;
		}
		if (!in && sts[unit].write) {
			return true;
		}
		return false;
	}

	private void startRew(int unit) {
		long len = 0;
		sts[unit].stat_pn.setText("...");
		sts[unit].end = false;
		sts[unit].beg = false;
		len = (sts[unit].len / BPI); // approx. inches
		if (len < 1) len = 1;
		sts[unit].busy.setDelay((int)len * MPI);
		sts[unit].busy.start();
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
			case 1:
				// allow ON/OFF
				allow = ((rwc.c3 & 1) != 0);
				break;
			case 4:
				// interrupt OFF
				sys.CTL.clrPC(irq);
				intr = false;
				break;
			case 5:
				// branch if interrupt ON
				if (intr) {
					branch = true;
				}
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
			// never any R/W errors? write permit errors...
			branch = sts[unit].error;
			sts[unit].error = false;
			break;
		case 020:
			unit = rwc.c3 & 007;
			if (in) {
				// close, unmount
				try {
					sts[unit].dev.close();
				} catch (Exception ee) {}
				sts[unit].dev = null;
			}
			startRew(unit);
			break;
		case 000:
			unit = rwc.c3 & 007;
			if (chkBusy(unit, in)) {
				branch = true;
			}
			break;
		}
		return branch;
	}

	private File pickFile(String purpose) {
		File file = null;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{"mti"}, new String[]{"Mag Tape Img"}, _last, wp);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
			_last = file; // or use dev[unit]?
		}
		return file;
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof javax.swing.Timer) {
			// Rewind/release is finished...
			int un = e.getActionCommand().charAt(0) - '0';
			sts[un].len = 0;
			if (sts[un].dev != null) { // rewind, no release...
				try {
					sts[un].dev.seek(0L);
				} catch (Exception ee) {}
				sts[un].stat_pn.setText("0");
				sts[un].beg = true;
			} else {
				sts[un].stat_pn.setText("");
				sts[un].mnt_pn.setText("No Tape");
			}
			return;
		}
		if (!(e.getSource() instanceof JButton)) {
			return;
		}
		JButton b = (JButton)e.getSource();
		char a = b.getActionCommand().charAt(0);
		if (a == 'R') {
			int c = b.getActionCommand().charAt(1) - '0';
			if (sts[c].dev != null) {
				startRew(c);
			}
		} else if (a == 'P') {
			int c = b.getActionCommand().charAt(1) - '0';
			if (sts[c].dev != null) {
				sts[c].permit = !sts[c].permit;
				sts[c].perm_bt.setBackground(
					sts[c].permit  && sts[c].wrRing ?
					Peripheral.btnWhiteOn :
					Peripheral.btnWhiteOff);
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
				sts[c].wrRing = false;
				sts[c].permit = false;
				sts[c].perm_bt.setBackground(Peripheral.btnWhiteOff);
			}
			wp.setSelected(false);
			File f = pickFile(s);
			if (f == null) {
				return;
			}
			try {
				sts[c].wrRing = wp.isSelected();
				sts[c].dev = new RandomAccessFile(f, "rw");
				sts[c].end = false;
				sts[c].beg = true;
				sts[c].stat_pn.setText("0");
				sts[c].mnt_pn.setText(f.getName());
				sts[c].perm_bt.setBackground(
					sts[c].permit  && sts[c].wrRing ?
					Peripheral.btnWhiteOn :
					Peripheral.btnWhiteOff);
				return;
			} catch (Exception ee) {
				PopupFactory.warning(this, s, ee.toString());
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

	// TODO: mutex with PDC/PCB...
	public boolean begin(int unit) {
		vUnit = unit & 07;
		vWritten = false;
		sts[vUnit].errno = 0;
		return true;
	}
	public boolean ready() {
		return (sts[vUnit].dev != null);
	}
	public boolean empty() {
		if (sts[vUnit].dev == null) {
			sts[vUnit].errno = 00501;
			return false; // error, actually
		}
		try {
			return (sts[vUnit].dev.length() == 0);
		} catch (Exception ee) {
			sts[vUnit].errno = 00501;
		}
		return false; // error, actually
	}
	public boolean rewind() {
		if (sts[vUnit].dev == null) {
			sts[vUnit].errno = 00501;
			return false;
		}
		try {
			sts[vUnit].dev.seek(0);
		} catch (Exception ee) {
			sts[vUnit].errno = 00501;
			return false;
		}
		updateDisp(vUnit);
		return true;
	}
	public boolean backspace() {
		if (sts[vUnit].dev == null) {
			sts[vUnit].errno = 00501;
			return false;
		}
		try {
			// must go back two tape marks, then fwd one.
			int c = 2;
			int b;
			long fp = sts[vUnit].dev.getFilePointer();
			while (fp > 0 && c > 0) {
				--fp;
				sts[vUnit].dev.seek(fp);
				b = sts[vUnit].dev.read();
				if (b < 0) {
					fp = sts[vUnit].dev.length();
					if (fp > 0) {
						--fp;
					}
					continue;
				}
				if ((b & 0300) == 0300) {
					// at this point, we are positioned correctly...
					// ...if c becomes 0...
					--c;
				}
			}
			if (fp == 0) {
				// assume we did not start here...
				sts[vUnit].dev.seek(fp);
			}
		} catch (Exception ee) {
			sts[vUnit].errno = 00501;
			return false;
		}
		updateDisp(vUnit);
		return true;
	}
	public byte[] nextRecord() {
		if (sts[vUnit].dev == null) {
			sts[vUnit].errno = 00501;
			return null;
		}
		byte[] ret = null;
		try {
			int n = 0;
			while (true) {
				int b = sts[vUnit].dev.read();
				if (b < 0 || (b & 0300) == 0300) {
					break;
				}
				if (vBuf == null || n >= vBuf.length) {
					byte[] nb = new byte[n + 256];
					System.arraycopy(vBuf, 0, nb, 0, vBuf.length);
					vBuf = nb;
				}
				vBuf[n++] = (byte)b;
			}
			ret = new byte[n];
			System.arraycopy(vBuf, 0, ret, 0, n);
		} catch (Exception ee) {
			sts[vUnit].errno = 00501;
			return null;
		}
		updateDisp(vUnit);
		return ret;
	}
	public boolean appendBulk(byte[] buf, int start, int len) {
		if (sts[vUnit].dev == null) {
			sts[vUnit].errno = 00501;
			return false;
		}
		if (!sts[vUnit].wrRing || !sts[vUnit].permit) {
			sts[vUnit].errno = 00502;
			return false;
		}
		vWritten = true;
		if (len < 0) {
			len = buf.length - start;
		}
		if (len == 0) {
			return true;
		}
		try {
			sts[vUnit].dev.write(buf, start, len);
		} catch (Exception ee) {
			sts[vUnit].errno = 00501;
			return false;
		}
		updateDisp(vUnit);
		return true;
	}
	public boolean appendRecord(byte[] buf, int start, int len) {
		if (sts[vUnit].dev == null) {
			sts[vUnit].errno = 00501;
			return false;
		}
		if (!sts[vUnit].wrRing || !sts[vUnit].permit) {
			sts[vUnit].errno = 00502;
			return false;
		}
		if (!appendBulk(buf, start, len)) {
			return false;
		}
		try {
			sts[vUnit].dev.write((byte)0300);
		} catch (Exception ee) {
			sts[vUnit].errno = 00501;
			return false;
		}
		updateDisp(vUnit);
		return true;
	}
	public void end() {
		if (sts[vUnit].dev == null) {
			return;
		}
		if (vWritten) try {
			sts[vUnit].dev.write((byte)0300);
			// Truncate to new EOT...
			sts[vUnit].dev.setLength(sts[vUnit].dev.getFilePointer());
		} catch (Exception ee) {}
		updateDisp(vUnit);
	}
	public int getError() { return sts[vUnit].errno; }
}
