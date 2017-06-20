// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.util.Arrays;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

// True single-path Reader-Punch:
//
// INPUT HOPPER -> [WAIT] -> READ STATION -> PUNCH STATION -> OUTPUT STACKER
//
// For punching "new" cards, INPUT HOPPER must be set to "BLANK" (i.e. no file).
// For reading cards only, OUTPUT STACKER should be set to "DISCARD" (i.e. no file).
//
// The 214-2 "Reader/Punch" is not a dual-path device. I.e. it cannot be used to
// make a copy of a card deck in a single pass. To do this one must:
//	1. Load cards to copy into INPUT HOPPER
//	2. Read cards (optionally modify data) and write to tape (or other media)
//	3. Load BLANK cards into INPUT HOPPER
//	4. Read tape (optionally modify data) and punch new cards
//
// It does, however, permit punching additional data on existing cards.

public class P_CardReaderPunch extends JFrame
		implements Peripheral, SequentialRecordIO, ActionListener, WindowListener {

	private class PunchCardStatus {
		boolean busy = false;
		boolean empty = true;
		boolean error = false;
		boolean illegal = false;
		boolean busy_if_ill = false;
		boolean busy_if_err = false;
		boolean offset_ill = false;
		boolean offset_err = false;
		boolean offset_next = false;
		boolean interrupt = false;
		int code = 0;
		int cards = 0;
		byte[] card;
		JLabel count_pn;
		JLabel deck_pn;
		public PunchCardStatus() {
		}
	}

	PunchCardStatus[] sts;

	boolean readPunch = false;
	File _last = null;
	boolean isOn = false;
	InputStream idev;
	OutputStream odev;
	CharConverter cvt;
	int vUnit;

	public P_CardReaderPunch(CharConverter cvt) {
		super("H214-2 Card Reader/Punch");
		setIconImage(Toolkit.getDefaultToolkit().
			getImage(getClass().getResource("icons/pcd-96.png")));
		this.cvt = cvt;
		_last = new File(System.getProperty("user.dir"));
		sts = new PunchCardStatus[2];
		sts[0] = new PunchCardStatus(); // output - punch
		sts[0].card = new byte[160]; // 80 columns, 16 bits each
		sts[1] = new PunchCardStatus(); // input - reader
		sts[1].card = new byte[160]; // 80 columns, 16 bits each
		sts[0].count_pn = new JLabel();
		sts[0].count_pn.setPreferredSize(new Dimension(75, 20));
		sts[0].count_pn.setOpaque(true);
		sts[0].count_pn.setBackground(Color.white);
		sts[0].deck_pn = new JLabel();
		sts[0].deck_pn.setPreferredSize(new Dimension(400, 20));
		sts[0].deck_pn.setBackground(Color.white);
		sts[0].deck_pn.setOpaque(true);
		sts[0].deck_pn.setText("Discard");
		sts[1].count_pn = new JLabel();
		sts[1].count_pn.setPreferredSize(new Dimension(75, 20));
		sts[1].count_pn.setOpaque(true);
		sts[1].count_pn.setBackground(Color.white);
		sts[1].deck_pn = new JLabel();
		sts[1].deck_pn.setPreferredSize(new Dimension(400, 20));
		sts[1].deck_pn.setBackground(Color.white);
		sts[1].deck_pn.setOpaque(true);
		sts[1].deck_pn.setText("Blank");
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		JPanel pn = new JPanel();
		pn.setLayout(new FlowLayout());
		JButton bt = new JButton("Read Hopper");
		bt.setPreferredSize(new Dimension(150, 20));
		bt.setActionCommand("reader");
		bt.addActionListener(this);
		JPanel pn2 = new JPanel();
		pn2.setPreferredSize(new Dimension(150, 20));
		pn2.setOpaque(false);
		pn.add(bt);
		pn.add(sts[1].deck_pn);
		add(pn);

		pn = new JPanel();
		pn.setLayout(new FlowLayout());
		bt = new JButton("Runout");
		bt.setPreferredSize(new Dimension(150, 20));
		bt.setActionCommand("runout");
		bt.addActionListener(this);
		pn.add(new JLabel("Read"));
		pn.add(sts[1].count_pn);
		pn.add(bt);
		pn.add(sts[0].count_pn);
		pn.add(new JLabel("Punch"));
		add(pn);

		pn = new JPanel();
		pn.setLayout(new FlowLayout());
		bt = new JButton("Punch Stacker");
		bt.setPreferredSize(new Dimension(150, 20));
		bt.setActionCommand("punch");
		bt.addActionListener(this);
		pn.add(bt);
		pn.add(sts[0].deck_pn);
		add(pn);

		addWindowListener(this);
		pack();
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

	public void addInput(InputStream deck, String src, int count) {
		// TODO: reject if idev != null? close it? stack after?
		idev = deck;
		sts[1].deck_pn.setText(src == null ? "Program" : src);
		sts[1].cards = count > 0 ? count : 0;
		sts[1].count_pn.setText(String.format("%d", sts[1].cards));
	}

	private int getCol(PunchCardStatus pcs, int ix) {
		int p = pcs.card[ix * 2] & 0x0ff;
		p |= (pcs.card[ix * 2 + 1] & 0x0ff) << 8;
		return p;
	}

	private void putCol(PunchCardStatus pcs, int ix, int p) {
		pcs.card[ix * 2] = (byte)(p & 0x0ff);
		pcs.card[ix * 2 + 1] = (byte)((p >> 8) & 0x0ff);
	}

	public void io(RWChannel rwc) {
		if (rwc.isInput() && idev == null) {
			// set error? how to handle?
			// same as EOF: no cards available...
			return;
		}
		if (rwc.isInput()) {
			sts[1].busy = true;
		} else {
			sts[0].busy = true;
		}
	}

	public void run(RWChannel rwc) {
		if (rwc.isInput()) {
			if (!sts[1].busy) {
				return;
			}
			doIn(rwc, sts[1]);
			sts[1].busy = false;
		} else {
			if (!sts[0].busy) {
				return;
			}
			doOut(rwc, sts[0]);
			sts[0].busy = false;
		}
	}

	private void putCard(PunchCardStatus pcs) {
		if (odev != null) {
			try {
				odev.write(pcs.card);
			} catch (Exception ee) {
				// TODO: handle exceptions? pass along?
			}
		}
		pcs.empty = true;
		++pcs.cards;
		pcs.count_pn.setText(String.format("%d", pcs.cards));
	}

	private void passCard() {
		System.arraycopy(sts[1].card, 0, sts[0].card, 0, sts[1].card.length);
		sts[1].empty = true;
		sts[0].empty = false;
	}

	private boolean getCard(PunchCardStatus pcs) {
		// 'pcs' must be sts[1]...
		int a = -1;
		if (idev != null) {
			try {
				// only one card read at a time... (?)
				a = idev.read(pcs.card);
			} catch (Exception ee) {
				// TODO: pass along EI/II exceptions
			}
		} else {
			// assume a stack of BLANK cards...
			Arrays.fill(pcs.card, (byte)0);
			a = pcs.card.length;
		}
		if (a < 0) {
			pcs.empty = true;
			// what status to set?
			pcs.count_pn.setText(String.format("%d END", pcs.cards));
			pcs.error = true;
			return false;
		}
		pcs.empty = false;
		++pcs.cards;
		pcs.count_pn.setText(String.format("%d", pcs.cards));
		return true;
	}

	private void runout() {
		while (!sts[0].empty) {
			putCard(sts[0]);
			if (!sts[1].empty) {
				passCard();
			}
		}
	}

	private void vacatePunch() {
		if (sts[0].empty) {
			return;
		}
		// eject card from punch station...
		putCard(sts[0]);
	}

	private void vacateReader() {
		if (sts[1].empty) {
			return;
		}
		// must pass along any current card...
		vacatePunch();
		// move card from read to punch...
		passCard();
	}

	private void fillPunch() {
		if (!sts[0].empty) {
			return;
		}
		// need a card to punch...
		if (sts[1].empty) {
			// load card into read station...
			getCard(sts[1]);
		}
		// move card from read to punch...
		passCard();
	}

	private void doIn(RWChannel rwc, PunchCardStatus pcs) {
		rwc.startCLC();
		// 'pcs' must be sts[1]...
		vacateReader();	// just in case - should be no-op
		if (!getCard(pcs)) {
			return;
		}
		byte m;
		for (int x = 0; x < 80; ++x) {
			boolean stop = false;
			// Must not disturb punctuation...
			int p = getCol(pcs, x);
			if (pcs.code == 2) {
				// TODO: what is proper order...
				m = rwc.writeChar((byte)((p >> 6) & 077));
				// this would probably be an error...
				stop = ((m & 0300) == 0300);
				if (rwc.incrCLC() || stop) {
					break;
				}
				m = rwc.writeChar((byte)p);
				stop = ((m & 0300) == 0300);
			} else {
				int c = cvt.punToHW(p, (pcs.code == 1));
				if (c < 0) {
					pcs.illegal = true;
					c = 0; // TODO: error code?
				}
				m = rwc.writeChar((byte)c);
				stop = ((m & 0300) == 0300);
			}
			if (rwc.incrCLC() || stop) {
				break;
			}
		}
		// make sure we don't read same card twice
		vacateReader();
	}

	public void doOut(RWChannel rwc, PunchCardStatus pcs) {
		rwc.startCLC();
		fillPunch();
		for (int x = 0; x < 80; ++x) {
			int p = 0;
			if (pcs.code == 2) {
				p = (rwc.readChar() & 077) << 6;
				if (rwc.incrCLC()) {
					return;
				}
				p |= (rwc.readChar() & 077);
			} else {
				byte a = rwc.readChar();
				p = cvt.hwToPun(a, (pcs.code == 1));
			}
			putCol(pcs, x, p);
			if (rwc.incrCLC()) {
				return;
			}
		}
		// make sure we don't punch same card twice
		vacatePunch();
	}

	public boolean busy(byte c2) {
		int io = ((c2 & 040) >> 5);
		return sts[io].busy;
	}

	public boolean ctl(RWChannel rwc) {
		// Assume this device cannot read/punch at the same time
		// C3-Cn:
		//	10	Branch if busy
		//	41	Branch if punch check error
		//	42	Branch if illegal punch
		//	70	Control allow OFF
		//	71	Control allow ON
		//	74	Control interrupt OFF
		//	75	Branch if interrupt ON
		// "Branch to A if device unavailable"...
		// TODO: what is "unavailable"?
		//	27	Hollerith code *
		//	26	Special code
		//	25	direct-transcription code
		//	24	Generate busy if illegal punch
		//	23	Generate busy if punch check errors
		//	22	Offset illegal punch cards
		//	21	Offset punch error cards
		//	20	Punch feed read mode (a punch may follow read)
		//	31	Offset current punch card
		boolean branch = false;
		boolean in = rwc.isInput();
		PunchCardStatus pcs;
		if (in) {
			pcs = sts[1];
		} else {
			pcs = sts[0];
		}
		byte[] cx = new byte[]{ rwc.c3, rwc.c4, rwc.c5, rwc.c6, rwc.c7 };
		for (int x = 0; x < rwc.cn - 2; ++x) {
			// some are mode changes, stored
			// Allow multiple conditions?
			switch(cx[x]) {
			case 010:
				// What does "busy" mean in this context?
				if (pcs.busy) {
					branch = true;
				}
				break;
			case 041:
				// never errors?
				if (pcs.error) {
					branch = true;
					pcs.error = false;
				}
				break;
			case 042:
				// TODO: what constitues illegal punch?
				if (pcs.illegal) {
					branch = true;
					pcs.illegal = false;
				}
				break;
			case 027:
				readPunch = false;
				// FALLTHROUGH
			case 026:
			case 025:
				pcs.code = (3 - (cx[x] & 003)); // 0=Hollerith, 1=Spcl, 2=Dir
				break;
			case 024:
				// TODO: what resets this?
				pcs.busy_if_ill = true;
				break;
			case 023:
				// TODO: what resets this?
				pcs.busy_if_err = true;
				break;
			case 022:
				// TODO: what resets this?
				pcs.offset_ill = true;
				break;
			case 021:
				// TODO: what resets this?
				pcs.offset_err = true;
				break;
			case 020:
				// TODO: enforce punch-feed read mode?
				// We operate in this mode automatically now,
				// If a punch follows a read, the card is re-punched.
				// If a read follows a read, send card to stacker.
				// See fillPunch(), vacateReader(), vacatePunch().
				readPunch = true;
				break;
			case 031:
				pcs.offset_next = true;
				break;
			case 074:
				pcs.interrupt = false;
				break;
			case 075:
				if (pcs.interrupt) {
					branch = true;
				}
				break;
			}
		}
		return branch;
	}

	private File pickFile(String purpose) {
		File file = null;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{"pcd"}, new String[]{"Punch Card Deck"}, _last, null);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
			_last = file; // or use dev[unit]?
		}
		return file;
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JButton)) {
			return;
		}
		JButton b = (JButton)e.getSource();
		String c = b.getActionCommand();
		if (c.equals("runout")) {
			runout();
			return;
		}
		String s = "";
		if (c.equals("reader")) {
			s = "Read Hopper";
			if (idev != null) {
				try {
					idev.close();
				} catch (Exception ee) {}
				idev = null;
			}
			sts[1].deck_pn.setText("Blank");
			sts[1].cards = 0;
			sts[1].count_pn.setText("");
		} else if (c.equals("punch")) {
			s = "Punch Stacker";
			if (odev != null) {
				try {
					odev.close();
				} catch (Exception ee) {}
				odev = null;
			}
			sts[0].deck_pn.setText("Discard");
			sts[0].cards = 0;
			sts[0].count_pn.setText(String.format("%d", sts[0].cards));
		}
		File f = pickFile(s);
		if (f == null) {
			return;
		}
		if (c.equals("reader")) {
			try {
				idev = new FileInputStream(f);
			} catch (Exception ee) {
				PopupFactory.warning(this, s, ee.toString());
				return;
			}
			sts[1].deck_pn.setText(f.getName());
			sts[1].count_pn.setText("0");
		} else if (c.equals("punch")) {
			try {
				odev = new FileOutputStream(f);
			} catch (Exception ee) {
				PopupFactory.warning(this, s, ee.toString());
				return;
			}
			sts[0].deck_pn.setText(f.getName());
			sts[0].count_pn.setText("0");
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
		// use unit for PCS code... TBD...
		vUnit = unit;
		return false; // input != output
	}
	public boolean ready() {
		return (odev != null);
	}
	public boolean empty() {
		return (idev == null);
	}
	public boolean rewind() {
		// Rewind not possible
		return true;
	}
	public boolean backspace() {
		// Backspace not possible
		return true;
	}
	public byte[] nextRecord() {
		if (!getCard(sts[1])) {
			return new byte[0]; // like MagTape EOF mark...
		}
		byte[] b = new byte[vUnit == 2 ? 160 : 80];
		// TODO: conversion modes...
		for (int x = 0; x < 80; ++x) {
			int p = getCol(sts[1], x);
			if (vUnit == 2) { // raw mode...
				b[x * 2] = (byte)((p >> 6) & 077);
				b[x * 2 + 1] = (byte)p;
			} else {
				int c = cvt.punToHW(p, (vUnit == 1));
				if (c < 0) {
					c = 0; // TODO: error code?
				}
				b[x] = (byte)c;
			}
		}
		return b;
	}
	public void appendBulk(byte[] buf, int start, int len) {
		for (int x = 0; x < 80; ++x) {
			int p = 0;
			if (vUnit == 2) {
				if (x * 2 + 1 < buf.length) {
					p = (buf[x * 2] & 077) << 6;
					p |= (buf[x * 2 + 1] & 077);
				}
			} else if (x < buf.length) {
				p = cvt.hwToPun(buf[x], (vUnit == 1));
			}
			putCol(sts[0], x, p);
		}
		putCard(sts[0]);
	}
	public void appendRecord(byte[] buf, int start, int len) {
		appendBulk(buf, start, len);
	}
	public void end() {
		// No need to truncate output... by definition output is new file.
	}
}
