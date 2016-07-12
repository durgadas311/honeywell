import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

public class P_CardReaderPunch extends JFrame
		implements Peripheral, ActionListener, WindowListener {

	byte c2;
	int clc, slc;
	boolean busy;
	boolean in;
	File _last = null;
	boolean isOn = false;
	InputStream idev;
	OutputStream odev;
	byte[] card;
	int cardsIn;
	int cardsOut;

	boolean error = false;
	boolean illegal = false;
	boolean busy_if_ill = false;
	boolean busy_if_err = false;
	boolean offset_ill = false;
	boolean offset_err = false;
	boolean loopback = false;
	boolean offset_next = false;
	boolean interrupt = false;
	int code = 0;

	JLabel in_count_pn;
	JLabel in_deck_pn;
	JLabel out_count_pn;
	JLabel out_deck_pn;

	public P_CardReaderPunch() {
		super("H214 Card Reader/Punch");
		_last = new File(System.getProperty("user.dir"));
		busy = false;
		card = new byte[160]; // 80 columns, 16 bits each
		cardsIn = 0;
		cardsOut = 0;
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		JPanel pn = new JPanel();
		pn.setLayout(new FlowLayout());
		JButton bt = new JButton("Read Hopper");
		bt.setPreferredSize(new Dimension(150, 20));
		bt.setActionCommand("reader");
		bt.addActionListener(this);
		in_count_pn = new JLabel();
		in_count_pn.setPreferredSize(new Dimension(75, 20));
		in_count_pn.setOpaque(true);
		in_count_pn.setBackground(Color.white);
		in_deck_pn = new JLabel();
		in_deck_pn.setPreferredSize(new Dimension(400, 20));
		in_deck_pn.setBackground(Color.white);
		in_deck_pn.setOpaque(true);
		in_deck_pn.setText("Empty");
		pn.add(bt);
		pn.add(in_count_pn);
		pn.add(in_deck_pn);
		add(pn);
		// TODO: support non-BLANK cards in punch?
		pn = new JPanel();
		pn.setLayout(new FlowLayout());
		bt = new JButton("Punch Stacker");
		bt.setPreferredSize(new Dimension(150, 20));
		bt.setActionCommand("punch");
		bt.addActionListener(this);
		out_count_pn = new JLabel();
		out_count_pn.setPreferredSize(new Dimension(75, 20));
		out_count_pn.setOpaque(true);
		out_count_pn.setBackground(Color.white);
		out_deck_pn = new JLabel();
		out_deck_pn.setPreferredSize(new Dimension(400, 20));
		out_deck_pn.setBackground(Color.white);
		out_deck_pn.setOpaque(true);
		out_deck_pn.setText("Discard");
		pn.add(bt);
		pn.add(out_count_pn);
		pn.add(out_deck_pn);
		add(pn);

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

	private int getCol(int ix) {
		int p = card[ix * 2] & 0x0ff;
		p |= (card[ix * 2 + 1] & 0x0ff) << 8;
		return p;
	}

	private void putCol(int ix, int p) {
		card[ix * 2] = (byte)(p & 0x0ff);
		card[ix * 2 + 1] = (byte)((p >> 8) & 0x0ff);
	}

	public void io(RWChannel rwc) {
		in = ((rwc.c2 & 040) == 040);
		if (in && idev == null) {
			// set error? how to handle?
			// same as EOF: no cards available...
			return;
		}
		busy = true;
	}

	public void run(RWChannel rwc) {
		if (!busy) {
			return;
		}
		if ((rwc.c2 & 040) == PeriphDecode.P_OUT) {
			doOut(rwc);
		} else {
			doIn(rwc);
		}
		busy = false;
	}

	private void doIn(RWChannel rwc) {
		rwc.startCLC();
		int a = -1;
		if (idev != null) {
			try {
				// only one card read at a time... (?)
				a = idev.read(card);
			} catch (Exception ee) {
				// TODO: pass along EI/II exceptions
			}
		}
		if (a < 0) {
			// what status to set?
			in_count_pn.setText(String.format("%d END", cardsIn));
			error = true;
			return;
		}
		++cardsIn;
		in_count_pn.setText(String.format("%d", cardsIn));
		for (int x = 0; x < 80; ++x) {
			// Must not disturb punctuation...
			int p = getCol(x);
			if (code == 2) {
				// TODO: proper order...
				rwc.writeChar((byte)((p >> 6) & 077));
				if (rwc.incrCLC()) {
					break;
				}
				rwc.writeChar((byte)p);
			} else {
				int c = rwc.sys.pdc.cvt.punToHW(p, (code == 1));
				if (c < 0) {
					illegal = true;
					c = 0; // TODO: error code?
				}
				rwc.writeChar((byte)c);
			}
			if (rwc.incrCLC()) {
				break;
			}
		}
	}

	public void doOut(RWChannel rwc) {
		rwc.startCLC();
		for (int x = 0; x < 80; ++x) {
			int p = 0;
			if (code == 2) {
				p = (rwc.readChar() & 077) << 6;
				if (rwc.incrCLC()) {
					return;
				}
				p |= (rwc.readChar() & 077);
			} else {
				byte a = rwc.readChar();
				p = rwc.sys.pdc.cvt.hwToPun(a, (code == 1));
			}
			putCol(x, p);
			if (rwc.incrCLC()) {
				return;
			}
		}
		if (odev != null) {
			try {
				odev.write(card);
			} catch (Exception ee) {
				// TODO: handle exceptions? pass along?
			}
		}
		++cardsOut;
		out_count_pn.setText(String.format("%d", cardsOut));
	}

	public void output(String s) {
	}

	public boolean busy(byte c2) {
		return busy;
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
		//	20	Punch feed read mode (loopback?)
		//	31	Offset current punch card
		boolean branch = false;
		boolean in = ((rwc.c2 & 040) == PeriphDecode.P_IN);
		byte[] cx = new byte[]{ rwc.c3, rwc.c4, rwc.c5, rwc.c6, rwc.c7 };
		for (int x = 0; x < rwc.cn - 2; ++x) {
			// some are mode changes, stored
			// Allow multiple conditions?
			switch(cx[x]) {
			case 010:
				// What does "busy" mean in this context?
				if (busy) {
					branch = true;
				}
				break;
			case 041:
				// never errors?
				if (error) {
					branch = true;
					error = false;
				}
				break;
			case 042:
				// TODO: what constitues illegal punch?
				if (illegal) {
					branch = true;
					illegal = false;
				}
				break;
			case 027:
				loopback = false;
				// FALLTHROUGH
			case 026:
			case 025:
				code = (3 - (cx[x] & 003)); // 0=Hollerith, 1=Spcl, 2=Dir
				break;
			case 024:
				// TODO: what resets this?
				busy_if_ill = true;
				break;
			case 023:
				// TODO: what resets this?
				busy_if_err = true;
				break;
			case 022:
				// TODO: what resets this?
				offset_ill = true;
				break;
			case 021:
				// TODO: what resets this?
				offset_err = true;
				break;
			case 020:
				// TODO: what resets this?
				loopback = true;
				break;
			case 031:
				offset_next = true;
				break;
			case 074:
				interrupt = false;
				break;
			case 075:
				if (interrupt) {
					branch = true;
				}
				break;
			}
		}
		return branch;
	}

	private File pickFile(String purpose) {
		File file = null;
		SuffFileChooser ch = new SuffFileChooser(purpose, "pcd", "Punch Card Deck", _last, 0);
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
		String s;
		if (c.equals("reader")) {
			s = "Read Hopper";
			if (idev != null) {
				try {
					idev.close();
				} catch (Exception ee) {}
				idev = null;
				in_deck_pn.setText("Empty");
				cardsIn = 0;
				in_count_pn.setText("");
			}
		} else {
			s = "Punch Stacker";
			if (odev != null) {
				try {
					odev.close();
				} catch (Exception ee) {}
				odev = null;
				out_deck_pn.setText("Discard");
				cardsOut = 0;
				out_count_pn.setText(String.format("%d", cardsOut));
			}
		}
		File f = pickFile(s);
		if (f == null) {
			return;
		}
		if (c.equals("reader")) {
			try {
				idev = new FileInputStream(f);
			} catch (Exception ee) {
				HW2000FrontPanel.warning(this, s, ee.getMessage());
				return;
			}
			in_deck_pn.setText(f.getName());
			in_count_pn.setText("0");
		} else {
			try {
				odev = new FileOutputStream(f);
			} catch (Exception ee) {
				HW2000FrontPanel.warning(this, s, ee.getMessage());
				return;
			}
			out_deck_pn.setText(f.getName());
			out_count_pn.setText("0");
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
