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
		Font font = null;
		if (font == null) {
			font = new Font("Monospaced", Font.PLAIN, 12);
		}
		setFont(font);
		JPanel pn = new JPanel();
		pn.setLayout(new FlowLayout());
		JButton bt = new JButton("Read Hopper");
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
		bt = new JButton("Punch Hopper");
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

	public void visible(boolean on) {
		if (on != isOn) {
			isOn = on;
			setVisible(on);
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

	public void io(HW2000 sys) {
		clc = (byte)(sys.getXtra(0) & 027);
		slc = clc + 010;
		c2 = sys.getXtra(1);
		in = ((c2 & 040) == 040);
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		if (in && idev == null) {
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
	}

	private void doIn(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
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
				sys.rawWriteChar(sys.cr[clc], (byte)((p >> 6) & 077));
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
				sys.rawWriteChar(sys.cr[clc], (byte)(p & 077));
			} else {
				int c = sys.pdc.cvt.punToHW(p, (code == 1));
				if (c < 0) {
					illegal = true;
					c = 0; // TODO: error code?
				}
				sys.rawWriteChar(sys.cr[clc], (byte)(c & 077));
			}
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
				break;
			}
		}
		busy = false;
	}

	public void doOut(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
		for (int x = 0; x < 80; ++x) {
			int p = 0;
			if (code == 2) {
				p = (sys.rawReadMem(sys.cr[clc]) & 077) << 6;
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
				p |= (sys.rawReadMem(sys.cr[clc]) & 077);
			} else {
				byte a = sys.rawReadMem(sys.cr[clc]);
				p = sys.pdc.cvt.hwToPun(a, (code == 1));
			}
			putCol(x, p);
			sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
			if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
				break;
			}
		}
		if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
			return;
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
		busy = false;
	}

	public void output(String s) {
	}

	public boolean busy() {
		return busy;
	}

	public void ctl(HW2000 sys) {
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
		boolean in = ((c2 & 040) == PeriphDecode.P_IN);
		for (int x = 2; x < sys.numXtra(); ++x) {
			byte cx = sys.getXtra(x);
			// some are mode changes, stored
			// Allow multiple conditions?
			switch(cx) {
			case 010:
				// What does "busy" mean in this context?
				if (busy) {
					branch = true;
				}
				break;
			case 041:
				// never errors?
				break;
			case 042:
				// TODO: what constitues illegal punch?
				if (error) {
					branch = true;
					error = false;
				}
				break;
			case 027:
			case 026:
			case 025:
				code = (3 - (cx & 003)); // 0=Hollerith, 1=Spcl, 2=Dir
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
		if (branch) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
			return;
		}
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
			}
		} else {
			s = "Punch Hopper";
			if (idev != null) {
				try {
					idev.close();
				} catch (Exception ee) {}
				idev = null;
				out_deck_pn.setText("Discard");
				cardsOut = 0;
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
		} else {
			try {
				odev = new FileOutputStream(f);
			} catch (Exception ee) {
				HW2000FrontPanel.warning(this, s, ee.getMessage());
				return;
			}
			out_deck_pn.setText(f.getName());
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
