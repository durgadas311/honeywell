import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.Arrays;

public class P_LinePrinter extends JFrame
		implements Peripheral, ActionListener, WindowListener {

	OutputStream dev;
	byte c3;
	int clc, slc;
	boolean busy;
	JTextArea text;
	JScrollPane scroll;
	java.util.concurrent.LinkedBlockingDeque<Integer> kq;
	int carr;
	int col;
	int ln;
	File _last = null;
	boolean isOn = false;
	byte[] ftape;

	public P_LinePrinter() {
		super("H222 Line Printer");
		_last = new File(System.getProperty("user.dir"));
		ftape = new byte[66];
		Arrays.fill(ftape, (byte)0);
		ftape[0] = 1;	// HOF
		ftape[65] = 2;	// EOF - how many lines?
		ftape[64] = 2;	//
		ftape[63] = 2;	//
		ftape[62] = 2;	//
		// TODO: how to represent forms shorter than (not equal to) 66 lines.
		dev = null;
		busy = false;
		setLayout(new FlowLayout());
		setFocusable(false);
		text = new JTextArea(66, 132);
		text.setEditable(false); // this prevents caret... grrr.
		text.setBackground(Color.white);
		Font font = null;
		String fn = "fonts/HW222.ttf";
		java.io.InputStream ttf = this.getClass().getResourceAsStream(fn);
		if (ttf != null) {
			try {
				Font f = Font.createFont(Font.TRUETYPE_FONT, ttf);
				font = f.deriveFont(10f);
			} catch (Exception ee) {}
		}
		if (font == null) {
			font = new Font("Monospaced", Font.PLAIN, 12);
		}
		text.setFont(font);
		scroll = new JScrollPane(text);
		scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		add(scroll);

		JMenuBar mb = new JMenuBar();
		JMenu mu = new JMenu("Paper");
		JMenuItem mi = new JMenuItem("Line Feed", KeyEvent.VK_N);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Form Feed", KeyEvent.VK_F);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Save", KeyEvent.VK_S);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Tear Off", KeyEvent.VK_T);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Log File", KeyEvent.VK_G);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		mu = new JMenu("Form Ctl");
		mi = new JMenuItem("Edit", KeyEvent.VK_E);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Save", KeyEvent.VK_V);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Load", KeyEvent.VK_L);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);
		setJMenuBar(mb);

		addWindowListener(this);
		pack();
		tearOff();
	}

	private void tearOff() {
		text.setText("");
		carr = 0;
		col = 0;
		ln = 0;
	}

	public void setOutput(OutputStream dev) {
		// TODO: handle output redirection
		this.dev = dev;
	}

	public OutputStream getOutput() {
		return this.dev;
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

	public void io(HW2000 sys) {
		clc = (byte)(sys.getXtra(0) & 027);
		slc = clc + 010;
		if ((sys.getXtra(1) & 030) == 010) {
			c3 = sys.getXtra(3);
		} else {
			c3 = sys.getXtra(2);
		}
		// C3:
		//	00nnnn: Print then advance nnnn lines.
		//	01nnnn: Print then advance nnnn lines unless HOF, etc.
		//	11nnnn: Do not print, advance nnnn lines.
		//	100xxx: Print, advance to channel xxx.
		//	101xxx: Do not print, advance to channel xxx.
		// NOTE: AAR was checked for protection violation in I_PDT.
		// No further checks will be made.
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		busy = true;
	}

	// Must protect against exceptions, and eventually throw them to main thread...
	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		sys.cr[clc] = sys.cr[slc];
		// Cannot depend on any processor state here.
		// The processor may be running a completely different program.
		String s = "";
		boolean print = ((c3 & 040) == 0 || (c3 & 030) == 0);
		// Printing stops *before* char with record mark...
		try {
			while (print) {
				byte a = sys.rawReadMem(sys.cr[clc]);
				if ((a & 0300)  == 0300) {
					break;
				}
				a &= 077;
				if (col >= 132) {
					s += "\n";
					col = 0;
					if (++ln >= ftape.length) ln = 0;
				}
				s += sys.pdc.cvt.hwToLP(a);
				++col;
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
			}
			if ((c3 & 060) == 040) {
				// special forms-advance
				int ch = 1;
				if ((c3 & 003) != 003) {
					ch = c3 & 007;
					ch += 3;
					if (ch > 5) {
						ch -= 1;
					}
				}
				ch = (1 << (ch - 1));
				ch |= 1; // always stop at HOF?
				while ((ftape[ln] & ch) == 0) {
					s += "\n";
					if (++ln >= ftape.length) ln = 0;
				}
			} else {
				if ((c3 & 060) == 020) {
					if ((ftape[ln] & 2) != 0) { // in EOF area?
						while ((ftape[ln] & 1) == 0) {
							s += "\n";
							if (++ln >= ftape.length) ln = 0;
						}
						col = 0;
						c3 = 0; // cancel any other action
					}
				}
				c3 &= 017;
				while (c3 > 0) {
					s += "\n";
					--c3;
					col = 0;
					if (++ln >= ftape.length) ln = 0;
				}
			}
			if (dev != null) {
				dev.write(s.getBytes());
			}
			text.append(s);
			carr += s.length();
			text.setCaretPosition(carr);
			visible(true);
		} catch (Exception ee) {
			// TODO: handle exceptions? How to pass along EI/II exceptions to CPU?
		}
		busy = false;
	}

	public void output(String s) {
		if (dev != null) {
			try {
				dev.write(s.getBytes());
			} catch (Exception ee) {}
		}
		text.append(s);
		carr += s.length();
		text.setCaretPosition(carr);
		visible(true);
	}

	public boolean busy() {
		return busy;
	}

	public void ctl(HW2000 sys) {
		if (busy) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
			return;
		}
		// TODO: apply control chars
	}

	private File pickFile(String purpose, String typ, String dsc) {
		File file = null;
		SuffFileChooser ch;
		if (typ == null) {
			ch = new SuffFileChooser(purpose, _last, 0);
		} else {
			ch = new SuffFileChooser(purpose, typ, dsc, _last, 0);
		}
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		}
		return file;
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_S) {
			File sav = pickFile("Save", null, null);
			if (sav != null) {
				try {
					FileOutputStream fo = new FileOutputStream(sav);
					fo.write(text.getText().getBytes());
					fo.close();
					_last = sav;
					// TODO: tear off?
				} catch (Exception ee) {
					HW2000FrontPanel.warning(this, "Save", ee.getMessage());
				}
			}
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_T) {
			tearOff();
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_N) {
			if (++ln >= ftape.length) ln = 0;
			text.append("\n");
			carr += 1;
			text.setCaretPosition(carr);
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_F) {
			String s = "";
			while ((ftape[ln] & 1) == 0) {
				s += "\n";
				if (++ln >= ftape.length) ln = 0;
			}
			text.append(s);
			carr += s.length();
			text.setCaretPosition(carr);
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_G) {
			// TODO: implement this?
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_E) {
			FormTapeEditor fte = new FormTapeEditor(this, ftape);
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_V) {
			File sav = pickFile("Save Form Ctl", "fct", "Form Ctl Tape");
			if (sav != null) {
				try {
					FileOutputStream fo = new FileOutputStream(sav);
					fo.write(ftape);
					fo.close();
					_last = sav;
				} catch (Exception ee) {
					HW2000FrontPanel.warning(this, "Save Form Ctl", ee.getMessage());
				}
			}
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_L) {
			File sav = pickFile("Load Form Ctl", "fct", "Form Ctl Tape");
			if (sav != null) {
				try {
					FileInputStream fi = new FileInputStream(sav);
					int n = fi.available(); // file size?
					if (n > 150) n = 150; // some arbitrary sanity
					ftape = new byte[n];
					fi.read(ftape);
					fi.close();
					_last = sav;
					// TODO: protect against blank (channel 1) tape?
				} catch (Exception ee) {
					HW2000FrontPanel.warning(this, "Load Form Ctl", ee.getMessage());
				}
			}
			return;
		}
	}

	public void windowActivated(WindowEvent e) { }
	public void windowClosed(WindowEvent e) { }
	public void windowIconified(WindowEvent e) { }
	public void windowOpened(WindowEvent e) { }
	public void windowDeiconified(WindowEvent e) { }
	public void windowDeactivated(WindowEvent e) { }
	public void windowClosing(WindowEvent e) {
		if (!(e.getSource() instanceof JFrame)) {
			return;
		}
		JFrame f = (JFrame)e.getSource();
		if (f.equals(this)) {
			isOn = false;
			setVisible(false);
			return;
		}
		if (f instanceof FormTapeEditor) {
			FormTapeEditor fte = (FormTapeEditor)f;
			if (fte.isChanged()) {
				int ans = HW2000FrontPanel.confirm("Quit", "Save Changes?");
				if (ans == JOptionPane.YES_OPTION) {
					ftape = fte.getTape();
				}
			}
		}
	}
}
