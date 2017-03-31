import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

public class P_Console extends JFrame
		implements Peripheral, KeyListener, ActionListener, WindowListener {
	private class ConsoleStatus {
		boolean busy;
		public ConsoleStatus() {
			busy = false;
		}
	}

	InputStream idev;
	OutputStream odev;
	ConsoleStatus[] sts;
	JTextArea text;
	JScrollPane scroll;
	java.util.concurrent.LinkedBlockingDeque<Integer> kq;
	int carr;
	int col;
	File _last = null;
	boolean isOn = false;
	boolean interrupt = false;
	boolean dataTerm = false;
	int io; // only valid between io() and run()
	JLabel type;

	public P_Console() {
		super("H220 Console");
		kq = new java.util.concurrent.LinkedBlockingDeque<Integer>();
		_last = new File(System.getProperty("user.dir"));
		odev = null;
		idev = null;
		sts = new ConsoleStatus[2];
		sts[0] = new ConsoleStatus();
		sts[1] = new ConsoleStatus();
		setLayout(new FlowLayout());
		text = new JTextArea(24, 64);
		text.setEditable(false); // this prevents caret... grrr.
		text.setBackground(Color.white);
		Font font = null;
		String fn = "fonts/HW222.ttf";
		java.io.InputStream ttf = this.getClass().getResourceAsStream(fn);
		if (ttf != null) {
			try {
				Font f = Font.createFont(Font.TRUETYPE_FONT, ttf);
				font = f.deriveFont(12f);
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
		JMenuItem mi = new JMenuItem("Save", KeyEvent.VK_S);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Tear Off", KeyEvent.VK_T);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Log File", KeyEvent.VK_F);
		mi.addActionListener(this);
		mu.add(mi);
		mb.add(mu);

		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 30));
		pn.setOpaque(true);
		pn.setBackground(Color.black);
		type = new JLabel("TYPE");
		type.setForeground(HW2000FrontPanel.indDark);
		pn.add(type);
		mb.add(pn);
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(400, 30));
		mb.add(pn);

		setJMenuBar(mb);

		addWindowListener(this);
		text.addKeyListener(this);
		pack();
		tearOff();
	}

	private void tearOff() {
		text.setText("\u2588");
		carr = 0;
		col = 0;
	}

	public void setOutput(OutputStream dev) {
		// TODO: handle output redirection
	}

	public OutputStream getOutput() {
		return this.odev;
	}

	public void setInput(InputStream dev) {
		// TODO: handle input redirection
	}

	public InputStream getInput() {
		return this.idev;
	}

	public void reset() {
		if (sts[1].busy) {
			kq.add(-1);
		}
	}

	// I.e. Front Panel switch
	public void setInterrupt(HW2000 sys) {
		// This is only a one-shot interrupt condition?
		// interrupt = true;
		sys.CTL.setEI(HW2000CCR.EIR_CONS);
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
		// C3:
		//	000000: no CR/LF
		//	000001: CR (LF?)
		// TODO: does c3 output CR/LF on input?
		int io = ((rwc.c2 & 040) >> 5);
		sts[io].busy = true;
	}

	public void run(RWChannel rwc) {
		int io = ((rwc.c2 & 040) >> 5);
		if (!sts[io].busy) {
			return;
		}
		if (io == 0) {
			doOut(rwc, sts[io]);
		} else {
			doIn(rwc, sts[io]);
		}
	}

	private void doIn(RWChannel rwc, ConsoleStatus unit) {
		type.setForeground(HW2000FrontPanel.indLit);
		rwc.startCLC();
		int a = -1;
		byte b;
		try {
			// TODO: what effect does c3 have? Print CR/LF before input?
			// Or... use CR as termination of input?
			while ((rwc.readMem() & 0300) != 0300) {
				byte c = getChar(rwc.sys);
				if (c < 0) {
					// caller must check CLC (CLC - SLC)
					break;
				}
				// Must not disturb punctuation
				rwc.writeChar(c);
				if (rwc.incrCLC()) {
					break;
				}
			}
		} catch (Exception ee) {
			// TODO: pass along EI/II exceptions
		}
		unit.busy = false;
		type.setForeground(HW2000FrontPanel.indDark);
	}

	public void doOut(RWChannel rwc, ConsoleStatus unit) {
		rwc.startCLC();
		String s = "";
		boolean print = true;
		// Printing stops *before* char with record mark...
		try {
			while (print) {
				byte a = rwc.readMem();
				if ((a & 0300)  == 0300) {
					break;
				}
				a &= 077;
				if (col >= 64) {
					s += "\n";
					col = 0;
				}
				s += rwc.sys.pdc.cvt.hwToLP(a);
				++col;
				if (rwc.incrCLC()) {
					break;
				}
			}
			if (rwc.c3 != 0) {
				s += "\n";
				col = 0;
			}
			if (odev != null) {
				odev.write(s.getBytes());
			}
			text.insert(s, carr);
			carr += s.length();
			text.setCaretPosition(carr);
			autoVisible(true);
		} catch (Exception ee) {
			// TODO: handle exceptions? pass along?
		}
		unit.busy = false;
	}

	// return -1 on CR
	private byte getChar(HW2000 sys) throws Exception {
		int a;
		if (idev != null) {
			a = idev.read();
		} else {
			a = kq.take();
		}
		if (a < 0) {
			return -2;
		}
		a = Character.toUpperCase((char)a);
		if (a == '\n') {
			text.insert("\n", carr++);
			text.setCaretPosition(carr);
			return -1;
		}
		if (col >= 64) {
			text.insert("\n", carr++);
			col = 0;
		}
		int ix = CharConverter.hwAsciiSup.indexOf((char)a);
		if (ix >= 0) {
			a = CharConverter.hwAsciiRep.charAt(ix);
		}
		byte c = sys.pdc.cvt.asciiToHw((byte)a);
		text.insert(sys.pdc.cvt.hwToLP(c), carr++);
		text.setCaretPosition(carr);
		return c;
	}

	// Note: may return special chars (unicode).
	// Called by Control Mode, and Logging Mode (which discards)
	public String input(HW2000 sys) {
		// TODO: lights up "TYPE"?
		String ret = "";
		byte c = -1;
		try {
			while (true) {
				c = getChar(sys);
				if (c < 0) {
					break;
				}
				ret += sys.pdc.cvt.hwToLP(c);
			}
		} catch (Exception ee) { }
		if (c < -1) {
			return "";
		}
		return ret;
	}

	public void output(String s) {
		if (odev != null) {
			try {
				odev.write(s.getBytes());
			} catch (Exception ee) {}
		}
		text.insert(s, carr);
		carr += s.length();
		text.setCaretPosition(carr);
		autoVisible(true);
	}

	public void poke() {
		kq.add(-1);
	}

	public boolean busy(byte c2) {
		if ((c2 & 040) == PeriphDecode.P_IN) {
			return sts[1].busy;
		} else {
			return sts[0].busy;
		}
	}

	public boolean ctl(RWChannel rwc) {
		boolean branch = false;
		int io = ((rwc.c2 & 040) >> 5);
		byte[] cx = new byte[]{ rwc.c3, rwc.c4, rwc.c5, rwc.c6, rwc.c7 };
		for (int x = 0; x < rwc.cn - 2; ++x) {
			if (cx[x] == 010) {
				if (sts[io].busy) {
					branch = true;
				}
			} else if ((cx[x] & 070) == 070) {
				switch(cx[x] & 007) {
				case 000:
					// allow OFF
					break;
				case 001:
					// allow ON
					break;
				case 004:
					dataTerm = false;
					break;
				case 005:
					if (dataTerm) {
						branch = true;
					}
					break;
				case 006:
					interrupt = false;
					break;
				case 007:
					if (interrupt) {
						branch = true;
					}
					break;
				}
			}
		}
		return branch;
	}

	private File pickFile(String purpose) {
		File file = null;
		SuffFileChooser ch = new SuffFileChooser(purpose, _last, 0);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		}
		return file;
	}

	public void keyTyped(KeyEvent e) {
		char c = e.getKeyChar();
		kq.add((int)c);
	}
	public void keyPressed(KeyEvent e) {}
	public void keyReleased(KeyEvent e) { }

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_S) {
			File sav = pickFile("Save");
			if (sav != null) {
				try {
					FileOutputStream fo = new FileOutputStream(sav);
					fo.write(text.getText(0, carr).getBytes());
					fo.close();
					_last = sav;
					// TODO: tear off?
				} catch (Exception ee) {
					HW2000FrontPanel.warning(this, "Save", ee.toString());
				}
			}
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_T) {
			tearOff();
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_F) {
			// TODO: implement this?
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
		isOn = false;
		setVisible(false);
	}
}
