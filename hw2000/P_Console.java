// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.util.Properties;
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
	boolean offline = false;
	boolean interrupt = false;
	boolean dataTerm = false;
	int io; // only valid between io() and run()
	LightedButton log;
	LightedButton type;

	public P_Console(Properties props) {
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

		String s = props.getProperty("console");
		if (s != null && s.equals("220-3")) {
			// TODO: create TYPE button here?
			// but needs to plug-in on front panel...
		} else {
			JPanel pn = new JPanel();
			pn.setPreferredSize(new Dimension(10, 30));
			mb.add(pn);
			ImageIcon icn;
			icn = new ImageIcon(getClass().getResource("icons/fp_type2.png"));
			LightedButton btn = new LightedButton(Peripheral.btnWhiteOn,
					Peripheral.btnWhiteOff, icn);
			mb.add(btn);
			type = btn;
			pn = new JPanel();
			pn.setPreferredSize(new Dimension(10, 30));
			mb.add(pn);
			icn = new ImageIcon(getClass().getResource("icons/fp_log.png"));
			btn = new LightedButton(Peripheral.btnWhiteOn,
					Peripheral.btnWhiteOff, icn);
			mb.add(btn);
			log = btn;
			log.addActionListener(this);
			pn = new JPanel();
			pn.setPreferredSize(new Dimension(360, 30));
			mb.add(pn);
		}
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

	public void setOffline(boolean off) {
		// TODO: reject if busy?
		offline = off;
		if (log != null) {
			log.setOn(off);
		}
	}

	public LightedButton getLogBtn() {
		return log;
	}

	public LightedButton getTypeBtn() {
		return type;
	}

	public void setTypeBtn(LightedButton btn) {
		type = btn;
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
		// TODO: wait for !offline?
		if (io == 0) {
			doOut(rwc, sts[io]);
		} else {
			doIn(rwc, sts[io]);
		}
	}

	private void doIn(RWChannel rwc, ConsoleStatus unit) {
		if (type != null) {
			type.setOn(true);
		}
		rwc.startCLC();
		int a = -1;
		byte b;
		try {
			// TODO: what effect does c3 have? Print CR/LF before input?
			// Or... use CR as termination of input?
			byte c = 0;
			while (true) {
				c = getChar(rwc.sys, true);
				if (c < 0) {
					// caller must check CLC (CLC - SLC)
					break;
				}
				// Must not disturb punctuation
				c = rwc.writeChar(c);
				if ((c & 0300) == 0300) {
					break;
				}
				if (rwc.incrCLC()) {
					break;
				}
			}
			// user did not press carriage-return and
			// carriage-return is NOT suppressed...
			if (c >= 0 && rwc.c3 != 0) {
				putChar("\n");
			}
		} catch (Exception ee) {
			// TODO: pass along EI/II exceptions
		}
		unit.busy = false;
		if (type != null) {
			type.setOn(false);
		}
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
				s += rwc.sys.pdc.cvt.hwToLP(a);
				if (rwc.incrCLC()) {
					break;
				}
			}
			if (rwc.c3 != 0) {
				s += "\n";
				col = 0;
			}
			putChar(s);
			if (odev != null) {
				odev.write(s.getBytes());
			}
			autoVisible(true);
		} catch (Exception ee) {
			// TODO: handle exceptions? pass along?
		}
		unit.busy = false;
	}

	// Can only have '\n' at end...
	private void putChar(String s) {
		int n = s.length();
		if (col + n > 64) {
			putChar(s.substring(0, 64 - col));
			s = s.substring(64 - col);
			n = s.length();
		}
		text.insert(s, carr);
		carr += n;
		col += n;
		if (s.endsWith("\n")) {
			col = 0;
		} else if (col >= 64) {
			// should only ever be "col == 64"...
			text.insert("\n", carr++);
			col = 0;
		}
		text.setCaretPosition(carr);
	}

	// return -1 on CR
	private byte getChar(HW2000 sys, boolean echo) throws Exception {
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
			// TODO: is <CR> always echoed?
			col = 0;
			if (echo) {
				putChar("\n");
			}
			return -1;
		}
		int ix = CharConverter.hwAsciiSup.indexOf((char)a);
		if (ix >= 0) {
			a = CharConverter.hwAsciiRep.charAt(ix);
		}
		byte c = sys.pdc.cvt.asciiToHw((byte)a);
		if (echo) {
			putChar(sys.pdc.cvt.hwToLP(c));
		}
		return c;
	}

	// Note: may return special chars (unicode).
	// Called by Control Mode, and Logging Mode (which discards)
	// Does not echo!
	public int inChar(HW2000 sys) {
		byte c = -2;
		try {
			c = getChar(sys, false);
		} catch (Exception ee) {}
		if (c < -1) {
			return -1;
		}
		if (c < 0) {
			return '\n';
		}
		return sys.pdc.cvt.hwToLP(c).charAt(0);
	}

	public String input(HW2000 sys) {
		// TODO: lights up "TYPE"?
		String ret = "";
		byte c = -1;
		try {
			while (true) {
				c = getChar(sys, true);
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
		putChar(s);
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
		SuffFileChooser ch = new SuffFileChooser(purpose, null, null, _last, null);
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
		if (e.getSource() instanceof LightedButton) {
			LightedButton b = (LightedButton)e.getSource();
			if (b != log) {
				return;
			}
			setOffline(!offline);
			return;
		}
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
					PopupFactory.warning(this, "Save", ee.toString());
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
