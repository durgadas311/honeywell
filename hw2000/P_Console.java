import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

public class P_Console extends JFrame
		implements Peripheral, KeyListener, ActionListener, WindowListener {

	InputStream idev;
	OutputStream odev;
	byte c3;
	byte c2;
	int clc, slc;
	boolean busy;
	JTextArea text;
	JScrollPane scroll;
	java.util.concurrent.LinkedBlockingDeque<Integer> kq;
	int carr;

	public P_Console() {
		super("H220 Console");
		kq = new java.util.concurrent.LinkedBlockingDeque<Integer>();
		odev = null;
		idev = null;
		busy = false;
		setLayout(new FlowLayout());
		text = new JTextArea(24, 80);
		text.setEditable(false); // this prevents caret... grrr.
		text.setBackground(Color.white);
		text.setFont(new Font("Monospaced", Font.PLAIN, 10));
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
		setJMenuBar(mb);

		addWindowListener(this);
		text.addKeyListener(this);
		pack();
		tearOff();
	}

	private void tearOff() {
		text.setText("\u2588");
		carr = 0;
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
		if (busy && (c2 & 040) != 0) {
			kq.add(-1);
		}
	}

	public void io(HW2000 sys) {
		clc = (byte)(sys.getXtra(0) & 027);
		slc = clc + 010;
		if ((sys.getXtra(1) & 030) == 010) {
			c2 = sys.getXtra(2);
			c3 = sys.getXtra(3);
		} else {
			c2 = sys.getXtra(1);
			c3 = sys.getXtra(2);
		}
		// C3:
		//	000000: no CR/LF
		//	000001: CR (LF?)
		sys.cr[slc] = sys.validAdr(sys.AAR);	// translate to physical address
		busy = true;
		// TODO: allow simultaneous input/output?
	}

	public void run(HW2000 sys) {
		if (!busy) {
			return;
		}
		if ((c2 & 040) == 0) {
			doOut(sys);
		} else {
			doIn(sys);
		}
	}

	private void doIn(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
		int a = -1;
		byte b;
		try {
			b = (byte)(sys.rawReadMem(sys.cr[clc]) & 0300); // ignored for termination
			do {
				if (idev != null) {
					a = idev.read();
				} else {
					a = kq.take();
				}
				if (a < 0) {
					break;
				}
				a = Character.toUpperCase((char)a);
				// TODO: what effect does c3 have?
				if (a == '\n') {
					// TODO: mark end of line? caller must cleanup...
					text.insert("\n", carr++);
					text.setCaretPosition(carr);
					sys.rawWriteMem(sys.cr[clc], (byte)0300);
					break;
				}
				int ix = CharConverter.hwAsciiSup.indexOf((char)a);
				if (ix >= 0) {
					a = CharConverter.hwAsciiRep.charAt(ix);
				}
				byte c = sys.pdc.cvt.asciiToHw((byte)a);
				text.insert(sys.pdc.cvt.hwToLP(c), carr++);
				text.setCaretPosition(carr);
				// Must not disturb punctuation?
				sys.rawWriteMem(sys.cr[clc], c);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
				b = (byte)(sys.rawReadMem(sys.cr[clc]) & 0300);
			} while (b != 0300); // always end at record mark, right?
		} catch (Exception ee) {
			// TODO: pass along EI/II exceptions
		}
		busy = false;
	}

	public void doOut(HW2000 sys) {
		sys.cr[clc] = sys.cr[slc];
		String s = "";
		boolean print = true;
		// Printing stops *before* char with record mark...
		try {
			while (print) {
				byte a = sys.rawReadMem(sys.cr[clc]);
				if ((a & 0300)  == 0300) {
					break;
				}
				a &= 077;
				s += sys.pdc.cvt.hwToLP(a);
				sys.cr[clc] = (sys.cr[clc] + 1) & 01777777;
				if (sys.cr[clc] == 0) { // sanity check. must stop sometime.
					break;
				}
			}
			if (c3 != 0) {
				s += "\n";
			}
			if (odev != null) {
				odev.write(s.getBytes());
			}
			text.insert(s, carr);
			carr += s.length();
			text.setCaretPosition(carr);
			setVisible(true);
		} catch (Exception ee) {
			// TODO: handle exceptions? pass along?
		}
		busy = false;
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
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_T) {
			tearOff();
			setVisible(true);
			return;
		}
		if (m.getMnemonic() == KeyEvent.VK_F) {
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
		setVisible(false);
	}
}
