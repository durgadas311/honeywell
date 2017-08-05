// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.geom.AffineTransform;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;

class CardViewer implements ActionListener
{
	JFrame _frame;

	CardHopper hopper;
	JMenu[] _menus;

	byte[] _card;
	File _cwd;

	JPanel acc;
	JRadioButton ibm026;
	JRadioButton ibm026h;
	JRadioButton ibm029;
	JRadioButton hw200;
	JRadioButton hw200spc;
	ButtonGroup bg;

	JTextArea text;
	JScrollPane scroll;
	CharConverter cvt;
	CharConverter cvt026;
	CharConverter cvt026h;
	Font f222;
	Font f029;
	Font f026;

	public JMenu[] getMenu() { return _menus; }

	public CardViewer(JFrame frame) {
		_frame = frame;

		_cwd = new File(System.getProperty("user.dir"));
		CardPunchOptions opts = new CardPunchOptions();
		cvt = new CharConverter();
		opts.ibm026 = true;
		cvt026 = new CharConverter(opts);
		opts.fortran = true;
		cvt026h = new CharConverter(opts);
		//f222 = new Font("Monospaced", Font.PLAIN, 12);
		// font size must be multiple of artifact geometry
		// for best appearance. For keypunch fonts that is 8.
		f222 = loadFont("HW222.ttf", 16);
		f029 = loadFont("IBM029.ttf", 16);
		f026 = loadFont("IBM026.ttf", 16);

		_card = new byte[2*80];
		hopper = new CardHopper("Input Hopper", 125, 90, 1, false);
		hopper.setListener(this);

		text = new JTextArea(10, 80);
		text.setEditable(false);
		text.setBackground(Color.white);
		text.setFont(f222);
		scroll = new JScrollPane(text);
		scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scroll.setViewportBorder(new LineBorder(Color.white, 3));
		_frame.add(scroll);

		_menus = new JMenu[1];
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Input", KeyEvent.VK_I);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Quit", KeyEvent.VK_Q);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[0] = mu;

		bg = new ButtonGroup();
		ibm026 = new JRadioButton("IBM 026");
		ibm026h = new JRadioButton("IBM 026-H");
		ibm029 = new JRadioButton("IBM 029");
		hw200 = new JRadioButton("HW 200/2000");
		hw200spc = new JRadioButton("HW 200/2000 (special)");
		bg.add(ibm026);
		bg.add(ibm026h);
		bg.add(ibm029);
		bg.add(hw200);
		bg.add(hw200spc);
		hw200.setSelected(true);

		acc = new JPanel();
		acc.setLayout(new BoxLayout(acc, BoxLayout.Y_AXIS));
		acc.add(new JLabel("View As:"));
		acc.add(ibm026);
		acc.add(ibm026h);
		acc.add(ibm029);
		acc.add(hw200);
		acc.add(hw200spc);
	}

	private Font loadFont(String fn, int fz) {
		java.io.InputStream ttf = this.getClass().getResourceAsStream(fn);
		if (ttf != null) try {
			Font font = Font.createFont(Font.TRUETYPE_FONT, ttf);
			return font.deriveFont((float)fz);
		} catch (Exception ee) {}
		return null;
	}

	private File pickFile(String purpose,
				String sfx, String typ, File prev) {
		File file;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{sfx}, new String[]{typ}, prev, acc);
		int rv = ch.showDialog(_frame);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		} else {
			file = null;
		}
		return file;
	}

	private int getCol(int ix) {
		int p = _card[ix * 2] & 0x0ff;
		p |= (_card[ix * 2 + 1] & 0x0ff) << 8;
		return p;
	}

	private void addCard029(byte[] card, CharConverter cvt) {
		String s = "";
		for (int x = 0; x < 80; ++x) {
			int p = getCol(x);
			String t = cvt.punToAscii(p);
			if (t == null) {
				s += ' ';
			} else {
				s += t;
			}
		}
		s += '\n';
		text.append(s);
	}

	private void addCardHW(byte[] card) {
		String s = "";
		for (int x = 0; x < 80; ++x) {
			int p = getCol(x);
			int t = cvt.punToHW(p, hw200spc.isSelected());
			if (t < 0) {
				s += ' ';
			} else {
				s += cvt.hwToLP((byte)t);
			}
		}
		s += '\n';
		text.append(s);
	}

	private void reFont(Font f) {
		text.setFont(f);
		_frame.validate();
		_frame.pack();
		_frame.repaint();
	}

	private void addCard(byte[] card) {
		if (hw200.isSelected() || hw200spc.isSelected()) {
			reFont(f222);
			addCardHW(card);
		} else if (ibm026.isSelected() || ibm026h.isSelected()) {
			reFont(f026);
			addCard029(card, ibm026h.isSelected() ? cvt026h : cvt026);
		} else {
			reFont(f029);
			addCard029(card, cvt);
		}
	}

	private void deckAdd() {
		File fi = pickFile("View Deck", "pcd", "Punch Card Deck", _cwd);
		if (fi == null) {
			return;
		}
		if (fi.exists()) {
			try {
				InputStream f = new FileInputStream(fi);
				String n = fi.getName();
				if (n.endsWith(".pcd")) {
					n = n.substring(0, n.length() - 4);
				}
				int c = (int)((fi.length() + 159) / 160);
				hopper.addInput(f, n, c, true);
			} catch (Exception ee) {
				// TODO: PopupFactory
				ee.printStackTrace();
				return;
			}
		} else {
			// TODO: PopupFactory
			System.err.format("Internal error: chosen file does not exist\n");
			return;
		}
		text.setText("");
		while (hopper.getCard(_card) > 0) {
			addCard(_card);
		}
		hopper.addBlank(0); // closes things
		text.setCaretPosition(0);
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JButton) {
			return;
		} else if (e.getSource() instanceof CardHandler) {
			// hopper or stacker, mouse or repaint...
			CardHandler ch = (CardHandler)e.getSource();
			String a = e.getActionCommand();
			if (a.equals("left")) {
				deckAdd();
			}
			return;
		} else if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_I) {
			deckAdd();
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper.addBlank(0); // close any input... we hope.
			// stacker.discardDeck(); // file should get removed
			System.exit(0);
		}
	}
}
