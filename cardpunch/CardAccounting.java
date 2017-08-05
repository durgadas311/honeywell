// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;
import java.util.Properties;

class CardAccounting implements ActionListener, Runnable
{
	class Entry {
		byte mask;
		byte colm;
		public Entry(byte type, byte col) {
			colm = col;
			if (type < 0) {
				mask = (byte)-1;
			} else {
				mask = (byte)(1 << type);
			}
		}
		public boolean enabled(int type) {
			return ((mask & (1 << type)) != 0);
		}
	}

	JFrame _frame;
	Font labels;
	File _cwd;
	File tmp;
	CardHopper hopper;
	CardStacker stacker;
	JButton start;
	JButton stop;
	JButton total;
	// TODO: these are indicators...
	JButton idle;
	JButton stopd;
	JButton fuse;
	JButton form;
	JButton feed;

	JTextArea text;
	JScrollPane scroll;
	CharConverter cvt;

	boolean stopped;
	boolean ibm403;
	JPanel acc;
	JCheckBox acc_cb;
	JTextArea acc_stk;
	JMenu[] _menus;
	GridBagLayout gb;
	GridBagConstraints gc;

	GenericHelp _help;
	SuffFileChooser ch;
	Properties props;
	Entry[] aprint;
	Entry[] nprint;
	int caret;

	public JMenu[] getMenu() { return _menus; }

	public CardAccounting(JFrame frame) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		_frame = frame;
		ibm403 = false;	// TODO: configure

		// TODO: allow inital properties but also load from file.
		aprint = new Entry[43];
		nprint = new Entry[45];
		Arrays.fill(aprint, null);
		Arrays.fill(nprint, null);
		loadProgram("ibm402.1");

		_cwd = new File(System.getProperty("user.dir"));
		cvt = new CharConverter();

		stopped = true;
		hopper = new CardHopper("Input Hopper", 20, 100, 1, false);
		hopper.setListener(this);
		deckUpdate(hopper);
		stacker = new CardStacker("Stacker", 20, 100, 1, true);
		stacker.setListener(this);
		deckUpdate(stacker);
		start = makeButton("START", "start", Color.black, Color.white);
		stop = makeButton("STOP", "stop", Color.black, Color.white);
		total = makeButton("FINAL<BR>TOTAL", "total", Color.black, Color.white);
		// TODO: make indicators...
		idle = makeButton("", null, Color.white, Color.black);
		stopd = makeButton("STOP", null, Color.white, Color.black);
		fuse = makeButton("FUSE", null, Color.white, Color.black);
		form = makeButton("FORM", null, Color.white, Color.black);
		feed = makeButton("CARD<BR>FEED<BR>STOP", null, Color.white, Color.black);

		text = new JTextArea(20, 89);
		text.setEditable(false);
		text.setBackground(Color.white);
		text.setFont(new Font("Monospaced", Font.PLAIN, 10));
		scroll = new JScrollPane(text);
		scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scroll.setViewportBorder(new LineBorder(Color.white, 3));
		caret = 0;

		_menus = new JMenu[3];
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Discard", KeyEvent.VK_D);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Input", KeyEvent.VK_I);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Quit", KeyEvent.VK_Q);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[0] = mu;
		mu = new JMenu("Paper");
		mi = new JMenuItem("Save", KeyEvent.VK_S);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Tear Off", KeyEvent.VK_T);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[1] = mu;
		mu = new JMenu("Help");
		mi = new JMenuItem("About", KeyEvent.VK_A);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Show Help", KeyEvent.VK_H);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[2] = mu;

		java.net.URL url = this.getClass().getResource("docs/Sorter.html");
		_help = new GenericHelp(frame.getTitle() + " Help", url);

		gb = new GridBagLayout();
		frame.setLayout(gb);
		gc = new GridBagConstraints();
		gc.fill = GridBagConstraints.NONE;
		gc.gridx = 0;
		gc.gridy = 0;
		gc.weightx = 0;
		gc.weighty = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gc.insets.bottom = 0;
		gc.insets.top = 0;
		gc.insets.left = 0;
		gc.insets.right = 0;
		gc.anchor = GridBagConstraints.NORTH;

		gc.gridwidth = 20;
		//gc.gridy = 0;
		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridy = 4;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridy = 6;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridwidth = 1;
		gc.gridheight = 5;
		gc.gridy = 1;
		//gc.gridx = 0;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 2;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 7;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 19;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridheight = 1;
		gc.gridx = 1;
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(hopper, gc);
		_frame.add(hopper);
		++gc.gridy;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridy;
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(stacker, gc);
		_frame.add(stacker);

		gc.gridwidth = 4;
		gc.gridheight = 3;
		gc.gridy = 1;
		gc.gridx = 3;
		gb.setConstraints(scroll, gc);
		_frame.add(scroll);

		gc.gridheight = 1;
		gc.gridwidth = 1;
		gc.gridy = 5;
		gc.gridx = 3;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(475, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(start, gc);
		_frame.add(start);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(stop, gc);
		_frame.add(stop);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(total, gc);
		_frame.add(total);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(idle, gc);
		_frame.add(idle);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(stopd, gc);
		_frame.add(stopd);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(fuse, gc);
		_frame.add(fuse);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(form, gc);
		_frame.add(form);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(feed, gc);
		_frame.add(feed);

		// -----------------------------------------
		// Accessory panel for Input Deck chooser...
		// Must be finished with _frame...
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb = new GridBagLayout();
		acc = new JPanel();
		acc.setLayout(gb);
		gc.anchor = GridBagConstraints.WEST;
		JLabel lb = new JLabel("Input Hopper:");
		gb.setConstraints(lb, gc);
		acc.add(lb);
		++gc.gridy;
		acc_cb = new JCheckBox("Remove All");
		gb.setConstraints(acc_cb, gc);
		acc.add(acc_cb);
		++gc.gridy;
		acc_stk = new JTextArea(4, 15);
		acc_stk.setEditable(false);
		gb.setConstraints(acc_stk, gc);
		acc.add(acc_stk);
		++gc.gridy;

		idle.setBackground(Color.red);
	}

	private Entry setEntry(Entry[] xts, int idx, String p) {
		byte c = (byte)-1;
		byte t = (byte)-1;
		int n = 1;
		try {
			if (p.charAt(1) != '.') return null;
			int i = p.indexOf('*');
			if (i > 0) {
				n = Integer.valueOf(p.substring(i + 1));
			} else {
				i = p.length();
			}
			t = Byte.valueOf(p.substring(0, 1));
			c = Byte.valueOf(p.substring(2, i));
			c -= 1;
			while (n > 0) {
				xts[idx++] = new Entry(t, c++);
				--n;
			}
		} catch (Exception ee) {}
		return null;
	}

	private void loadProgram(String prog) {
		Arrays.fill(aprint, null);
		Arrays.fill(nprint, null);
		props = new Properties();
		try {
			InputStream is = new FileInputStream(prog);
			props.load(is);
		} catch (Exception ee) {
			return;
		}
		// TODO: short-hand for fields (ranges)
		// TODO: multiple sources?
		// [an]N = [123].M[*L] :: route first/second/third reading col M
		//	to alpha/num print col N, optionally L columns wide.
		// TODO: aN=3.x requires zN=2.x, produce erroneous output if not wired.
		// TODO: printing requires "all" -> "list"
		for (int x = 0; x < aprint.length; ++x) {
			String p = String.format("a%d", x + 1);
			p = props.getProperty(p);
			if (p == null) continue;
			setEntry(aprint, x, p);
		}
		for (int x = 0; x < nprint.length; ++x) {
			String p = String.format("n%d", x + 1);
			p = props.getProperty(p);
			if (p == null) continue;
			setEntry(nprint, x, p);
		}
	}

	private JButton makeButton(String lab, String act, Color bg, Color fg) {
		JButton btn = new JButton("<HTML><CENTER>" + lab + "</CENTER></HTML>");
		// TODO: indicator, not button
		if (act != null) {
			btn.setActionCommand(act);
			btn.addActionListener(this);
		}
		btn.setFont(labels);
		btn.setPreferredSize(new Dimension(40, 40));
		btn.setMargin(new Insets(1, 1, 1, 1));
		btn.setBackground(bg);
		btn.setForeground(fg);
		btn.setOpaque(true);
		return btn;
	}

	private int getCol(byte[] card, int ix) {
		int p = card[ix * 2] & 0x0ff;
		p |= (card[ix * 2 + 1] & 0x0ff) << 8;
		return p;
	}

	private char printCol(byte[] card, int col) {
		// TODO: alphameric vs. numeric character sets...
		//	alphameric = A-Z,0-9,&
		//	odd numeric = 0-9,*
		//	even numeric = 0-9,CR
		char c = ' ';
		int p = getCol(card, col);
		String t = cvt.punToAscii(p);
		if (t != null) {
			c = t.charAt(0);
		}
		return c;
	}

	private void processRead(int num, byte[] card) {
		if (card == null) {
			return;
		}
		String s = "";
		int n = 0;
		for (int x = 0; x < aprint.length; ++x) {
			if (aprint[x] == null || !aprint[x].enabled(num)) {
				s += ' ';
			} else {
				s += printCol(card, aprint[x].colm);
				++n;
			}
		}
		s += ' ';
		for (int x = 0; x < nprint.length; ++x) {
			if (nprint[x] == null || !nprint[x].enabled(num)) {
				s += ' ';
			} else {
				s += printCol(card, nprint[x].colm);
				++n;
			}
		}
		if (n > 0) {
			text.append(s + '\n');
			caret += s.length() + 1;
			text.setCaretPosition(caret);
		}
		// if programmed stop... {
		//	stopd.setBackground(Color.red);
		//	stopped = true;
		// }
	}

	public void run() {
		idle.setBackground(Color.white);
		feed.setBackground(Color.white); // OFF by what?
		byte[] card1 = null;
		byte[] card2 = null;
		byte[] card3 = null;
		while (!stopped) {
			if (card3 != null) {
				stacker.putCard(card3);
			}
			card3 = card2;
			card2 = card1;
			card1 = new byte[2*80];
			int c = hopper.getCard(card1);
			if (c < 0) {
				//feed.setBackground(Color.red); // feed jam only?
				stopped = true;
				// TODO: what is done for 403?
				if (card3 != null) {
					processRead(2, card2);
					processRead(3, card3);
					stacker.putCard(card3);
				}
				if (card2 != null) {
					processRead(3, card2); // third read!
					stacker.putCard(card2);
				}
				break;
			}
			if (ibm403) {
				processRead(1, card1);
			}
			processRead(2, card2);
			processRead(3, card3);
			try {
				Thread.sleep(10);
			} catch (Exception ee) {}
		}
		idle.setBackground(Color.red);
	}

	private File pickFile(String purpose, boolean input,
				String sfx, String typ, File prev) {
		File file;
		ch = new SuffFileChooser(purpose,
			new String[]{sfx}, new String[]{typ}, prev,
			input ? acc : null);
		int rv = ch.showDialog(_frame);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		} else {
			file = null;
		}
		return file;
	}

	private void showAbout() {
		java.net.URL url = this.getClass().getResource("docs/About2.html");
		try {
			JEditorPane about = new JEditorPane(url);
			about.setEditable(false);
			Dimension dim = new Dimension(300, 260);
			about.setPreferredSize(dim);
			JOptionPane.showMessageDialog(_frame, about,
				"About: Card Punch Simulator", JOptionPane.PLAIN_MESSAGE);
		} catch (Exception ee) { }
	}

	private void showHelp() {
		_help.setVisible(true);
	}

	private void deckAdd() {
		acc_stk.setText(hopper.stackList('\n', false));
		acc_cb.setSelected(false);
		File fi = pickFile("Add Input", true, "pcd", "Punch Card Deck", _cwd);
		if (fi == null) {
			return;
		}
		deckAdd(fi, acc_cb.isSelected());
	}

	private void deckAdd(File fi, boolean empty) {
		if (fi.exists()) {
			try {
				InputStream f = new FileInputStream(fi);
				String n = fi.getName();
				if (n.endsWith(".pcd")) {
					n = n.substring(0, n.length() - 4);
				}
				int c = (int)((fi.length() + 159) / 160);
				hopper.addInput(f, n, c, empty);
			} catch (Exception ee) {
				// TODO: PopupFactory
				ee.printStackTrace();
			}
		} else {
			// TODO: PopupFactory
			System.err.format("Internal error: chosen file does not exist\n");
		}
	}

	private void deckChange(CardHandler obj, String act) {
		if (act.equals("right")) {
			if (obj == hopper) {
				//hopper.addBlank(50);
			} else {
				stacker.discardDeck();
			}
		} else if (act.equals("left")) {
			if (obj == hopper) {
				deckAdd();
			} else {
				stacker.discardDeck();
			}
		}
	}

	private void deckUpdate(CardHandler obj) {
		String tip = obj.getLabel();
		tip += String.format(": %d", obj.stackCount());
		String lst = obj.stackList(',', true);
		if (lst != null) {
			tip += '(';
			tip += lst;
			tip += ')';
		}
		obj.setToolTipText(tip);
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JButton) {
			JButton butt = (JButton)e.getSource();
			String act = butt.getActionCommand();
			if (act.equals("start")) {
				stopped = false;
				Thread t = new Thread(this);
				t.start();
			} else if (act.equals("stop")) {
				stopped = true;
			} else if (act.equals("total")) {
				stopd.setBackground(Color.white);
				// TODO: final total processing...
			}
			return;
		} else if (e.getSource() instanceof CardHandler) {
			// hopper or stacker, mouse or repaint...
			CardHandler ch = (CardHandler)e.getSource();
			String a = e.getActionCommand();
			if (a.equals("repaint")) {
				deckUpdate(ch);
			} else {
				deckChange(ch, a);
			}
			return;
		} else if (e.getSource() instanceof JCheckBox) {
			JCheckBox cb = (JCheckBox)e.getSource();
			return;
		} else if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_D) {
			stacker.discardDeck();
		} else if (m.getMnemonic() == KeyEvent.VK_I) {
			deckAdd();
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper.addBlank(0); // close any input... we hope.
			// stacker.discardDeck(); // file should get removed
			System.exit(0);
		} else if (m.getMnemonic() == KeyEvent.VK_S) {
			File f = pickFile("Save Report", false,
					"txt", "Text Files", _cwd);
			if (f != null) try {
				FileOutputStream fo = new FileOutputStream(f);
				fo.write(text.getText(0, caret).getBytes());
				fo.close();
			} catch (Exception ee) {
				// TODO: pop-up error
			}
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_T) {
			text.setText("");
			caret = 0;
			text.setCaretPosition(caret);
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_A) {
			showAbout();
		} else if (m.getMnemonic() == KeyEvent.VK_H) {
			showHelp();
		}
	}
}
