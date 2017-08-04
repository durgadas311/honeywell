// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;

class CardSorter implements ActionListener, Runnable
{
	static final long serialVersionUID = 311614000000L;

	class ColumnSelector extends JLabel implements MouseListener {
		public int column;

		public ColumnSelector() {
			super("1");
			column = 1;
			addMouseListener(this);
		}

		public void mouseClicked(MouseEvent e) {
			int m = e.getModifiers();
			int col = column;
			if (e.getButton() == MouseEvent.BUTTON1) {
				if (col > 1) --col;
				if ((m & InputEvent.SHIFT_MASK) != 0) {
					col = (col / 10) * 10;
					if (col < 1) col = 1;
				}
			} else if (e.getButton() == MouseEvent.BUTTON2) {
				col = 40;
			} else if (e.getButton() == MouseEvent.BUTTON3) {
				if (col < 80) ++col;
				if ((m & InputEvent.SHIFT_MASK) != 0) {
					col = ((col + 9) / 10) * 10;
				}
			}
			if (col != column) {
				column = col;
				setText(String.format("%d", column));
			}
		}
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		public void mousePressed(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) {}
	}

	JFrame _frame;
	Font labels;
	File _cwd;
	CardHopper hopper;
	CardStacker[] stackers;
	JButton start;
	JButton stop;
	boolean stopped;
	ColumnSelector _col_lb;
	JPanel acc;
	JPanel acc2;
	JCheckBox acc_cb;
	JTextArea acc_stk;
	JCheckBox[] inhibits;
	JCheckBox alphabetic;
	JCheckBox[] pickers;
	JCheckBox rev_pick;
	JMenu[] _menus;
	Rectangle _top, _bottom;
	static final int _inset = 2;
	GridBagLayout gb;
	GridBagConstraints gc;

	byte[] _card;

	GridBagLayout pn_gb;
	GridBagConstraints pn_gc;
	JPanel pn_pn;

	GenericHelp _help;

	public JMenu[] getMenu() { return _menus; }

	// Order of stackers, L-R
	int[] order = new int[]{
		9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 10, 11, 12
	};

	ImageIcon blk;
	ImageIcon red;
	ImageIcon gry;

	public CardSorter(JFrame frame) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		_frame = frame;
		blk = new ImageIcon(getClass().getResource("icons/black-box.png"));
		red = new ImageIcon(getClass().getResource("icons/red-box.png"));
		gry = new ImageIcon(getClass().getResource("icons/gray-box.png"));

		_cwd = new File(System.getProperty("user.dir"));

		_card = new byte[2*80];
		stopped = true;
		hopper = new CardHopper("Input Hopper", 20, 100, 1, false);
		hopper.setListener(this);
		deckUpdate(hopper);
		stackers = new CardStacker[13];
		pickers = new JCheckBox[13];
		inhibits = new JCheckBox[12];
		String t;
		for (int x = 0; x < 13; ++x) {
			if (x < 10) {
				t = String.format("%d", x);
			} else if (x < 12) {
				t = String.format("%d", x + 1);
			} else {
				t = "R";
			}
			stackers[x] = new CardStacker(t, 20, 100, 1, true);
			stackers[x].setListener(this);
			deckUpdate(stackers[x]);
			pickers[x] = new JCheckBox(t);
			if (x < 10) {
				pickers[x].setSelected(true);
			}
			if (x < 12) {
				inhibits[x] = new JCheckBox(t);
				inhibits[x].setOpaque(false);
				inhibits[x].setIcon(gry);
				inhibits[x].setSelectedIcon(blk);
				inhibits[x].setPressedIcon(blk);
			}
		}
		rev_pick = new JCheckBox("Pick L-R");
		alphabetic = new JCheckBox("ALPHA");
		alphabetic.setOpaque(false);
		alphabetic.setIcon(gry);
		alphabetic.setSelectedIcon(red);
		alphabetic.setPressedIcon(red);
		start = new JButton("START");
		start.setActionCommand("start");
		start.addActionListener(this);
		start.setFont(labels);
		start.setPreferredSize(new Dimension(40, 40));
		start.setMargin(new Insets(1, 1, 1, 1));
		start.setBackground(Color.black);
		start.setForeground(Color.white);
		start.setOpaque(true);
		stop = new JButton("STOP");
		stop.setActionCommand("stop");
		stop.addActionListener(this);
		stop.setFont(labels);
		stop.setPreferredSize(new Dimension(40, 40));
		stop.setMargin(new Insets(1, 1, 1, 1));
		stop.setBackground(Color.black);
		stop.setForeground(Color.white);
		stop.setOpaque(true);

		_menus = new JMenu[2];
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Output", KeyEvent.VK_O);
		mi.addActionListener(this);
		mu.add(mi);
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
		mu = new JMenu("Help");
		mi = new JMenuItem("About", KeyEvent.VK_A);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Show Help", KeyEvent.VK_H);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[1] = mu;

		java.net.URL url = this.getClass().getResource("docs/Sorter.html");
		_help = new GenericHelp(frame.getTitle() + " Help", url);

		_col_lb = new ColumnSelector();
		_col_lb.setPreferredSize(new Dimension(20, 20));
		_col_lb.setBackground(new Color(255,255,200));
		_col_lb.setOpaque(true);
		_col_lb.setFocusable(false);
		_col_lb.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));

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

		gc.gridwidth = 30;
		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridy = 2;
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
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 30;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridheight = 1;
		gc.gridx = 27;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(_col_lb, gc);
		_frame.add(_col_lb);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(hopper, gc);
		_frame.add(hopper);

		gc.gridy = 3;
		gc.gridheight = 2;
		gc.gridx = 1;
		for (int x = 0; x < stackers.length; ++x) {
			setPocket(order[x]);
		}
		gc.gridheight = 1;
		pn = new JPanel();
		pn.setLayout(new BoxLayout(pn, BoxLayout.Y_AXIS));
		pn.setOpaque(false);
		pn.add(ibm082ctl());
		pn.add(alphabetic);
		gc.anchor = GridBagConstraints.NORTH;
		gc.gridy = 1;
		gc.gridx = 15;
		gc.gridwidth = 11;
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridwidth = 1;
		gc.gridy = 3;
		gc.gridx = 27;
		gb.setConstraints(start, gc);
		_frame.add(start);
		gc.gridx = 29;
		gb.setConstraints(stop, gc);
		_frame.add(stop);

		// -----------------------------------------
		// Accessory panel for Input Deck chooser...
		// Must be finished with _frame...
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		acc = new JPanel();
		gb = new GridBagLayout();
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
		// Accessory panel for Save Output...
		acc2 = new JPanel();
		acc2.setLayout(new BoxLayout(acc2, BoxLayout.Y_AXIS));
		acc2.add(new JLabel("Save Pockets"));
		acc2.add(pickers[9]);
		acc2.add(pickers[8]);
		acc2.add(pickers[7]);
		acc2.add(pickers[6]);
		acc2.add(pickers[5]);
		acc2.add(pickers[4]);
		acc2.add(pickers[3]);
		acc2.add(pickers[2]);
		acc2.add(pickers[1]);
		acc2.add(pickers[0]);
		acc2.add(pickers[10]);
		acc2.add(pickers[11]);
		acc2.add(pickers[12]);
		acc2.add(rev_pick);
	}

	private JPanel ibm082ctl() {
		JPanel pn = new JPanel();
		pn.setLayout(new GridLayout(3, 4));
		pn.setOpaque(false);
		for (int x = 0; x < 12; ++x) {
			pn.add(inhibits[order[x]]);
		}
		return pn;
	}

	private void setPocket(int x) {
		int gh = gc.gridheight;
		gb.setConstraints(stackers[x], gc);
		_frame.add(stackers[x]);
		gc.gridy += gh;
		gc.gridheight = 1;
		JLabel lb = new JLabel(stackers[x].getLabel());
		gb.setConstraints(lb, gc);
		_frame.add(lb);
		gc.gridx += gc.gridwidth;
		gc.gridy -= gh;
		gc.gridheight = gh + 1;
		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridheight = gh;
		++gc.gridx;
	}

	private JPanel centeredLabel(String lab) {
		// 'lab' is assumed to have "<BR>"(s) in it...
		JPanel pn = new JPanel();
		pn.setOpaque(false);
		GridBagLayout gb2 = new GridBagLayout();
		pn.setLayout(gb2);
		GridBagConstraints gc2 = new GridBagConstraints();
		gc2.fill = GridBagConstraints.NONE;
		gc2.gridx = 0;
		gc2.gridy = 0;
		gc2.weightx = 0;
		gc2.weighty = 0;
		gc2.gridwidth = 1;
		gc2.gridheight = 1;
		gc2.insets.bottom = 0;
		gc2.insets.top = 0;
		gc2.insets.left = 0;
		gc2.insets.right = 0;
		gc2.anchor = GridBagConstraints.NORTH;
		JLabel lb = new JLabel("<HTML><CENTER>" + lab + "</CENTER></HTML>");
		lb.setFont(labels);
		gb2.setConstraints(lb, gc2);
		pn.add(lb);
		return pn;
	}

	private int getCol(int ix) {
		int p = _card[ix * 2] & 0x0ff;
		p |= (_card[ix * 2 + 1] & 0x0ff) << 8;
		return p;
	}

	private int getMask() {
		if (alphabetic.isSelected()) {
			return 0x0e00;
		}
		int m = 0;
		for (int x = 0; x < 12; ++x) {
			if (inhibits[order[x]].isSelected()) {
				m |= (1 << x);
			}
		}
		return m ^ 0x0fff;
	}

	public void run() {
		int col = _col_lb.column;
		int msk = getMask();
		while (!stopped) {
			int c = hopper.getCard(_card);
			if (c < 0) {
				stopped = true;
				break;
			}
			int p = getCol(col) & msk;
			int n = Integer.numberOfTrailingZeros(p);
			if (n < 10) {
				n = 9 - n;
			} else if (n > 12) {
				n = 12;
			}
			stackers[n].putCard(_card);
			try {
				Thread.sleep(10);
			} catch (Exception ee) {}
		}
	}

	private File pickFile(String purpose, boolean input,
				String sfx, String typ, File prev) {
		File file;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{sfx}, new String[]{typ}, prev,
			input ? acc : acc2);
		int rv = ch.showDialog(_frame);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		} else {
			file = null;
		}
		return file;
	}

	private boolean confirmChanges(String op) {
//			int res = Wang_UI.confirm(op, "Changes have not been saved. " +
//							"Discard changes?");
//			if (res == JOptionPane.YES_OPTION) {
//				return true;
//			}
		return true;
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
		if (fi.exists()) {
			try {
				InputStream f = new FileInputStream(fi);
				String n = fi.getName();
				if (n.endsWith(".pcd")) {
					n = n.substring(0, n.length() - 4);
				}
				int c = (int)((fi.length() + 159) / 160);
				hopper.addInput(f, n, c, acc_cb.isSelected());
			} catch (Exception ee) {
				// TODO: PopupFactory
				ee.printStackTrace();
			}
		} else {
			// TODO: PopupFactory
			System.err.format("Internal error: chosen file does not exist\n");
		}
	}

	private void deckSave() {
		File fi = pickFile("Save Output", false, "pcd", "Punch Card Deck", _cwd);
		if (fi == null) {
			return;
		}
		OutputStream os;
		try {
			os = new FileOutputStream(fi);
		} catch (Exception ee) {
			// TODO: pop-up error
			return;
		}
		boolean ok = true;
		if (rev_pick.isSelected()) {
			for (int x = 0; x < stackers.length; ++x) {
				int p = order[x];
				if (!pickers[p].isSelected()) continue;
				if (!stackers[p].saveDeck(os)) {
					ok = false;
				}
			}
		} else {
			for (int x = stackers.length - 1; x >= 0; --x) {
				int p = order[x];
				if (!pickers[p].isSelected()) continue;
				if (!stackers[p].saveDeck(os)) {
					ok = false;
				}
			}
		}
		if (!ok) {
			// TODO: pop-up error
		}
	}

	private void deckChange(CardHandler obj, String act) {
		if (act.equals("right")) {
			if (obj == hopper) {
				//hopper.addBlank(50);
			} else {
				((CardStacker)obj).discardDeck();
				//for (int x = 0; x < stackers.length; ++x) {
				//	stackers[x].discardDeck();
				//}
			}
		} else if (act.equals("left")) {
			if (obj == hopper) {
				deckAdd();
			} else {
				deckSave();
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
		} else if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_O) {
			deckSave();
		} else if (m.getMnemonic() == KeyEvent.VK_D) {
			//stacker.discardDeck();
		} else if (m.getMnemonic() == KeyEvent.VK_I) {
			deckAdd();
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper.addBlank(0); // close any input... we hope.
			// stacker.discardDeck(); // file should get removed
			System.exit(0);
		} else if (m.getMnemonic() == KeyEvent.VK_A) {
			showAbout();
		} else if (m.getMnemonic() == KeyEvent.VK_H) {
			showHelp();
		}
	}
}
