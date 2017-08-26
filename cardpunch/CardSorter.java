// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;

class CardSorter implements Machine, ActionListener, Runnable
{
	static final long serialVersionUID = 311614000000L;

	class ColumnSelector extends JPanel implements MouseListener {
		public int column;
		private JLabel cntr;

		public ColumnSelector() {
			super();
			setPreferredSize(new Dimension(40, 40));
			setOpaque(false);
			setBackground(new Color(150,150,150));
			cntr = new JLabel(" 1");
			cntr.setPreferredSize(new Dimension(20, 20));
			cntr.setBackground(new Color(200,200,200));
			cntr.setOpaque(true);
			cntr.setFocusable(false);
			cntr.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
			add(cntr);
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
				cntr.setText(String.format("%2d", column));
			}
		}
		public void mouseEntered(MouseEvent e) {
			setOpaque(true);
			repaint();
		}
		public void mouseExited(MouseEvent e) {
			setOpaque(false);
			repaint();
		}
		public void mousePressed(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) {}
	}

	JFrame _frame;
	Font labels;
	File _cwd;
	File tmp;
	CardHopper hopper;
	CardStacker[] stackers;
	JButton start;
	JButton stop;
	boolean stopped;
	ColumnSelector _col_lb;
	JPanel acc2;
	JCheckBox[] inhibits;
	JCheckBox alphabetic;
	JCheckBox[] pickers;
	JCheckBox rev_pick;
	JCheckBox recycle;
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
	SuffFileChooser ch;

	public JMenu[] getMenu() { return _menus; }
	public JFrame getFrame() { return _frame; }
	public void setQuitListener(ActionListener lstn) { quit = lstn; }
	private ActionListener quit = null;
	String title;

	// Order of stackers, L-R
	int[] order = new int[]{
		9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 10, 11, 12
	};

	ImageIcon blk;
	ImageIcon red;
	ImageIcon gry;

	AppManager manager;
	CardViewer viewer;

	public CardSorter(JFrame frame, AppManager mgr) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		manager = mgr;
		viewer = null;
		_frame = frame;
		title = _frame.getTitle();
		blk = new ImageIcon(getClass().getResource("icons/black-box.png"));
		red = new ImageIcon(getClass().getResource("icons/red-box.png"));
		gry = new ImageIcon(getClass().getResource("icons/gray-box.png"));

		_cwd = new File(System.getProperty("user.dir"));
		try {
			tmp = File.createTempFile("PCD-", ".pcd");
			tmp.deleteOnExit();
		} catch (Exception ee) {}

		_card = new byte[2*80];
		stopped = true;
		hopper = new CardHopper("Input Hopper", 20, 100, 1, false);
		hopper.setListener(this);
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
			pickers[x] = new JCheckBox(t);
			if (x < 12) {
				inhibits[x] = new JCheckBox(t);
				inhibits[x].setOpaque(false);
				inhibits[x].setIcon(gry);
				inhibits[x].setSelectedIcon(blk);
				inhibits[x].setPressedIcon(blk);
				inhibits[x].setFocusable(false);
			}
		}
		rev_pick = new JCheckBox("Pick L-R");
		recycle = new JCheckBox("Recycle");
		recycle.addActionListener(this);
		alphabetic = new JCheckBox("ALPHA");
		alphabetic.setOpaque(false);
		alphabetic.setIcon(gry);
		alphabetic.setSelectedIcon(red);
		alphabetic.setPressedIcon(red);
		alphabetic.addActionListener(this);
		alphabetic.setFocusable(false);
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

		// Accessory panel for Save Output...
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gb = new GridBagLayout();
		acc2 = new JPanel();
		acc2.setLayout(gb);
		JLabel lb = new JLabel("Save Pockets");
		gc.gridwidth = 2;
		gb.setConstraints(lb, gc);
		acc2.add(lb);
		gc.gridwidth = 1;
		addPicker(pickers[9], 0, 1);
		addPicker(pickers[8], 0, 2);
		addPicker(pickers[7], 0, 3);
		addPicker(pickers[6], 0, 4);
		addPicker(pickers[5], 0, 5);
		addPicker(pickers[4], 0, 6);
		addPicker(pickers[3], 1, 1);
		addPicker(pickers[2], 1, 2);
		addPicker(pickers[1], 1, 3);
		addPicker(pickers[0], 1, 4);
		addPicker(pickers[10], 1, 5);
		addPicker(pickers[11], 1, 6);
		addPicker(pickers[12], 1, 7);
		gc.gridwidth = 2;
		gc.gridx = 0;
		gc.gridy = 8;
		gb.setConstraints(rev_pick, gc);
		acc2.add(rev_pick);
		++gc.gridy;
		gb.setConstraints(recycle, gc);
		acc2.add(recycle);

		setAlphabetic(false);
	}

	private void addPicker(JCheckBox pk, int x, int y) {
		gc.gridx = x;
		gc.gridy = y;
		gb.setConstraints(pk, gc);
		acc2.add(pk);
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

	private void setAlphabetic(boolean alpha) {
		pickers[0].setSelected(true);
		for (int x = 1; x < 13; ++x) {
			if (x < 10) {
				pickers[x].setSelected(!alpha);
			} else {
				pickers[x].setSelected(alpha);
			}
		}
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
		_frame.setTitle(title + " (running)");
		int col = _col_lb.column;
		int msk = getMask();
		while (!stopped) {
			int c = hopper.getCard(_card);
			if (c < 0) {
				stopped = true;
				break;
			}
			int p = getCol(col - 1) & msk;
			int n = Integer.numberOfTrailingZeros(p);
			if (n < 10) {
				n = 9 - n;
			} else if (n > 12) {
				n = 12;
			}
			stackers[n].putCard(_card);
			try {
				Thread.sleep(50);
			} catch (Exception ee) {}
		}
		_frame.setTitle(title);
	}

	private File pickFile(String purpose, JComponent acc,
				String sfx, String typ, File prev) {
		File file;
		ch = new SuffFileChooser(purpose,
			new String[]{sfx}, new String[]{typ}, prev, acc);
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

	private void deckAdd(CardHopper hop) {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		File fi = hop.addDialog("Add Input", dir);
		if (fi != null && manager != null) {
			manager.setCardDir(fi);
		}
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

	private void deckSave() {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		recycle.setSelected(false);
		File fi = pickFile("Save Output", acc2, "pcd", "Punch Card Deck", dir);
		if (recycle.isSelected()) {
			fi = tmp;
		}
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
		try {
			os.close();
		} catch (Exception ee) {}
		if (!ok) {
			// TODO: pop-up error
			return;
		}
		if (recycle.isSelected()) {
			deckAdd(fi, true);
		}
		if (manager != null) {
			manager.setCardDir(fi);
		}
	}

	private void deckView(CardStacker stk) {
		if (manager == null) {
			return;
		}
		if (viewer == null) {
			viewer = manager.getViewer();
		}
		viewer.viewDeck(stk.getDeck(), false, false);
	}

	private void deckChange(CardHandlerEvent ae, CardHandler obj, String act) {
		if (act.equals("right")) {
			if (obj instanceof CardHopper) {
				ae.consume(); // BLANK not allowed?
				//hopper.addBlank(50);
			}
		} else if (act.equals("left")) {
			ae.consume();
			if (obj instanceof CardHopper) {
				deckAdd((CardHopper)obj);
			} else {
				deckSave();
			}
		} else if (act.equals("LEFT")) {
			if (obj instanceof CardStacker) {
				ae.consume();
				deckView((CardStacker)obj);
			}
		}
	}

	private void deckUpdate(CardHandlerEvent ae, CardHandler obj) {
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
		} else if (e instanceof CardHandlerEvent) {
			// hopper or stacker, mouse or repaint...
			CardHandler ch = (CardHandler)e.getSource();
			String a = e.getActionCommand();
			CardHandlerEvent ae = (CardHandlerEvent)e;
			if (a.equals("repaint")) {
				deckUpdate(ae, ch);
			} else {
				deckChange(ae, ch, a);
			}
			return;
		} else if (e.getSource() instanceof JCheckBox) {
			JCheckBox cb = (JCheckBox)e.getSource();
			if (cb == recycle) {
				ch.setSelectedFile(recycle.isSelected() ?
					new File("dont-care") :
					new File(""));
			} else if (cb == alphabetic) {
				setAlphabetic(alphabetic.isSelected());
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
			deckAdd(hopper);
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper.addBlank(0); // close any input... we hope.
			// stacker.discardDeck(); // file should get removed
			if (quit != null) {
				quit.actionPerformed(new ActionEvent(this, e.getID(), "quit"));
			} else {
				System.exit(0);
			}
		} else if (m.getMnemonic() == KeyEvent.VK_A) {
			showAbout();
		} else if (m.getMnemonic() == KeyEvent.VK_H) {
			showHelp();
		}
	}
}
