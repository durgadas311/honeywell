// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.geom.AffineTransform;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;

import java.awt.print.*;
import javax.print.attribute.*;
import javax.print.attribute.standard.*;

class PunchCardDeck extends PunchCard
		implements Machine, KeyListener, ActionListener, WindowListener, Runnable
{
	static final long serialVersionUID = 311614000000L;

	JFrame _frame;
	java.util.concurrent.LinkedBlockingDeque<Integer> _keyQue;
	boolean _codeCard;

	Font labels;
	File _progFile;
	File _cwd;
	CardHopper hopper;
	CardStacker stacker;
	JPanel reading;
	JPanel snap_acc;
	JCheckBox snap_cb;
	int _pgix = 0;
	JMenu[] _menus;
	Rectangle _top, _bottom;
	byte[] bb;
	static final int _inset = 2;

	byte[] _code;
	byte[] _prog;
	byte[] _prev;
	boolean changed;
	boolean _currIsProg;
	boolean _saveImage;
	boolean _ibm026;
	boolean _fortran;
	boolean _endOfCard;

	GridBagLayout pn_gb;
	GridBagConstraints pn_gc;
	JPanel pn_pn;
	JCheckBox _interp_cb;
	JCheckBox _autoSD_cb;
	JCheckBox _progSel_cb;
	JCheckBox _autoFeed_cb;
	JCheckBox _print_cb;
	JCheckBox _lzprint_cb;
	JButton _clear_bn;

	GenericHelp _help;

	JCheckBox _prog_cb;
	JLabel _col_lb;

	// Program card punches - prog2 are shifted to match
	static final int FIELD = 0x0800;
	static final int SKIP  = 0x0400;
	static final int DUP   = 0x0200;
	static final int ALPHA = 0x0100; // not needed?

	ImageIcon icn_on;
	ImageIcon icn_off;
	ImageIcon icn_pr;
	ImageIcon pgm_on;
	ImageIcon pgm_off;

	public JMenu[] getMenu() { return _menus; }
	public JFrame getFrame() { return _frame; }
	public void setQuitListener(ActionListener lstn) { quit = lstn; }
	private ActionListener quit = null;

	public Color getBg() { return hole; }

	private int getProg(byte[] card, int x) {
		int c = 0;
		if (_prog_cb.isSelected()) {
			c = getCode(card, x);
			if (!_progSel_cb.isSelected()) {
				c <<= 6;
			}
		}
		return c;
	}

	AppManager manager;
	CardViewer viewer;

	public PunchCardDeck(JFrame frame, AppManager mgr, CardPunchOptions opts) {
		super(opts);
		manager = mgr;
		viewer = null;
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		pgm_on = new ImageIcon(getClass().getResource("icons/ibm029-pgm-30-on.png"));
		pgm_off = new ImageIcon(getClass().getResource("icons/ibm029-pgm-30-off.png"));
		_ibm026 = opts.ibm026;
		_fortran = opts.fortran;
		if (_ibm026) {
			icn_on = new ImageIcon(getClass().getResource("icons/ibm026-30-on.png"));
			icn_off = new ImageIcon(getClass().getResource("icons/ibm026-30-off.png"));
			icn_pr = new ImageIcon(getClass().getResource("icons/ibm026-30-pr.png"));
		} else { // IBM029
			icn_on = new ImageIcon(getClass().getResource("icons/ibm029-30-on.png"));
			icn_off = new ImageIcon(getClass().getResource("icons/ibm029-30-off.png"));
			icn_pr = new ImageIcon(getClass().getResource("icons/ibm029-30-pr.png"));
		}
		_saveImage = opts.images;
		_frame = frame;
		_frame.setFocusTraversalKeysEnabled(false);
		bb = new byte[1];

		_cwd = new File(System.getProperty("user.dir"));
		_top = new Rectangle(0, 0, 10, 10);
		_bottom = new Rectangle(0, _image.getIconHeight() - 10, 10, 10);

		_code = null;
		_curr = _code;
		changed = false;
		_currIsProg = false;
		_endOfCard = false;
		_prev = null;
		_prog = new byte[2*80];
		// TODO: initialize program card from file...
		Arrays.fill(_prog, (byte)0);
		_progFile = null;
		hopper = new CardHopper("Input Hopper", 125, 90, 1, false);
		stacker = new CardStacker("Output Stacker", 125, 90, 1, false);
		hopper.addBlank(50);
		hopper.setListener(this);
		stacker.setListener(this);
		reading = new JPanel();
		reading.setPreferredSize(new Dimension(45 + 4, 20 + 4));
		reading.setBackground(Color.gray);
		reading.setOpaque(true);
		reading.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));

		_menus = new JMenu[3];
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Output", KeyEvent.VK_O);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Discard", KeyEvent.VK_D);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Snap Image", KeyEvent.VK_P);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Input", KeyEvent.VK_I);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Blank", KeyEvent.VK_B);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Quit", KeyEvent.VK_Q);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[0] = mu;
		mu = new JMenu("Prog");
		mi = new JMenuItem("Load", KeyEvent.VK_L);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Save", KeyEvent.VK_S);
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

		java.net.URL url = this.getClass().getResource("docs/CardPunch.html");
		_help = new GenericHelp(frame.getTitle() + " Help", url);

		_col_lb = new JLabel();
		_col_lb.setPreferredSize(new Dimension(20, 20));
		_col_lb.setBackground(Color.white);
		_col_lb.setOpaque(true);
		_col_lb.setFocusable(false);
		_col_lb.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));

		_interp_cb = new JCheckBox("Interpret (Punch)");
		_interp_cb.setFocusable(false);
		_autoSD_cb = new JCheckBox("Auto SKIP/DUP");
		_autoSD_cb.setFocusable(false);
		_progSel_cb = new JCheckBox("Prog 2 (1)");
		_progSel_cb.setFocusable(false);
		_progSel_cb.setSelected(true);
		_autoFeed_cb = new JCheckBox("Auto Feed");
		_autoFeed_cb.setFocusable(false);
		_print_cb = new JCheckBox("Print");
		_print_cb.setFocusable(false);
		_print_cb.setSelected(true);
		_lzprint_cb = new JCheckBox("LZ Print");
		_lzprint_cb.setFocusable(false);
		_prog_cb = new JCheckBox("Prog");
		_prog_cb.setFocusable(false);
		_prog_cb.setIcon(pgm_off);
		_prog_cb.setSelectedIcon(pgm_on);
		_prog_cb.setHorizontalTextPosition(SwingConstants.LEFT);
		_clear_bn = new JButton("Clear");
		_clear_bn.addActionListener(this);
		_clear_bn.setFocusable(false);

		pn_gc = new GridBagConstraints();
		pn_gc.fill = GridBagConstraints.NONE;
		pn_gc.gridx = 0;
		pn_gc.gridy = 0;
		pn_gc.weightx = 0;
		pn_gc.weighty = 0;
		pn_gc.gridwidth = 1;
		pn_gc.gridheight = 1;
		pn_gc.insets.bottom = 0;
		pn_gc.insets.top = 0;
		pn_gc.insets.left = 0;
		pn_gc.insets.right = 0;
		pn_gc.anchor = GridBagConstraints.CENTER;
		pn_gb = new GridBagLayout();
		JPanel hp = pn_pn = new JPanel();
		pn_pn.setLayout(pn_gb);
		pn_pn.setPreferredSize(new Dimension(_image.getIconWidth() + 2 * _inset, 108));
		hopperPanel(opts);
		pn_gb = new GridBagLayout();
		pn_pn = new JPanel();
		pn_pn.setLayout(pn_gb);
		pn_pn.setPreferredSize(new Dimension(_image.getIconWidth() + 2 * _inset, 100));
		if (_ibm026) {
			ibm026Panel(opts);
		} else {
			ibm029Panel(opts);
		}
		GridBagLayout gridbag = new GridBagLayout();
		frame.setLayout(gridbag);
		GridBagConstraints gc = new GridBagConstraints();
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

		gridbag.setConstraints(hp, gc);
		frame.add(hp);
		++gc.gridy;
		pn_pn.setFocusable(false);
		gridbag.setConstraints(pn_pn, gc);
		frame.add(pn_pn);
		++gc.gridy;
		gc.insets.bottom = _inset;
		gc.insets.top = _inset;
		gridbag.setConstraints(this, gc);
		frame.add(this);

		frame.addKeyListener(this);

		_keyQue = new java.util.concurrent.LinkedBlockingDeque<Integer>();

		// Accessory panel for card snapshot dialog
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		snap_acc = new JPanel();
		GridBagLayout gb = new GridBagLayout();
		snap_acc.setLayout(gb);
		gc.anchor = GridBagConstraints.WEST;
		snap_cb = new JCheckBox("Transparency");
		gb.setConstraints(snap_cb, gc);
		snap_acc.add(snap_cb);
		++gc.gridy;
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

	private void iconToggle(AbstractButton sw) {
		sw.setOpaque(false);
		sw.setText("");
		sw.setIcon(icn_off);
		if (sw instanceof JButton) {
			sw.setPressedIcon(icn_on);
			sw.setBorderPainted(false);
			sw.setContentAreaFilled(false);
			sw.setFocusPainted(false);
		} else {
			// must be JCheckBox...
			sw.setSelectedIcon(icn_on);
			sw.setPressedIcon(icn_pr);
		}
	}

	private void labeledToggle(AbstractButton sw, String top, String bot,
					boolean gap) {
		int saveY = pn_gc.gridy;
		JPanel spc;
		pn_gc.gridheight = 1;
		iconToggle(sw);
		// For now, 'top' is never multi-line
		JLabel lb = new JLabel(top);
		lb.setFont(labels);
		pn_gc.anchor = GridBagConstraints.SOUTH;
		pn_gb.setConstraints(lb, pn_gc);
		pn_pn.add(lb);
		++pn_gc.gridy;
		pn_gc.anchor = GridBagConstraints.CENTER;
		pn_gb.setConstraints(sw, pn_gc);
		pn_pn.add(sw);
		++pn_gc.gridy;
		pn_gc.anchor = GridBagConstraints.NORTH;
		if (bot.matches(".*<BR>.*")) {
			spc = centeredLabel(bot);
			pn_gb.setConstraints(spc, pn_gc);
			pn_pn.add(spc);
		} else {
			lb = new JLabel(bot);
			lb.setFont(labels);
			pn_gb.setConstraints(lb, pn_gc);
			pn_pn.add(lb);
		}
		++pn_gc.gridx;
		// cleanup
		pn_gc.anchor = GridBagConstraints.CENTER;
		pn_gc.gridheight = 3;
		pn_gc.gridy = saveY;
		//
		if (gap) {
			spc = new JPanel();
			spc.setPreferredSize(new Dimension(15, 20));
			pn_gb.setConstraints(spc, pn_gc);
			pn_pn.add(spc);
			++pn_gc.gridx;
		}
	}

	class Ibm026SwitchPlate extends JPanel {
		int _sx, _sy, _arcw;

		public Ibm026SwitchPlate(int w, int h, int r) {
			super();
			setOpaque(false);
			setPreferredSize(new Dimension(w, h));
			_sx = w;
			_sy = h;
			_arcw = r;
		}
		public void paint(Graphics g) {
			Graphics2D g2d = (Graphics2D)g;
			g2d.setColor(Color.white);
			g2d.fillRoundRect(0, 0, _sx, _sy, _arcw, _arcw);
			super.paint(g);
		}
	}

	private JPanel ibm026Switches(CardPunchOptions opts) {
		JPanel pan = new Ibm026SwitchPlate(250, 90, 48);
		GridBagLayout gb = new GridBagLayout();
		pan.setLayout(gb);
		GridBagConstraints gc = new GridBagConstraints();
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
		gc.anchor = GridBagConstraints.CENTER;
		JPanel spc = new JPanel();
		spc.setPreferredSize(new Dimension(20, 20));
		spc.setOpaque(false);
		gc.gridheight = 3;
		gb.setConstraints(spc, gc);
		pan.add(spc);
		++gc.gridx;
		gc.gridheight = 1;
		int savex = gc.gridx;
		//
		gc.gridwidth = 5;
		JLabel lb = new JLabel("");
		lb.setFont(labels);
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(lb, gc);
		pan.add(lb);
		gc.gridwidth = 1;
		//
		gc.gridx = savex;
		++gc.gridy;
		gc.anchor = GridBagConstraints.CENTER;
		iconToggle(_autoFeed_cb);
		gb.setConstraints(_autoFeed_cb, gc);
		pan.add(_autoFeed_cb);
		++gc.gridx;
		JPanel pn = centeredLabel("&nbsp;&nbsp;ON&nbsp;&nbsp;<BR>OFF");
		gb.setConstraints(pn, gc);
		pan.add(pn);
		++gc.gridx;
		iconToggle(_autoSD_cb);
		gb.setConstraints(_autoSD_cb, gc);
		pan.add(_autoSD_cb);
		++gc.gridx;
		//pn = centeredLabel("ON<BR>OFF");
		pn = centeredLabel("&nbsp;&nbsp;ON&nbsp;&nbsp;<BR>OFF");
		gb.setConstraints(pn, gc);
		pan.add(pn);
		++gc.gridx;
		iconToggle(_print_cb);
		gb.setConstraints(_print_cb, gc);
		pan.add(_print_cb);
		++gc.gridx;

		gc.gridx = savex;
		++gc.gridy;
		gc.anchor = GridBagConstraints.NORTH;
		pn = centeredLabel("AUTO<BR>FEED");
		gb.setConstraints(pn, gc);
		pan.add(pn);
		++gc.gridx;
		gc.gridwidth = 3;
		pn = centeredLabel("AUTO SKIP<BR>AUTO DUP");
		gb.setConstraints(pn, gc);
		pan.add(pn);
		gc.gridx += 3;
		gc.gridwidth = 1;
		lb = new JLabel("PRINT");
		lb.setFont(labels);
		gb.setConstraints(lb, gc);
		pan.add(lb);
		++gc.gridx;

		gc.gridy = 0;
		gc.gridheight = 3;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(50, 20));
		spc.setOpaque(false);
		gb.setConstraints(spc, gc);
		pan.add(spc);

		return pan;
	}

	private void ibm026Panel(CardPunchOptions opts) {
		pn_gc.gridx = 0;
		pn_gc.gridy = 0;
		pn_gc.gridheight = 1;
		pn_gc.gridwidth = 3;
		JPanel spc = new JPanel();
		spc.setPreferredSize(new Dimension(200, 5));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		pn_gc.gridy = 2;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(200, 5));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		pn_gc.gridy = 1;
		pn_gc.gridwidth = 1;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(200, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		spc = ibm026Switches(opts);
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(220, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
	}

	private void hopperPanel(CardPunchOptions opts) {
		// First, the hoppers and program drum...
		pn_gc.anchor = GridBagConstraints.SOUTH;
		pn_gc.gridx = 0;
		pn_gc.gridy = 0;
		JPanel spc = new JPanel();
		spc.setPreferredSize(new Dimension(5, 20));
		pn_gc.gridheight = 3;
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		spc = stacker;
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(5, 100));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		//
		spc = new JPanel();
		spc.setLayout(new BoxLayout(spc, BoxLayout.Y_AXIS));
		JLabel lb = new JLabel("<HTML>Reading<BR>Station</HTML>");
		lb.setFont(labels);
		spc.add(lb);
		spc.add(reading);
		pn_gc.anchor = GridBagConstraints.SOUTH;
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		//
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(30, 100));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		pn_gc.gridheight = 1;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(5, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		pn_gc.gridy = 1;
		pn_gb.setConstraints(_col_lb, pn_gc);
		pn_pn.add(_col_lb);
		++pn_gc.gridy;
		pn_gb.setConstraints(_prog_cb, pn_gc);
		pn_pn.add(_prog_cb);
		pn_gc.gridy = 0;
		pn_gc.gridheight = 3;
		++pn_gc.gridx;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(180, 100));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		spc = hopper;
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(5, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		pn_gc.gridy += pn_gc.gridheight;
		pn_gc.gridheight = 1;
		pn_gc.gridwidth = pn_gc.gridx;
		pn_gc.gridx = 0;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(5, 5));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridy;
		JSeparator sp = new JSeparator(SwingConstants.HORIZONTAL);
		sp.setPreferredSize(new Dimension(_image.getIconWidth(), 3));
		sp.setForeground(Color.black);
		pn_gb.setConstraints(sp, pn_gc);
		pn_pn.add(sp);
		pn_gc.gridwidth = 1;
	}

	private void ibm029Panel(CardPunchOptions opts) {
		// Now the switch panel
		pn_gc.gridx = 0;
		pn_gc.gridy = 0;
		JPanel spc = new JPanel();
		spc.setPreferredSize(new Dimension(10, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		labeledToggle(_interp_cb, "INTERP", "PUNCH", true);
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(60, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		labeledToggle(_autoSD_cb, "ON", "AUTO<BR>SKIP<BR>DUP", true);
		JSeparator sp = new JSeparator(SwingConstants.VERTICAL);
		sp.setPreferredSize(new Dimension(3, 90));
		sp.setForeground(Color.black);
		pn_gb.setConstraints(sp, pn_gc);
		pn_pn.add(sp);
		++pn_gc.gridx;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(100, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		labeledToggle(_progSel_cb, "ONE", "TWO<BR>PROG<BR>SEL", true);
		labeledToggle(_autoFeed_cb, "ON", "AUTO<BR>FEED", true);
		labeledToggle(_print_cb, "ON", "PRINT", true);
		labeledToggle(_lzprint_cb, "ON", "LZ<BR>PRINT", true);
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(80, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		sp = new JSeparator(SwingConstants.VERTICAL);
		sp.setPreferredSize(new Dimension(3, 90));
		sp.setForeground(Color.black);
		pn_gb.setConstraints(sp, pn_gc);
		pn_pn.add(sp);
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(40, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
		labeledToggle(_clear_bn, "ON", "CLEAR", true);
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(5, 20));
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
	}

	public void start() {
		Thread t = new Thread(this);
		t.start();
	}

	private void setCursor(int curs) {
		_cursor = curs;
		if (_cursor <= 0) {
			_col_lb.setText("");
		} else {
			_col_lb.setText(Integer.toString(_cursor));
		}
	}

	// This might recurse, but only at field start and until end of card
	private void nextCol() {
		setCursor(_cursor + 1);
		_endOfCard = !(_cursor <= 80);
		if (!_endOfCard && _autoSD_cb.isSelected()) {
			int p = getProg(_prog, _cursor - 1);
			if ((p & (DUP | FIELD)) == DUP) {
				dupStart();
			} else if ((p & (SKIP | FIELD)) == SKIP) {
				skipStart();
			}
		}
	}

	private void ejectCard() {
		if (_prev != null) {
			stacker.putCard(_prev);
			_prev = null;
		}
		_prev = _code;
		_code = null;
		setState(_code, false);
	}

	private void newCard(boolean blank) {
		_endOfCard = false;
		_code = new byte[2*80];
		Arrays.fill(_code, (byte)0);
		if (!blank && hopper.getCard(_code) < 0) {
			_code = null;
		}
		setState(_code, false);
		++_pgix;
		setCursor(1);
		repaint();
	}

	private void skipStart() {
		if (_currIsProg) {
			setProg(false);
			return;
		}
		nextCol();
		while ((getProg(_prog, _cursor - 1) & FIELD) != 0) {
			nextCol();
		}
		repaint();
	}

	private void setProg(boolean in) {
		// TODO: animate this?
		if (in) {
			setState(_prog, false);
			_col_lb.setBackground(Color.yellow);
		} else {
			setState(_code, false);
			_col_lb.setBackground(Color.white);
		}
		_currIsProg = in;
		_endOfCard = false;
		setCursor(1);
		repaint();
	}

	// Animate movement of card out of sight to the left.
	private void cardOutLeft() {
		setCursor(0);
		_tranX = _tranY = 0;
		_animate = true;
		int tEnd = -_image.getIconWidth();
		for (; _tranX >= tEnd; _tranX -= 10) {
			repaint();
			try {
				Thread.sleep(5);
			} catch (Exception ee) {}
		}
		setState(null, false);
		setCursor(1);
	}

	// Animate movement of card into sight from the top (moving down).
	private void cardInDown() {
		setCursor(0);
		_tranY = -_image.getIconHeight();
		_animate = true;
		for (; _tranY <= 0; _tranY += 10) {
			repaint();
			try {
				Thread.sleep(5);
			} catch (Exception ee) {}
		}
		_tranY = 0;
		_animate = false;
		setCursor(1);
	}

	// Animate movement of card out of sight to the top (moving up).
	private void cardOutUp() {
		setCursor(0);
		_tranX = _tranY = 0;
		int tEnd = -_image.getIconHeight();
		_animate = true;
		for (; _tranY >= tEnd; _tranY -= 10) {
			repaint();
			try {
				Thread.sleep(5);
			} catch (Exception ee) {}
		}
		setState(null, false);
		setCursor(1);
	}

	private void cardOutRight() {
		setCursor(0);
		_tranX = _tranY = 0;
		_animate = true;
		int tEnd = _image.getIconWidth();
		for (; _tranX <= tEnd; _tranX += 10) {
			repaint();
			try {
				Thread.sleep(5);
			} catch (Exception ee) {}
		}
		setState(null, false);
		setCursor(1);
	}

	private void cardInRight() {
		setCursor(0);
		_tranY = 0;
		_tranX = _image.getIconWidth();
		_animate = true;
		for (; _tranX >= 0; _tranX -= 10) {
			repaint();
			try {
				Thread.sleep(5);
			} catch (Exception ee) {}
		}
		_tranX = 0;
		_animate = false;
		setCursor(1);
	}

	private void shred() {
		if (_curr == null) {
			return;
		}
		// Animate the destruction of the card to the right...
		cardOutRight();
		if (_currIsProg) {
			setProg(false);
			Arrays.fill(_prog, (byte)0);
			_prog_cb.setSelected(false);
		} else {
			_code = null;
			setState(_code, false);
		}
		repaint();
	}

	private void blank() {
		if (_curr != null) {
			release();
		}
		newCard(true);
		cardInRight();
	}

	private void feed() {
		if (_curr != null) {
			return;
		}
		newCard(false);
		cardInDown();
	}

	// Not called if no Card or _currIsProg...
	// This only logically finishes the card.
	// It makes no changes to card positions in machine.
	private void finishDataCard() {
		if (_autoSD_cb.isSelected()) {
			// Must scan rest of program card for auto-dup fields.
			// Let nextCol() handle that, though.
			while (!_endOfCard) {
				nextCol();
			}
			// Allow user to glipse results...
			changed = true;
		}
		if (_saveImage) { // OBSOLETE
			String fn = String.format("pcard%02d.png", _pgix);
			saveImage(new File(fn), false);
		}
	}

	// Only called if _currIsProg, back to data card
	// does not affect Reading Station!
	private void finishProgCard() {
		setCursor(0);
		cardOutRight();
		setProg(false);
		if (_curr != null) {
			cardInDown();
			setCursor(1);
			repaint();
		}
	}

	// Only called if !_currIsProg, switching to prog card
	// does not affect Reading Station!
	private void switchToProg() {
		if (_curr != null) {
			finishDataCard(); // TODO: too much?
			cardOutUp();
		}
		setProg(true);
		cardInRight();
		setCursor(1);
		repaint();
	}

	// Only called if !_currIsProg, (and !next-is-prog-card)
	// Passes data card to reader station, and any card in
	// reader station to the stacker.
	private void release() {
		reading.setBackground(Color.gray);
		if (_curr != null) {
			finishDataCard();
			setCursor(0);
			if (changed) {
				repaint();
				try {
					Thread.sleep(150);
				} catch (Exception ee) {}
			}
			// Animate the passing of the card to the left...
			cardOutLeft();
		}
		ejectCard();
		if (_prev != null) {
			reading.setBackground(CardHandler.buff1);
		}
		repaint();
	}

	private void interpret() {
		int x;
		while (_cursor <= 80) {
			// TODO: handle program card...
			punch(0x1000, false);
			repaint();
			try {
				Thread.sleep(5);
			} catch (Exception ee) {}
		}
		changed = true;
	}

	// Can't reach here if _curr == null?
	private void repair() {
		int cx = (_cursor - 1) * 2;
		_curr[cx] = 0;
		_curr[cx + 1] = 0;
		repaint();
	}

	// Can't reach here if _curr == null?
	private void punch(int p, boolean multi) {
		if (_endOfCard) {
			return;
		}
		if (p != 0) {
			p &= 0x0fff;
			// this corrupts 'p'...
			if (_print_cb.isSelected()) {
				p |= 0x1000;
			}
			int cx = (_cursor - 1) * 2;
			_curr[cx] |= (byte)(p & 0x0ff);
			_curr[cx + 1] |= (byte)((p >> 8) & 0x0ff);
		}
		if (!multi) {
			nextCol();
		}
	}

	private void dupStart() {
		// If start of field, then dup entire field...
		boolean cont = ((getProg(_prog, _cursor - 1) & FIELD) == 0);
		do {
			int p = 0;
			if (_prev != null) {
				p = getCode(_prev, _cursor - 1) & 0x0fff;
			}
			punch(p, false);
			cont = cont &&
				((getProg(_prog, _cursor - 1) & FIELD) != 0);
		} while (cont);
		repaint();
	}

	public void newCheck() {
		if (_curr == null) {
			return;
		}
		if (_interp_cb.isSelected()) {
			interpret();
			// We know we are at end of card now...
			if (_autoFeed_cb.isSelected()) {
				release();
				_keyQue.add(0x2000); // FEED
			}
			return;
		}
		if (_cursor <= 1) {
			_cursor = 0; // nextCol updates display
			nextCol();
		}
	}

	// Must not tie-up the Event Dispatch Thread... queue-up key and return...
	public void keyPressed(KeyEvent e) {
		boolean multi = ((e.getModifiers() & InputEvent.ALT_MASK) != 0);
		boolean shift = ((e.getModifiers() & InputEvent.SHIFT_MASK) != 0);
		int k = e.getKeyCode();
		int evt;
		if (k == KeyEvent.VK_F6) {
			evt = 004; // Ctrl-D DUP
		} else if (k == KeyEvent.VK_HOME) {
			evt = 002; // Ctrl-B
		} else if (k == KeyEvent.VK_END) {
			evt = 005; // Ctrl-E interpret
		} else if (k == KeyEvent.VK_DELETE) {
			if (shift) {
				evt = 0177; // DEL erase punches
			} else {
				evt = 030; // Ctrl-X delete card
			}
		} else if (k == KeyEvent.VK_INSERT) {
			evt = 016; // Ctrl-N insert new card
		} else if (k == KeyEvent.VK_F1) {
			if (shift) {
				evt = 0x4000; // curr to prog drum...
			} else {
				evt = 001; // Ctrl-A program card in/out
			}
		} else if (k == KeyEvent.VK_F12) { // FEED
			evt = 0x2000;
		} else {
			char c = e.getKeyChar();
			if (c == KeyEvent.CHAR_UNDEFINED) {
				return;
			}
			evt = (int)c;
		}
		if (multi) {
			evt |= 0x1000;
		}
		_keyQue.add(evt);
	}

	public void keyTyped(KeyEvent e) { }

	public void keyReleased(KeyEvent e) { }

	public void run() {
		int c = 0;
		while (true) {
			try {
				c = _keyQue.take();
			} catch (Exception ee) {
				break;
			}
			if (c == 0x4000) {	// move current to program drum
				if (_curr == null) {
					continue;
				}
				if (_currIsProg) {
					// ???
					continue;
				}
				_prog = _code;
				_code = null;
				_codeCard = true; // NO code card...
				finishProgCard();
				_currIsProg = true;
				continue;
			}
			if (c == 0x3000) {	// CLEAR
				release();
				// We never feed a new card here, so no need
				// to check for any automated tasks.
				if (_prev != null) {
					// make visible delay
					try {
						Thread.sleep(300);
					} catch (Exception ee) {}
					ejectCard();
					reading.setBackground(Color.gray);
				}
				continue;
			}
			if (c == 0x2000) {	// FEED
				// starting new card, check for automated tasks
				// (includes interpret).
				feed();
				newCheck();
				continue;
			}
			boolean multi = ((c & 0x1000) != 0);
			c &= 0x7f;
			int p = 0;
			if (c == '\005') {	// ^E
				interpret();
				// TODO: auto-feed
				continue;
			}
			if (c == '\002') {	// ^B
				setCursor(1);
				repaint();
				continue;
			}
			if (c == '\030') {	// ^X
				shred();
				continue;
			}
			if (c == '\016') {	// ^N
				// insert blank card
				blank();
				continue;
			}
			if (c == '\n') {
				release();
				if (_autoFeed_cb.isSelected()) {
					_keyQue.add(0x2000);
				}
				continue;
			}
			if (c == '\001') {	// ^A
				if (_currIsProg) {
					finishProgCard();
				} else {
					switchToProg();
				}
				continue;
			}
			if (_curr == null) {
				continue;
			}
			// From here on, we must have a valid _cursor...
			if (_cursor < 1) {
				setCursor(1);
			}
			if (c == '\t') {
				skipStart();
				if (_endOfCard && _autoFeed_cb.isSelected()) {
					changed = true;
					release();
					_keyQue.add(0x2000); // FEED
				}
				continue;
			}
			if (c == 0x7f) {
				repair();
				continue;
			}
			if (c == '\b') {
				if (_cursor > 1) {
					setCursor(_cursor - 1);
					repaint();
				}
				continue;
			}
			if (c == '\004') {	// DUP
				dupStart();
				if (_endOfCard && _autoFeed_cb.isSelected()) {
					changed = true;
					release();
					_keyQue.add(0x2000); // FEED
				}
				continue;
			}
			// TODO: handle ALHPA SHIFT
			// if ((c & 0x100) == 0) {
			// }
			c = Character.toUpperCase(c);
			p = _cvt.asciiToPun((int)c);
			if (p < 0) {
				continue;
			}
			punch(p, multi);
			repaint();
			if (_endOfCard && _autoFeed_cb.isSelected()) {
				changed = true;
				release();
				_keyQue.add(0x2000);
			}
		}
	}

	private void saveImage(File fn, boolean transparent) {
		Dimension d = getSize();
		java.awt.image.BufferedImage i = new java.awt.image.BufferedImage(
			d.width, d.height, transparent ?
				java.awt.image.BufferedImage.TYPE_INT_ARGB :
				java.awt.image.BufferedImage.TYPE_INT_RGB);
		int c = _cursor;
		_cursor = 0; // turn off cursor
		paint(i.getGraphics());
		if (transparent) {
			for (int x = 0; x < i.getWidth(); ++x) {
			for (int y = 0; y < i.getHeight(); ++y) {
				int px = i.getRGB(x, y);
				if (hole.equals(new Color(px, false))) {
					i.setRGB(x, y, 0);
				}
			}}
		}
		try {
			javax.imageio.ImageIO.write(i, "png", fn);
		} catch (IOException ee) {
			System.err.println("error writing " + fn.getAbsolutePath());
		}
		_cursor = c;
	}

	private File pickFile(String purpose, JComponent acc,
				String sfx, String typ, File prev) {
		File file;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{sfx}, new String[]{typ}, prev, acc);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		} else {
			file = null;
		}
		return file;
	}

	private void loadProg(File file) {
		FileInputStream f;
		try {
			f = new FileInputStream(file);
			int n = f.read(_prog);
			if (n < 160) {
				Arrays.fill(_prog, n, 160, (byte)0);
			}
		} catch (Exception ee) {
			// TODO: report errors
			ee.printStackTrace();
		}
	}

	private void saveProg(File file) {
		FileOutputStream f = null;
		try {
			f = new FileOutputStream(file);
		} catch (Exception ee) {
		}
		if (f == null) {
			return;
		}
		try {
			f.write(_prog);
		} catch (Exception ee) {
			// TODO: report errors
			ee.printStackTrace();
		}
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
		java.net.URL url = this.getClass().getResource("docs/About.html");
		try {
			JEditorPane about = new JEditorPane(url);
			about.setEditable(false);
			Dimension dim = new Dimension(300, 230);
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

	private void deckView(CardStacker stk) {
		if (viewer == null) {
			if (manager == null) {
				viewer = DataCenter.makeViewer();
				if (viewer == null) {
					// TODO: pop-up error
					return;
				}
				viewer.getFrame().addWindowListener(this);
				viewer.setQuitListener(this);
			} else {
				viewer = manager.getViewer();
				if (viewer == null) {
					// TODO: pop-up error
					return;
				}
			}
		}
		viewer.viewDeck(stk.getDeck(), _ibm026, _fortran);
	}

	private void deckSave(CardStacker stk) {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		File fi = stk.saveDialog("Save Output", dir);
		// TODO: set _cwd also?
		if (fi != null && manager != null) {
			manager.setCardDir(fi);
		}
	}

	private void deckChange(CardHandlerEvent ae, CardHandler obj, String act) {
		if (act.equals("left")) {
			if (obj instanceof CardStacker) {
				ae.consume();
				deckSave((CardStacker)obj);
			} else if (obj instanceof CardHopper) {
				ae.consume();
				deckAdd((CardHopper)obj);
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
			if (!butt.equals(_clear_bn)) {
				return;
			}
			_keyQue.add(0x3000);
			return;
		} else if (e instanceof CardHandlerEvent) {
			// hopper or stacker, mouse or repaint...
			CardHandler ch = (CardHandler)e.getSource();
			CardHandlerEvent ae = (CardHandlerEvent)e;
			String a = e.getActionCommand();
			if (a.equals("repaint")) {
				deckUpdate(ae, ch);
			} else {
				deckChange(ae, ch, a);
			}
			return;
		} else if (e.getSource() == viewer) {
			// any other shutdown?
			viewer = null;
		} else if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_O) {
			deckSave(stacker);
		} else if (m.getMnemonic() == KeyEvent.VK_D) {
			stacker.discardDeck();
		} else if (m.getMnemonic() == KeyEvent.VK_P) {
			File nu = pickFile("Snap Image", snap_acc,
				"png", "PNG Image", _cwd);
			if (nu != null) {
				saveImage(nu, snap_cb.isSelected());
			}
		} else if (m.getMnemonic() == KeyEvent.VK_I) {
			deckAdd(hopper);
		} else if (m.getMnemonic() == KeyEvent.VK_B) {
			hopper.addBlank(50);
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper.addBlank(0); // close any input... we hope.
			// stacker.discardDeck(); // file should get removed
			if (quit != null) {
				quit.actionPerformed(new ActionEvent(this, e.getID(), "quit"));
			} else {
				System.exit(0);
			}
		} else if (m.getMnemonic() == KeyEvent.VK_L) {
			File dir = _cwd;
			if (manager != null) {
				dir = manager.getDrumDir();
			}
			if (_progFile != null) {
				dir =  _progFile;
			}
			File nu = pickFile("Load Prog Card", null,
				"prc", "Program Card", dir);
			if (nu != null) {
				_progFile = nu;
				loadProg(_progFile);
				if (manager != null) {
					manager.setDrumDir(nu);
				}
			}
		} else if (m.getMnemonic() == KeyEvent.VK_S) {
			File dir = _cwd;
			if (manager != null) {
				dir = manager.getDrumDir();
			}
			if (_progFile == null) {
				File nu = pickFile("Save Prog Card As", null,
					"prc", "Program Card", dir);
				if (nu != null) {
					_progFile = nu;
				}
				if (manager != null) {
					manager.setDrumDir(nu);
				}
			}
			saveProg(_progFile);
		} else if (m.getMnemonic() == KeyEvent.VK_A) {
			showAbout();
		} else if (m.getMnemonic() == KeyEvent.VK_H) {
			showHelp();
		}
	}

	public void windowActivated(WindowEvent e) { }
	public void windowClosed(WindowEvent e) { }
	public void windowIconified(WindowEvent e) { }
	public void windowOpened(WindowEvent e) { }
	public void windowDeiconified(WindowEvent e) { }
	public void windowDeactivated(WindowEvent e) { }
	public void windowClosing(WindowEvent e) {
		if (viewer == null) return;
		if (e.getWindow() == viewer.getFrame()) {
			viewer.getFrame().setVisible(false);
			return;
		}
	}
}
