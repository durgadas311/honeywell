// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;
import java.util.Properties;
import java.util.Vector;

class ReproducingPunch implements Machine, ActionListener, Runnable
{
	static final Color red = new Color(255, 120, 120);
	static final Color off = new Color(190, 190, 180);

	class ProgSet {
		public ProgItem pit;
		public int col;
		public int wid;
		public ProgSet(ProgItem i, int c, int w) {
			pit = i;
			col = c;
			wid = w;
		}
	}

	class ReadingItem extends ProgItem {
		public ReadingItem(int w) {
			super(w);
		}
		public void processExits(byte[] card) {
			for (int c = 0; c < ents.length; ++c) {
				if (ents[c] == null) continue;
				int p = getCol(card, c);
				String t = cvt.punToAscii(p);
				if (t == null) continue;
				char h = t.charAt(0);
				ents[c].putCol(p, h);
			}
		}
	}

	class PunchingItem extends ProgItem {
		short[] punch;

		public PunchingItem(int w) {
			super(w);
			exit = false;
			punch = new short[w];
		}

		// Reset for new program panel
		public void reset() {
			super.reset();
			Arrays.fill(punch, (short)0);
		}

		@Override
		public ProgStart get(int p) {
			if (ents[p] == null) {
				ents[p] = new PunchingEntry(punch, p);
			}
			return ents[p];
		}

		// This generates punch output, and resets for next card
		public void punchCard(byte[] card) {
			for (int x = 0; x < punch.length; ++x) {
				punchCol(card, x, punch[x]);
			}
			Arrays.fill(punch, (short)0);
		}
	}

	class TSelector extends ProgStart {
		Selector i_pu;

		public TSelector(Selector I_PU) {
			super(true);
			i_pu = I_PU;
		}

		@Override
		public void putCol(int p, char c) {
			super.trigger(p, c);	// n/a ?
			// Any non-zero punch? or only '9'?
			i_pu.set((p & 0x001) != 0);
		}
	}

	class XSelector extends ProgStart {
		Selector i_pu;

		public XSelector(Selector I_PU) {
			super(true);
			i_pu = I_PU;
		}

		@Override
		public void set(boolean b) {
			i_pu.set(b);
		}

		@Override
		public void putCol(int p, char c) {
			super.trigger(p, c);	// n/a ?
			i_pu.set((p & 0x400) != 0);
		}
	}

	private String dumpSelectors() {
		String ret = "";
		for (int x = 0; x < selector.length; ++x) {
			if (selector[x] != null) {
				ret += selector[x].dump();
			} else {
				ret += '.';
			}
		}
		return ret;
	}

	JFrame _frame;
	Font labels;
	File _cwd;
	File tmp;
	CardHopper rhopper;
	CardStacker rstacker;
	CardHopper phopper;
	CardStacker pstacker;
	JButton start;
	JButton stop;
	JButton reset;
	// TODO: these are indicators...
	JButton idle;
	JButton comp;
	JButton dpbc;
	JButton form;
	JButton feed;

	Icon tog_up;
	Icon tog_dn;
	Icon tog_pr;
	JCheckBox repro;	// REPRODUCING switch
	JCheckBox selre;	// SEL REPO AND G P COMP
	JCheckBox detma;	// (card x punched) DETAIL-MASTER
	JCheckBox mapun;	// MASTER CARD PUNCHING
	GridBagLayout pn_gb;
	GridBagConstraints pn_gc;

	JTextArea text;
	JScrollPane scroll;
	CharConverter cvt;

	boolean stopped;
	ErrorStop errorStop;
	boolean ibm514;
	boolean progSet;
	boolean empty;
	boolean done;
	JMenu[] _menus;
	GridBagLayout gb;
	GridBagConstraints gc;

	GenericHelp _help;
	SuffFileChooser ch;
	Properties props;
	// Card data entry
	ReadingItem cbrush; // comparing brushes (read #2)
	ReadingItem rbrush; // repro brushes (read #1)
	ReadingItem pbrush; // punch brushes (punch #2)
	ReadingItem pxbrsh; // punch X brushes (punch #1)
	ReadingItem rxbrsh; // read X brushes (read #1)
	PunchingItem punch; // punch magnets (punch #1)

	boolean reading;
	boolean punching;
	byte[] rcard1;
	byte[] rcard2;
	byte[] pcard1;
	byte[] pcard2;

	// These are exits, may have watchers - one-shots

	SingleExit allCards;
	SingleExit allCycles;
	SingleExit rxDelay;
	SingleExit pxDelay;
	SingleEntry rxEntry;
	SingleEntry pxEntry;
	ColumnSplit csplits;

	ComparingMagnets comparing;
	Selector[] selector;

	public JMenu[] getMenu() { return _menus; }
	public JFrame getFrame() { return _frame; }
	public void setQuitListener(ActionListener lstn) { quit = lstn; }
	private ActionListener quit = null;
	String title;
	Puncher puncher;

	AppManager manager;
	CardViewer viewer;

	public ReproducingPunch(JFrame frame, AppManager mgr) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		manager = mgr;
		viewer = null;
		_frame = frame;
		title = _frame.getTitle();
		ibm514 = false;	// TODO: configure
		errorStop = new ErrorStop(off);
		progSet = false;
		rbrush = new ReadingItem(80);
		cbrush = new ReadingItem(80);
		pbrush = new ReadingItem(80);
		pxbrsh = new ReadingItem(80);
		rxbrsh = new ReadingItem(80);
		punch = new PunchingItem(80);
		rcard1 = null;
		rcard2 = null;
		pcard1 = null;
		pcard2 = null;
		selector = new Selector[2];
		csplits = new ColumnSplit(8, false);

		comparing = new ComparingMagnets(80);

		// These are one-shots, used for watchers only
		allCards = new SingleExit();
		allCycles = new SingleExit();
		// TODO: hook these up...
		// NOTE: connecting RD to RX must cause selection for
		// all cycles after first RX (i.e. latching). (PD, PX)
		// (manual reset required - could fudge it with RESET).
		// Ex:	rx = rd rx1
		//	s1x = rx1
		// must cause s1x to transfer every cycle after first!
		rxDelay = new SingleExit();
		pxDelay = new SingleExit();
		rxEntry = new SingleEntry();
		pxEntry = new SingleEntry();

		_cwd = new File(System.getProperty("user.dir"));
		cvt = new CharConverter();
		tog_up = new ImageIcon(getClass().getResource("icons/ibm026-30-on.png"));
		tog_dn = new ImageIcon(getClass().getResource("icons/ibm026-30-off.png"));
		tog_pr = new ImageIcon(getClass().getResource("icons/ibm026-30-pr.png"));
		pn_gb = new GridBagLayout();
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
		pn_gc.anchor = GridBagConstraints.NORTH;

		stopped = true;
		rhopper = new CardHopper("Read Hopper", 20, 100, 1, false);
		rhopper.setListener(this);
		rstacker = new CardStacker("Read Stacker", 20, 100, 1, true);
		rstacker.setListener(this);
		phopper = new CardHopper("Punch Hopper", 20, 100, 1, false);
		phopper.setListener(this);
		pstacker = new CardStacker("Punch Stacker", 20, 100, 1, true);
		pstacker.setListener(this);
		start = makeButton("START", "start", Color.black, Color.white);
		stop = makeButton("STOP", "stop", Color.black, Color.white);
		reset = makeButton("RESET", "reset", Color.black, Color.white);
		// TODO: make indicators...
		idle = makeButton("", null, off, Color.black);
		idle.setFocusable(false);
		comp = makeButton("COMP", null, off, Color.black);
		comp.setFocusable(false);
		dpbc = makeButton("DPBC<BR>DETECT", null, off, Color.black);
		dpbc.setFocusable(false);
		form = makeButton("?", null, off, Color.black);
		form.setFocusable(false);
		feed = makeButton("?", null, off, Color.black);
		feed.setFocusable(false);
		errorStop.addLight(comp);
		errorStop.addLight(dpbc);
		errorStop.addLight(form);
		errorStop.addLight(feed);

		// Control Panel Switches
		repro = toggleSwitch();
		selre = toggleSwitch();
		detma = toggleSwitch();
		mapun = toggleSwitch();

		_menus = new JMenu[2];
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Load Prog", KeyEvent.VK_P);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Unload Prog", KeyEvent.VK_U);
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

		java.net.URL url = this.getClass().getResource("docs/ReproPunch.html");
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

		// Horiz border/dividers
		gc.gridwidth = 19;
		//gc.gridy = 0;
		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridy = 7;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 50));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridwidth = 17;
		gc.gridx = 1;
		gc.gridy = 2;
		pn = new JPanel();
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

		// Vert border/dividers
		gc.gridwidth = 1;
		gc.gridheight = 5;
		gc.gridy = 1;
		gc.gridx = 0;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 18;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 16;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(100, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);

		// hoppers/stackers
		gc.gridheight = 1;
		gc.gridx = 1;
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(rhopper, gc);
		_frame.add(rhopper);
		gc.gridx = 11;
		gb.setConstraints(phopper, gc);
		_frame.add(phopper);
		gc.gridy = 5;
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(rstacker, gc);
		_frame.add(rstacker);
		gc.gridx = 17;
		gb.setConstraints(pstacker, gc);
		_frame.add(pstacker);

		// lights/switches
		gc.gridheight = 1;
		gc.gridwidth = 1;
		gc.gridy = 3;
		gc.gridx = 1;
		gb.setConstraints(idle, gc);
		_frame.add(idle);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(comp, gc);
		_frame.add(comp);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(dpbc, gc);
		_frame.add(dpbc);
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
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
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
		gb.setConstraints(reset, gc);
		_frame.add(reset);

		// Control panel
		pn = makeSwitches();
		gc.gridheight = 2;
		gc.gridwidth = 9;
		gc.gridy = 5;
		gc.gridx = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(pn, gc);
		_frame.add(pn);

		// Comparing magnets
		pn = comparing;
		gc.gridheight = 1;
		gc.gridwidth = 4;
		gc.gridy = 6;
		gc.gridx = 13;
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		
		idle.setBackground(red);
	}

	// Main interfaces used by Puncher (Accounting Machine)
	public void connect(Puncher pun) {
		puncher = pun;
		if (pun == null) {
			_frame.setVisible(false); // ...or let user do this?
		} else {
			_frame.setVisible(true);
		}
		// TODO: other setup?
	}
	public boolean startPunch() {
		if (phopper.stackCount() == 0) {
			return true;
		}
		return doOneCycle();
	}
	public ProgItem summaryEntry() {
		return csplits.X();
	}
	/////////////////////////////////

	private ProgItem getReadCycle(char t) {
		switch (t) {
		case 'r': return rbrush;
		case 'p': return pbrush;
		case 'c': return cbrush;
		default: return null;
		}
	}

	private ProgItem parseEntry(String pm) {
		if (pm.equals("rx")) {
			return rxEntry;
		} else if (pm.equals("px")) {
			return pxEntry;
		}
		return null;
	}

	private ProgItem parseExit(String pm) {
		if (pm.equals("rd")) {
			return rxDelay;
		} else if (pm.equals("pd")) {
			return pxDelay;
		}
		return null; // or some dummy item?
	}

	// This ensures the counter gets created on first reference...
	// Returns 0-based index
	private int getCounter(String p) {
		if (!p.matches("[2468][abcd].*")) {
			return -1;
		}
		int w = p.charAt(0) - '0';	// 2,4,6,8
		int ctr = (w - 2) << 1;		// (0,2,4,6) 0,4,8,12
		ctr |= (p.charAt(1) - 'a');	// 0..15
		return ctr;
	}

	// TODO: pass in width... ???
	// Returns 0-based index
	private int getSelector(String p) {
		int sel = Integer.valueOf(p) - 1;
		if (selector[sel] == null) {
			selector[sel] = new Selector(10);
		}
		return sel;
	}

	// 'ctx': 0=EXIT, 1=ENTRY (2=ENTRY B...)
	private ProgSet parseItem(String p, int ctx) {
		ProgItem rd = null;
		int w = 1;
		int c = 1;	// start out 1-based
		int i = p.indexOf('*');
		if (i > 0) {
			w = Integer.valueOf(p.substring(i + 1));
			p = p.substring(0, i);
		}
		int ctr = getCounter(p);
		if (ctr >= 0 && p.length() >= 3) {
			char t = p.charAt(2);
			int nw = w;
			w = 1;
			if (Character.isDigit(t)) { // counter digit exit(s)
				w = nw;
				c = t - '0';
				rd = puncher.counterExit(ctr);
			}
		} else if (p.matches("[rpc]\\.[0-9]+")) {
			// REPRODUCING/PUNCH/COMPARING BRUSHES
			c = Integer.valueOf(p.substring(2));
			rd = getReadCycle(p.charAt(0));
		} else if (p.matches("[rp]x[0-9]+")) {
			// RX or PX BRUSHES
			c = Integer.valueOf(p.substring(2));
			if (p.charAt(0) == 'r') {
				rd = rxbrsh;
			} else {
				rd = pxbrsh;
			}
		} else if (p.matches("pm[0-9]+")) {
			// PUNCH MAGNETS
			c = Integer.valueOf(p.substring(2));
			rd = punch;
		} else if (p.matches("x[0-9]+[-+]")) {
			char t = p.charAt(p.length() - 1);
			c = Integer.valueOf(p.substring(1, p.length() - 1));
			// rd = ???
		} else if (p.matches("c[0-9]+[pc]")) {
			// COMPARING MAGNETS FROM PUNCH/COMP
			char t = p.charAt(p.length() - 1);
			c = Integer.valueOf(p.substring(1, p.length() - 1));
			if (t == 'p') {
				rd = comparing.A();
			} else {
				rd = comparing.B();
			}
		} else if (p.matches("s[0-9]+[rpt]")) {
			// SELECTOR RX/PX/T PU
			w = 1;
			char t = p.charAt(p.length() - 1);
			int sel = getSelector(p.substring(1, p.length() - 1));
			if (t == 't') {
				rd = new SingleEntry(new TSelector(selector[sel]));
			} else { // TODO: any diff btw RX/RD and PX/PD?
				rd = new SingleEntry(new XSelector(selector[sel]));
			}
		} else if (p.matches("s[0-9]+[cnx][0-9]+")) {
			// SELECTOR C/N/T contacts
			i = p.indexOf('c');
			if (i < 0) {
				i = p.indexOf('n');
				if (i < 0) {
					i = p.indexOf('t');
				}
			}
			char t = p.charAt(i);
			int sel = getSelector(p.substring(1, i));
			c = Integer.valueOf(p.substring(i + 1));
			selector[sel].resize(c); // 'c' is still +1 == width
			if (t == 'c') {
				selector[sel].C().setExit(ctx == 0);
				rd = selector[sel].C();
			} else if (t == 'n') {
				selector[sel].N().setExit(ctx == 0);
				rd = selector[sel].N();
			} else {
				selector[sel].T().setExit(ctx == 0);
				rd = selector[sel].T();
			}
		} else if (p.matches("cs[0-9]+[xdc]")) {
			// COLUMN SPLITS: 11-12 (x), 0-9 (d), and COM (c) hubs
			char t = p.charAt(p.length() - 1);
			c = Integer.valueOf(p.substring(2, p.length() - 1));
			if (t == 'x') {
				rd = csplits.X();
			} else if (t == 'd') {
				rd = csplits.D();
			} else {
				rd = csplits.C();
			}
		} else {
			w = 1;
			if (ctx == 0) {
				rd = parseExit(p);
			} else {
				rd = parseEntry(p);
			}
		}
		if (rd == null) {
			return null;
		}
		--c;	// 0-based
		return new ProgSet(rd, c, w);
	}

	private void loadProgram(String prog) {
		String err = "";
		props = new Properties();
		try {
			InputStream is = new FileInputStream(prog);
			props.load(is);
		} catch (Exception ee) {
			return;
		}
		// TODO: aN=3.x requires zN=2.x, produce erroneous output if not wired.
		for (String prop : props.stringPropertyNames()) {
			String p = props.getProperty(prop);
			String[] vals = p.split("\\s");
			Vector<ProgSet> pv = new Vector<ProgSet>();
			ProgSet p1 = parseItem(prop, 1);
			if (p1 == null) {
				err += String.format("%s = \"%s\"\n", prop, p);
				continue;
			}
			int n = p1.wid;
			boolean zero = false;
			for (String val : vals) {
				ProgSet p2 = parseItem(val, 0);
				if (p2 == null) {
					err += String.format("%s = \"%s\"\n", prop, p);
					continue;
				}
				if (p2.wid > n) n = p2.wid;
				pv.add(p2);
			}
			for (ProgSet p2 : pv) {
				int c1 = p1.col;
				int c2 = p2.col;
				int id = c1;
				for (int x = 0; x < n; ++x) {
					p1.pit.linkEntry(id, c1, p2.pit.get(c2));
					//p2.pit.linkEntry(id, c2, p1.pit.get(c1));
					++c1;
					++c2;
				}
			}
		}
		if (err.length() > 0) {
			PopupFactory.warning(_frame, "Property Errors",
				"<HTML><PRE>" + err + "</PRE></HTML>");
		}
		progSet = true;
	}

	private void labeledToggle(JPanel pn_pn, AbstractButton sw, String lab) {
		JPanel spc;
		pn_pn.setBackground(off);
		pn_pn.setOpaque(true);
		pn_gc.gridheight = 2;
		pn_gc.anchor = GridBagConstraints.CENTER;
		pn_gb.setConstraints(sw, pn_gc);
		pn_pn.add(sw);
		pn_gc.gridheight = 1;
		pn_gc.gridy += 2;
		pn_gc.anchor = GridBagConstraints.NORTH;
		if (lab.matches(".*<BR>.*")) {
			JLabel lb = new JLabel("<HTML><CENTER>" + lab + "</CENTER></HTML>");
			lb.setFont(labels);
			pn_gb.setConstraints(lb, pn_gc);
			pn_pn.add(lb);
			//spc = centeredLabel(lab);
			//pn_gb.setConstraints(spc, pn_gc);
			//pn_pn.add(spc);
		} else {
			JLabel lb = new JLabel(lab);
			lb.setFont(labels);
			pn_gb.setConstraints(lb, pn_gc);
			pn_pn.add(lb);
		}
		++pn_gc.gridx;
		// cleanup
		pn_gc.anchor = GridBagConstraints.CENTER;
		pn_gc.gridheight = 3;
		pn_gc.gridy = 0;
		spc = new JPanel();
		spc.setPreferredSize(new Dimension(5, 20));
		spc.setOpaque(false);
		pn_gb.setConstraints(spc, pn_gc);
		pn_pn.add(spc);
		++pn_gc.gridx;
	}

	// For this machine, "up" is OFF
	private JCheckBox toggleSwitch() {
		JCheckBox cb = new JCheckBox();
		cb.setFocusable(false);
		cb.setOpaque(false);
		cb.setText("");
		cb.setIcon(tog_up);
		cb.setSelectedIcon(tog_dn);
		cb.setPressedIcon(tog_pr);
		return cb;
	}

	private JPanel makeSwitches() {
		JPanel pn = new JPanel();
		pn.setLayout(pn_gb);
		pn.setOpaque(false);
		JLabel lb = new JLabel("OFF");
		lb.setFont(labels);
		pn_gc.anchor = GridBagConstraints.NORTH;
		pn_gb.setConstraints(lb, pn_gc);
		pn.add(lb);
		++pn_gc.gridy;
		lb = new JLabel("ON");
		lb.setFont(labels);
		pn_gc.anchor = GridBagConstraints.SOUTH;
		pn_gb.setConstraints(lb, pn_gc);
		pn.add(lb);
		pn_gc.gridy = 0;
		++pn_gc.gridx;
		labeledToggle(pn, repro, "REPRO");
		labeledToggle(pn, selre, "SEL<BR>REPO");
		labeledToggle(pn, detma, "X<BR>MASTER");
		labeledToggle(pn, mapun, "MASTER<BR>PUNCH");
		return pn;
	}

	private JButton makeButton(String lab, String act, Color bg, Color fg) {
		JButton btn = new JButton("<HTML><CENTER>" + lab + "</CENTER></HTML>");
		// TODO: indicator, not button
		if (act != null) {
			btn.setActionCommand(act);
			btn.addActionListener(this);
		} else {
			btn.setBorderPainted(false);
			btn.setFocusPainted(false);
		}
		btn.setFont(labels);
		btn.setPreferredSize(new Dimension(40, 40));
		btn.setMargin(new Insets(1, 1, 1, 1));
		btn.setBackground(bg);
		btn.setForeground(fg);
		btn.setOpaque(true);
		return btn;
	}

	static private int getCol(byte[] card, int ix) {
		int p = card[ix * 2] & 0x0ff;
		p |= (card[ix * 2 + 1] & 0x00f) << 8;
		return p;
	}

	static private void punchCol(byte[] card, int ix, int p) {
		card[ix * 2] |= (byte)p;
		card[ix * 2 + 1] |= (byte)(p >> 8);
	}

	int count = 0;

	private void processRead(ReadingItem ents, byte[] card) {
		if (card == null) {
			return;
		}
		ents.processExits(card);
	}

	private void processPunch(PunchingItem ents, byte[] card) {
		if (card == null) {
			return;
		}
		ents.punchCard(card);
	}

	private void changeSelectors() {
		for (Selector sel : selector) {
			if (sel != null) {
				sel.change();
			}
		}
	}

	// 'xt' was previously set 'true'...
	private void impulseCycle(ProgItem xt) {
		if (!xt.is(0)) {
			return;
		}
		allCycles.set(0, true);
		xt.set(0, false);
		allCycles.set(0, false);
	}

	// Do not require both cards to be fed, only one is required to
	// begin punching.
	// TODO: improve restart capability... need to be able to restart
	// where last stopped...
	private boolean doOneCycle() {
		boolean both = repro.isSelected();
		boolean rfeed = true;
		boolean pfeed = true;
		//reading = repro.isSelected() || selre.isSelected();
		//punching = repro.isSelected();
		empty = false;
		done = false;
		allCycles.set(0, true);
		allCards.set(0, true);
		if (both && (rhopper.stackCount() == 0 || phopper.stackCount() == 0)) {
			// stop feed both, runout cards...
			rfeed = false;
			pfeed = false;
		} else {
			rfeed = (rhopper.stackCount() > 0 || rcard2 != null);
			pfeed = (phopper.stackCount() > 0 || pcard2 != null);
		}
		// Always advance cards, even if not feeding new ones...
		if (rcard2 != null) {
			rstacker.putCard(rcard2);
		}
		rcard2 = rcard1;
		rcard1 = null;
		if (rfeed) {
			rcard1 = new byte[2*80];
			int c = rhopper.getCard(rcard1);
			if (c < 0) {
				rcard1 = null;
			}
			empty = empty || (rhopper.stackCount() == 0);
			done = done || (rcard2 == null && rcard1 == null);
		}
		if (pcard2 != null) {
			pstacker.putCard(pcard2);
		}
		pcard2 = pcard1;
		pcard1 = null;
		if (pfeed) {
			pcard1 = new byte[2*80];
			int c = phopper.getCard(pcard1);
			if (c < 0) {
				pcard1 = null;
			}
			empty = empty || (phopper.stackCount() == 0);
			done = done || (pcard2 == null && pcard1 == null);
		}
		// Auto run-out?
		if (empty && done) {
			// end cycle? or return later?
			return true;
		}
		processRead(rxbrsh, rcard1);
		processRead(pxbrsh, pcard1);
		// TODO: impulse PD/RD here...
		// TODO: confirm that AcctMach will impulse TOTAL before this...
		changeSelectors();
		processRead(rbrush, rcard1);
		processRead(cbrush, rcard2);
		processRead(pbrush, pcard2);
++ncards;
		// TODO: are csplits only for punching???
		csplits.commit();
		if (!comparing.processExits()) {
			// error stop...
			comp.setBackground(red);
			errorStop.set(true);
			// end cycle? or return later?
			return true;
		}
		processPunch(punch, pcard1);
//System.err.format("at card %d %s\n", ncards, dumpSelectors());
		impulseCycle(allCards);	// End ALL CARDS cycle
		return false;
	}

public static int ncards = 0;

	public void run() {
		if (errorStop.is()) {
			return;
		}
		_frame.setTitle(title + " (running)");
		idle.setBackground(off);
		while (!stopped) {
			if (doOneCycle()) {
				stopped = true;
				break;
			}
			try {
				_frame.repaint();
				Thread.sleep(50);
			} catch (Exception ee) {}
		}
		if (empty) {
			idle.setBackground(red);
		}
		_frame.setTitle(title);
	}

	private File pickFile(String purpose,
				String sfx, String typ, File prev, JComponent acc) {
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

	private void showAbout() {
		java.net.URL url = this.getClass().getResource("docs/About5.html");
		try {
			JEditorPane about = new JEditorPane(url);
			about.setEditable(false);
			Dimension dim = new Dimension(300, 280);
			about.setPreferredSize(dim);
			JOptionPane.showMessageDialog(_frame, about,
				"About: Card Punch Simulator", JOptionPane.PLAIN_MESSAGE);
		} catch (Exception ee) { }
	}

	private void showHelp() {
		_help.setVisible(true);
	}

	private void unProg() {
		progSet = false;
		Arrays.fill(selector, null);
		comparing.reset();
		rbrush.reset();
		cbrush.reset();
		pbrush.reset();
		pxbrsh.reset();
		rxbrsh.reset();
		punch.reset();
		// clear exits
		allCards.reset();
		allCycles.reset();
	}

	private void getProg() {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getPanelDir();
		}
		File fi = pickFile("Get Prog", "51x", "IBM 51x Prog", dir, null);
		if (fi == null) {
			return;
		}
		unProg();
		loadProgram(fi.getAbsolutePath());
		if (manager != null) {
			manager.setPanelDir(fi);
		}
	}

	private void deckAdd(CardHopper hop) {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		File fi = hop.addDialog(hop.getLabel(), dir);
		if (fi != null && manager != null) {
			manager.setCardDir(fi);
		}
	}

	private void deckSave(CardStacker stk) {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		File fi = stk.saveDialog(stk.getLabel(), dir);
		if ( fi != null && manager != null) {
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
		if (act.equals("left")) {
			ae.consume();
			if (obj instanceof CardHopper) {
				deckAdd((CardHopper)obj);
			} else if (obj == pstacker) {
				deckSave((CardStacker)obj);
			} else {
				((CardStacker)obj).discardDeck();
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
				// TODO: indicate if no program is loaded...
				stopped = false;
				Thread t = new Thread(this);
				t.start();
			} else if (act.equals("stop")) {
				stopped = true;
			} else if (act.equals("reset")) {
				comparing.clear();
				errorStop.clear();
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
			return;
		} else if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_P) {
			getProg();
		} else if (m.getMnemonic() == KeyEvent.VK_U) {
			unProg();
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			rhopper.addBlank(0); // close any input... we hope.
			// rstacker.discardDeck(); // file should get removed
			if (quit != null) {
				quit.actionPerformed(new ActionEvent(this, e.getID(), "quit"));
			} else {
				System.exit(0);
			}
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_A) {
			showAbout();
		} else if (m.getMnemonic() == KeyEvent.VK_H) {
			showHelp();
		}
	}
}
