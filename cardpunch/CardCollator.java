// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;
import java.util.Properties;
import java.util.Vector;
import java.util.Random;

class CardCollator implements Machine, ActionListener, Runnable
{
	static final Color red = new Color(255, 120, 120);
	static final Color grn = new Color(120, 255, 120);
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

	class DelaySelector extends ProgStart {
		DelayStart delay;
		Selector i_pu;

		public DelaySelector(Selector I_PU) {
			super(true);
			i_pu = I_PU;
			delay = new DelayStart();
			delay.addWatcher(i_pu);
			allCycles.get(0).addWatcher(delay);
		}

		@Override
		public void set(boolean b) {
			super.trigger(b);	// n/a ?
			delay.setFlag(b);
		}

		@Override
		public void putCol(int p, char c) {
			super.trigger(p, c);	// n/a ?
			// Any punch in X/11 or 12
			delay.setFlag((p & 0x0c00) != 0);
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
		ret += ' ';
		for (int x = 0; x < delay.length; ++x) {
			if (delay[x] != null) {
				ret += delay[x].dump();
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
	CardHopper hopper1;
	CardHopper hopper2;
	CardStacker stacker1;
	CardStacker stacker2;
	CardStacker stacker3;
	CardStacker stacker4;
	JButton start;
	JButton stop;
	JButton runout;
	JButton reset;
	// TODO: these are indicators...
	JButton ready;
	JButton error;
	JButton bcd1;
	JButton bcd2;

	CharConverter cvt;

	boolean stopped;
	boolean runningOut;
	ErrorStop errorStop;
	boolean ibm087;
	boolean progSet;
	JPanel acc;
	JCheckBox acc_cb;
	JTextArea acc_stk;
	JMenu[] _menus;
	GridBagLayout gb;
	GridBagConstraints gc;

	GenericHelp _help;
	SuffFileChooser ch;
	Properties props;
	// Card data entry
	ReadingItem read1s;	// sequence read
	ReadingItem read1;	// primary read
	ReadingItem read2;	// secondar read
	byte[] card1;
	byte[] card2;
	byte[] card1s;
	CardStacker out1;
	CardStacker out2;

	// These are exits, may have watchers - one-shots

	SingleExit allCards;
	SingleExit allCycles;

	SingleEntry errStp;
	SingleEntry priFeed;
	SingleEntry priEject;
	SingleEntry secdyFd;
	SingleEntry priSel;
	SingleEntry secdySel3;
	SingleEntry secdySel4;
	// "switches"
	ProgStart zone;

	HELComparator sequence;
	HELComparator selection;
	Counter[] counter;
	Selector[] selector;
	Selector[] delay;

	public JMenu[] getMenu() { return _menus; }
	public JFrame getFrame() { return _frame; }
	public void setQuitListener(ActionListener lstn) { quit = lstn; }
	private ActionListener quit = null;
	String title;

	AppManager manager;
	CardViewer viewer;

	public CardCollator(JFrame frame, AppManager mgr) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		manager = mgr;
		viewer = null;
		_frame = frame;
		title = _frame.getTitle();
		ibm087 = false;	// TODO: configure
		progSet = false;
		read1 = new ReadingItem(80);
		read2 = new ReadingItem(80);
		read1s = new ReadingItem(80);
		card1 = null;
		card2 = null;
		card1s = null;
		out1 = null;
		out2 = null;
		counter = new Counter[1];
		selector = new Selector[5];
		delay = new Selector[2];
		sequence = new HELComparator(16);
		selection = new HELComparator(16);

		allCards = new SingleExit();
		allCycles = new SingleExit();

		//errStp = new SingleEntry();
		errStp = new SingleEntry();
		priFeed = new SingleEntry();
		priEject = new SingleEntry();
		secdyFd = new SingleEntry();
		priSel = new SingleEntry();
		secdySel3 = new SingleEntry();
		secdySel4 = new SingleEntry();
		zone = new ProgStart(false);

		_cwd = new File(System.getProperty("user.dir"));
		cvt = new CharConverter();

		stopped = true;
		runningOut = false;
		errorStop = new ErrorStop(off);
		hopper1 = new CardHopper("Primary Hopper", 20, 100, 1, false);
		hopper2 = new CardHopper("Secondary Hopper", 20, 100, 1, false);
		hopper1.setListener(this);
		hopper2.setListener(this);
		deckUpdate(hopper1);
		deckUpdate(hopper2);
		stacker1 = new CardStacker("Stacker 1", 20, 100, 1, true);
		stacker2 = new CardStacker("Stacker 2", 20, 100, 1, true);
		stacker3 = new CardStacker("Stacker 3", 20, 100, 1, true);
		stacker4 = new CardStacker("Stacker 4", 20, 100, 1, true);
		stacker1.setListener(this);
		stacker2.setListener(this);
		stacker3.setListener(this);
		stacker4.setListener(this);
		deckUpdate(stacker1);
		deckUpdate(stacker2);
		deckUpdate(stacker3);
		deckUpdate(stacker4);
		start = makeButton("START", "start", Color.black, Color.white, 1.0);
		stop = makeButton("STOP", "stop", Color.black, Color.white, 1.0);
		runout = makeButton("RUNOUT", "runout", Color.black, Color.white, 0.8);
		reset = makeButton("RESET", "reset", Color.black, Color.white, 1.0);
		// TODO: make indicators...
		ready = makeButton("READY", null, off, Color.black, 1.0);
		ready.setFocusable(false);
		error = makeButton("ERROR", null, off, Color.black, 1.0);
		error.setFocusable(false);
		bcd1 = makeButton("BCD 1", null, off, Color.black, 1.0);
		bcd1.setFocusable(false);
		bcd2 = makeButton("BCD 2", null, off, Color.black, 1.0);
		bcd2.setFocusable(false);
		errorStop.addLight(error);
		errorStop.addLight(bcd1);
		errorStop.addLight(bcd2);

		_menus = new JMenu[3];
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Load Prog", KeyEvent.VK_P);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Unload Prog", KeyEvent.VK_U);
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
		_menus[1] = mu;
		mu = new JMenu("Help");
		mi = new JMenuItem("About", KeyEvent.VK_A);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Show Help", KeyEvent.VK_H);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[2] = mu;

		java.net.URL url = this.getClass().getResource("docs/Collator.html");
		_help = new GenericHelp(frame.getTitle() + " Help", url);

		JLabel lb;
		JPanel pn;
		gb = new GridBagLayout();
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

		// sub-panel of stackers...
		JPanel stk = new JPanel();
		stk.setLayout(gb);
		stk.setOpaque(false);
		lb = new JLabel("4");
		gb.setConstraints(lb, gc);
		stk.add(lb);
		gc.gridx += 2;
		lb = new JLabel("3");
		gb.setConstraints(lb, gc);
		stk.add(lb);
		gc.gridx += 2;
		lb = new JLabel("2");
		gb.setConstraints(lb, gc);
		stk.add(lb);
		gc.gridx += 2;
		lb = new JLabel("1");
		gb.setConstraints(lb, gc);
		stk.add(lb);
		gc.gridx = 0;
		++gc.gridy;
		gb.setConstraints(stacker4, gc);
		stk.add(stacker4);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		stk.add(pn);
		++gc.gridx;
		gb.setConstraints(stacker3, gc);
		stk.add(stacker3);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		stk.add(pn);
		++gc.gridx;
		gb.setConstraints(stacker2, gc);
		stk.add(stacker2);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		stk.add(pn);
		++gc.gridx;
		gb.setConstraints(stacker1, gc);
		stk.add(stacker1);

		// main frame
		gb = new GridBagLayout();
		frame.setLayout(gb);
		gc.anchor = GridBagConstraints.NORTH;
		gc.gridx = 0;
		gc.gridy = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gc.gridwidth = 21;
		//gc.gridy = 0;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridy = 5;
		//gc.gridx = 0;
		//gc.gridwidth = 21;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 40));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridy = 2;
		gc.gridx = 1;
		gc.gridwidth = 15;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 20));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);

		gc.gridwidth = 1;
		gc.gridheight = 3;
		gc.gridx = 0;
		gc.gridy = 1;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(40, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 16;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(20, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 18;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		gc.gridx = 20;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);

		gc.gridheight = 3;
		gc.gridx = 17;
		gc.gridy = 1;
		gc.anchor = GridBagConstraints.SOUTH;
		gb.setConstraints(hopper2, gc);
		_frame.add(hopper2);
		gc.gridx = 19;
		gc.gridy = 2;
		gc.anchor = GridBagConstraints.NORTH;
		gb.setConstraints(hopper1, gc);
		_frame.add(hopper1);

		gc.gridheight = 2;
		gc.gridwidth = 15;
		gc.gridy = 3;
		gc.gridx = 1;
		gb.setConstraints(stk, gc);
		_frame.add(stk);

		gc.gridheight = 1;
		gc.gridwidth = 1;
		gc.gridy = 1;
		gc.gridx = 1;
		gb.setConstraints(bcd1, gc);
		_frame.add(bcd1);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(bcd2, gc);
		_frame.add(bcd2);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(error, gc);
		_frame.add(error);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(ready, gc);
		_frame.add(ready);
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
		gb.setConstraints(runout, gc);
		_frame.add(runout);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(reset, gc);
		_frame.add(reset);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(5, 5));
		pn.setOpaque(false);
		gb.setConstraints(pn, gc);
		_frame.add(pn);
		++gc.gridx;
		gb.setConstraints(stop, gc);
		_frame.add(stop);

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
		lb = new JLabel("Input Hopper:");
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

		ready.setBackground(grn);
	}

	private ProgItem getReadCycle(char t) {
		switch (t) {
		case 'p': return read1;
		case 's': return read2;
		case 'q': return read1s;
		default: return null;
		}
	}

	private ProgItem parseEntry(String pm) {
		if (pm.equals("secdy3")) {
			return secdySel3;
		} else if (pm.equals("secdy4")) {
			return secdySel4;
		} else if (pm.equals("pri")) {
			return priSel;
		} else if (pm.equals("eject")) {
			return priEject;
		} else if (pm.equals("priFeed")) {
			return priFeed;
		} else if (pm.equals("secdyFd")) {
			return secdyFd;
		} else if (pm.equals("error")) {
			return errStp;
		}
		return null;
	}

	private ProgItem parseExit(String pm) {
		if (pm.equals("cards")) {
			return allCards;
		} else if (pm.equals("all")) {
			return allCycles;
		}
		return null; // or some dummy item?
	}

	// This ensures the counter gets created on first reference...
	// Returns 0-based index
	private int getCounter(String p) {
		return -1;
	}

	// TODO: pass in width... ???
	// Returns 0-based index
	private int getSelector(String p) {
		int sel = Integer.valueOf(p) - 1;
		if (selector[sel] == null) {
			selector[sel] = new Selector(5);
		}
		return sel;
	}
	private int getDelaySel(String p) {
		int sel = Integer.valueOf(p) - 1;
		if (delay[sel] == null) {
			delay[sel] = new Selector(1);
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
			if (t == 't') { // TOTAL ENTRY
				rd = counter[ctr].TOTAL();
			} else if (t == 'y') { // CARRY EXIT/ENTRY
				if (ctx == 0) {
					rd = counter[ctr].CI();
				} else {
					rd = counter[ctr].C();
				}
			} else if (Character.isDigit(t)) { // digit
				w = nw;
				c = t - '0';
				if (ctx == 0) {
					rd = counter[ctr].X();
				} else {
					rd = counter[ctr].E();
				}
			} else if (t == 'c') { // CR SYMBOL EXIT
				rd = counter[ctr].CR();
			} else if (t == 'm') { // MINUS ENTRY
				rd = counter[ctr].MINUS();
			} else if (t == 'p') { // PLUS ENTRY
				rd = counter[ctr].PLUS();
			} else if (t == 'n') { // NEGATIVE BALANCE TEST/CTRL
				if (ctx == 0) {
					rd = counter[ctr].NBT();
				} else {
					rd = counter[ctr].NBC();
				}
			} else if (t == 's') { // CTR EXIT SUPPRESSION
				rd = counter[ctr].SUPP();
			}
		} else if (p.matches("[psq]\\.[0-9]+")) {
			// Pri/Sec/Seq READING EXITs
			c = Integer.valueOf(p.substring(2));
			rd = getReadCycle(p.charAt(0));
		} else if (p.matches("[ps]s[0-9]+")) {
			// Selection Comparing entry
			char t = p.charAt(0);
			c = Integer.valueOf(p.substring(2));
			if (t == 'p') {
				rd = selection.A();
			} else {
				rd = selection.B();
			}
		} else if (p.matches("[ps]q[0-9]+")) {
			// Sequence Comparing entry
			char t = p.charAt(0);
			c = Integer.valueOf(p.substring(2));
			if (t == 'p') {
				rd = sequence.A();
			} else {
				rd = sequence.B();
			}
		} else if (p.matches("pu[0-9]+")) {
			w = 1;
			int sel = getSelector(p.substring(2));
			rd = new SingleEntry(selector[sel]);
		} else if (p.matches("pu[0-9]+[cnt]")) {
			// SELECTOR C/N/T contacts
			i = p.length() - 1;
			char t = p.charAt(i);
			int sel = getSelector(p.substring(2, i));
			c = 1;
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
		} else if (p.matches("dy[0-9]+")) {
			// CYCLE DELAY PU
			w = 1;
			int sel = getDelaySel(p.substring(2));
			rd = new SingleEntry(new DelaySelector(delay[sel]));
		} else if (p.matches("dy[0-9]+[cnt]")) {
			// CYCLE DELAY C/N/T contacts
			i = p.length() - 1;
			char t = p.charAt(i);
			int sel = getDelaySel(p.substring(2, i));
			c = 1;
			delay[sel].resize(c); // 'c' is still +1 == width
			if (t == 'c') {
				delay[sel].C().setExit(ctx == 0);
				rd = delay[sel].C();
			} else if (t == 'n') {
				delay[sel].N().setExit(ctx == 0);
				rd = delay[sel].N();
			} else {
				delay[sel].T().setExit(ctx == 0);
				rd = delay[sel].T();
			}
		} else if (p.equals("lowSecdy")) { // a.k.a. high Primary?
			w = 1;
			c = 1; // A() > B() : psN > ssN
			rd = selection.X();
		} else if (p.equals("equal")) {
			w = 1;
			c = 2;
			rd = selection.X();
		} else if (p.equals("lowPri")) {
			w = 1;
			c = 3;
			rd = selection.X();
		} else if (p.equals("highSeq")) {
			w = 1;
			c = 3;
			rd = sequence.X();
		} else if (p.equals("equalSeq")) {
			w = 1;
			c = 2;
			rd = sequence.X();
		} else if (p.equals("lowSeq")) { // a.k.a. pqN > sqN
			w = 1;
			c = 1; // A() > B() : pqN > sqN
			rd = sequence.X();
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

	private boolean getSwitch(String p, String v) {
		ProgStart ps = null;
		if (p.equals("zone")) {
			ps = zone;
		}
		if (ps != null) {
			ps.set(v.equals("on"));
		}
		return (ps != null);
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
			if (getSwitch(prop, p)) {
				continue;
			}
			String[] vals = p.split("\\s");
			Vector<ProgSet> pv = new Vector<ProgSet>();
			ProgSet p1 = parseItem(prop, 1);
			if (p1 == null) {
				err += String.format("%s = \"%s\"\n", prop, p);
				continue;
			}
			int n = p1.wid;
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

	private JButton makeButton(String lab, String act, Color bg, Color fg, double sc) {
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

	int count = 0;

	private void processRead(ReadingItem ents, byte[] card) {
		if (card == null) {
			return;
		}
		ents.processExits(card);
	}

	private void changeSelectors() {
		for (Selector sel : selector) {
			if (sel != null) {
				sel.change();
			}
		}
		for (Selector sel : delay) {
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
		changeSelectors();
	}

public static int ncards = 0;

	public void reset() {
		errorStop.clear();
		// TODO: allow machine to run...
	}

	public void runout() {
		if (!stopped) {
			return;
		}
		ready.setBackground(off); // should already be off...
		stopped = false;
		runningOut = true;
		// TODO: preserve previous stacker selections?
		out1 = stacker2;
		out2 = stacker2;
		Thread t = new Thread(this);
		t.start();
	}

	public void run() {
		if (errorStop.is()) {
			return;
		}
		ready.setBackground(off);
		_frame.setTitle(title + " (running)");
		boolean cmp = ibm087 && zone.is();
		// Need to start the cycles...
		//allCycles.set(0, true);
		// TODO: supposed to stop when last card fed, not when
		// feeding next card failed...
		while (!stopped) {
			boolean empty = false;
			allCycles.set(0, true);
			allCards.set(0, true);
			if (runningOut || secdyFd.is(0)) {
				if (card2 != null) {
					out2.putCard(card2);
				}
				card2 = new byte[2*80];
				int c = hopper2.getCard(card2);
				if (c < 0) {
					empty = true;
					card2 = null;
				}
			}
			if (runningOut || priFeed.is(0)) {
				if (card1 != null) {
					out1.putCard(card1);
				}
				card1 = card1s;
				card1s = new byte[2*80];
				int c = hopper1.getCard(card1s);
				if (c < 0) {
					empty = true;
					card1s = null;
				}
			}
			// machine stops if either hopper is empty
			// (and not RUNOUT)
			if ((!runningOut && empty) ||
			    (runningOut && card1 == null && card1s == null && card2 == null)) {
				stopped = true;
				break;
			}
			// TODO: need to make progress if there is a card anywhere...
			if (card2 != null) {
				processRead(read2, card2);
			}
			if (card1s != null) {
				processRead(read1s, card1s);
			}
			if (card1 != null) {
++ncards;
				processRead(read1, card1);
//System.err.format("at card %d %s\n", ncards, dumpSelectors());
			}
			if (card1s != null) {
				sequence.processExits(cmp);
			}
			if (card1 != null) {
				if (card2 != null) {
					selection.processExits(cmp);
				}
			}
			if (priSel.is(0)) {
				out1 = stacker1;
			} else {
				out1 = stacker2;
			}
			if (secdySel4.is(0)) {
				out2 = stacker4;
			} else if (secdySel3.is(0)) {
				out2 = stacker3;
			} else {
				out2 = stacker2;
			}
			impulseCycle(allCards);	// End ALL CARDS cycle
			if (errStp.is(0) && !runningOut) {
				errorStop.set(true);
				error.setBackground(red);
				stopped = true;
				break;
			}
			try {
				Thread.sleep(50);
			} catch (Exception ee) {}
		}
		runningOut = false;
		if (card1 == null && card1s == null && card2 == null) {
			ready.setBackground(grn);
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
		java.net.URL url = this.getClass().getResource("docs/About4.html");
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
		Arrays.fill(counter, null);
		Arrays.fill(selector, null);
		sequence.reset();
		selection.reset();
		read1.reset();
		read2.reset();
		read1s.reset();
		// clear exits
		allCards.reset();
		allCycles.reset();
		errStp.reset();
		priFeed.reset();
		priEject.reset();
		secdyFd.reset();
		priSel.reset();
		secdySel3.reset();
		secdySel4.reset();
		zone.reset();
	}
	private void getProg() {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getPanelDir();
		}
		File fi = pickFile("Get Prog", "08x", "IBM 08x Prog", dir, null);
		if (fi == null) {
			return;
		}
		unProg();
		loadProgram(fi.getAbsolutePath());
		if (manager != null) {
			manager.setPanelDir(fi);
		}
	}

	private void deckSave(CardStacker stk) {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		File fi = pickFile("Save " + stk.getLabel(), "pcd", "Punch Card Deck",
					dir, null);
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
		ok = stk.saveDeck(os);
		try {
			os.close();
		} catch (Exception ee) {
			// TODO: pop-up error
		}
		if (ok) {
			if (manager != null) {
				manager.setCardDir(fi);
			}
		}
	}

	private void deckAdd(CardHopper hop) {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		acc_stk.setText(hop.stackList('\n', false));
		acc_cb.setSelected(false);
		File fi = pickFile(hop.getLabel(), "pcd", "Punch Card Deck",
						dir, acc);
		if (fi == null) {
			return;
		}
		deckAdd(hop, fi, acc_cb.isSelected());
		if (manager != null) {
			manager.setCardDir(fi);
		}
	}

	private void deckAdd(CardHopper hop, File fi, boolean empty) {
		if (fi.exists()) {
			try {
				InputStream f = new FileInputStream(fi);
				String n = fi.getName();
				if (n.endsWith(".pcd")) {
					n = n.substring(0, n.length() - 4);
				}
				int c = (int)((fi.length() + 159) / 160);
				hop.addInput(f, n, c, empty);
			} catch (Exception ee) {
				// TODO: PopupFactory
				ee.printStackTrace();
			}
		} else {
			// TODO: PopupFactory
			System.err.format("Internal error: chosen file does not exist\n");
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

	private void deckChange(CardHandler obj, String act) {
		if (act.equals("right")) {
			if (obj instanceof CardHopper) {
				((CardHopper)obj).addBlank(0);
			} else {
				((CardStacker)obj).discardDeck();
			}
		} else if (act.equals("left")) {
			if (obj instanceof CardHopper) {
				deckAdd((CardHopper)obj);
			} else {
				deckSave((CardStacker)obj);
			}
		} else if (act.equals("LEFT")) {
			if (obj instanceof CardStacker) {
				deckView((CardStacker)obj);
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
				// TODO: indicate if no program is loaded...
				stopped = false;
				Thread t = new Thread(this);
				t.start();
			} else if (act.equals("stop")) {
				stopped = true;
			} else if (act.equals("runout")) {
				runout();
			} else if (act.equals("reset")) {
				reset();
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
			stacker1.discardDeck();
		} else if (m.getMnemonic() == KeyEvent.VK_P) {
			getProg();
		} else if (m.getMnemonic() == KeyEvent.VK_U) {
			unProg();
		} else if (m.getMnemonic() == KeyEvent.VK_I) {
			deckAdd(hopper1);
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper1.addBlank(0); // close any input... we hope.
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
