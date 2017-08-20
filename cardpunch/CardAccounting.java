// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;
import java.util.Properties;
import java.util.Vector;

class CardAccounting implements Machine, Puncher, ActionListener, Runnable
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

	class PrintItem extends ProgItem implements TypeBars {
		char[] print;
		boolean[] zsupp;
		boolean needed;

		public PrintItem(int w) {
			super(w);
			exit = false;
			print = new char[w];
			zsupp = new boolean[w];
		}

		public void print(int p, char c) {
			print[p] = c;
			needed = true;
		}

		// Reset for new program panel
		public void reset() {
			super.reset();
			Arrays.fill(zsupp, false);
			Arrays.fill(print, ' ');
			needed = false;
		}

		@Override
		public ProgStart get(int p) {
			if (ents[p] == null) {
				ents[p] = new PrintEntry(this, p);
			}
			return ents[p];
		}

		public void setZSupp(int p) {
			zsupp[p] = true;
		}

		public boolean printNeeded() { return needed; }

		// This generates printer output, and resets for next line
		public String zeroSuppress() {
			boolean zsup = false;
			boolean zlast = false;
			for (int x = 0; x < print.length; ++x) {
				if (zsupp[x]) {
					if (!zlast) zsup = true;
				} else {
					zlast = false;
					zsup = false;
				}
				if (zsup) {
					if (print[x] == '0') {
						print[x] = ' ';
					} else if (print[x] != ' ') {
						zsup = false;
					}
				}
				zlast = zsupp[x];
			}
			String ret = new String(print);
			Arrays.fill(print, ' ');
			needed = false;
			return ret;
		}
	}

	class PrintExit extends ProgStart {
		private PrintControl ctl;

		public PrintExit(PrintControl pc) {
			super(true);
			ctl = pc;
		}

		@Override
		public void set(boolean b) {
			super.set(b);	// triggers watchers... never any?
			if (b) return;	// trigger on falling edge?
//			if (!aprint.printNeeded() && !nprint.printNeeded()) {
//				return;
//			}
			if (ctl.SS().is(0)) {
				return;
			}
			// TODO: allow print spacing suppress...
			// TODO: do not print if nothing was printed?
			String s;
			if (ctl.S1().is(0)) {
				s = "\n";
			} else if (ctl.S2().is(0)) {
				s = "\n\n";
			} else if (ctl.S3().is(0)) {
				s = "\n\n\n";
			} else if (firstMinor.is(0)) {
				s = "\n\n";
			} else {
				s = "\n";
			}
			s += aprint.zeroSuppress();
			s += ' ';
			s += nprint.zeroSuppress();
			text.append(s);
			caret += s.length();
			text.setCaretPosition(caret);
		}
	}

	class XSelector extends ProgStart {
		Selector i_pu;
		DelayStart delay;

		public XSelector(Selector I_PU) {
			super(true);
			i_pu = I_PU;
			delay = new DelayStart();
			delay.addWatcher(i_pu);
			allCards.get(0).addWatcher(delay);
		}

		@Override
		public void putCol(int p, char c) {
			super.trigger(p, c);	// n/a ?
			// Any punch in X/11 or 12
			delay.setFlag((p & 0x0c00) != 0);
		}
	}

	class DSelector extends ProgStart {
		Selector i_pu;
		DelayStart delay;

		public DSelector(Selector I_PU) {
			super(true);
			i_pu = I_PU;
			delay = new DelayStart();
			delay.addWatcher(i_pu);
			allCards.get(0).addWatcher(delay);
		}

		@Override
		public void putCol(int p, char c) {
			super.trigger(p, c);	// n/a ?
			// Any punch in 9-0,X/11, or 12.
			// Requires digit selectors to be more specific.
			delay.set(p != 0);
		}
	}

	// Summary Punch must trigger on trailing edge.
	class SummaryEntry extends ProgStart {
		public SummaryEntry() {
			super(true);
		}
		@Override
		public void set(boolean b) {
			super.set(b); // never watchers?
			if (b) return;
			if (summary != null) {
				// TODO: too much thrash?
				feed.setBackground(red);
				if (summary.startPunch()) {
					errorStop.set(true);
					stopped = true;
				} else {
					feed.setBackground(off);
				}
			}
		}
	}
	class SummaryItem extends SingleEntry {
		public SummaryItem() {
			super(new SummaryEntry());
		}
		@Override
		public ProgStart get(int x) {
			if (ents[x] == null) {
				ents[x] = new SummaryEntry();
			}
			return ents[x];
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
	boolean progSet;
	ErrorStop errorStop;
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
	ReadingItem read1;
	ReadingItem read2;
	ReadingItem read3;

	// These are exits, may have watchers - one-shots

	SingleExit firstMinor;
	SingleExit firstInter;
	SingleExit firstMajor;
	SingleExit firstBody;
	SingleExit minor;
	SingleExit inter;
	SingleExit major;
	SingleExit finalTotal;
	SingleExit allCards;
	SingleExit allCycles;
	SingleExit asterMajor;
	SingleExit asterInter;
	SingleExit asterMinor;
	SingleExit asterFinal;
	SingleExit asterAll;
	ColumnSplit csplits;

	// These should only be polled, internally
	// (no watchers added)
	SingleEntry minorStart;
	SingleEntry interStart;
	SingleEntry majorStart;
	SingleEntry listStart;
	LatchingEntry printed;
	SummaryItem summPU;
	ProgItem summPC;

	// Special entry
	PrintControl prtCtl;
	// Printing entry
	PrintItem aprint;
	PrintItem nprint;

	Comparator comparing;
	Counter[] counter;
	Selector[] selector;
	int caret;

	public JMenu[] getMenu() { return _menus; }
	public JFrame getFrame() { return _frame; }
	public void setQuitListener(ActionListener lstn) { quit = lstn; }
	private ActionListener quit = null;
	String title;
	ReproducingPunch summary;

	AppManager manager;

	// TODO: 'summ' is ReproducingPunch a.k.a. Summary Punch
	public CardAccounting(JFrame frame, AppManager mgr, ReproducingPunch summ) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		manager = mgr;
		_frame = frame;
		summary = summ;
		title = _frame.getTitle();
		ibm403 = false;	// TODO: configure
		errorStop = new ErrorStop(off);
		progSet = false;
		read1 = new ReadingItem(80);
		read2 = new ReadingItem(80);
		read3 = new ReadingItem(80);
		aprint = new PrintItem(43);
		nprint = new PrintItem(45);
		counter = new Counter[16];
		selector = new Selector[12];
		prtCtl = new PrintControl();
		summPC = new ProgItem(12);
		csplits = new ColumnSplit(4, true);

		comparing = new Comparator(20);

		// These are one-shots, used for watchers only
		firstMinor = new SingleExit();
		firstInter = new SingleExit();
		firstMajor = new SingleExit();
		firstBody = new SingleExit();
		minor = new SingleExit();
		inter = new SingleExit();
		major = new SingleExit();
		finalTotal = new SingleExit();
		allCards = new SingleExit();
		allCycles = new SingleExit();
		// TODO: use asterisk punch code, or simply X...
		asterMajor = new SingleExit(new SpecialPrint(0x422, '*'));
		asterInter = new SingleExit(new SpecialPrint(0x422, '*'));
		asterMinor = new SingleExit(new SpecialPrint(0x422, '*'));
		asterFinal = new SingleExit(new SpecialPrint(0x422, '*'));
		asterAll = new SingleExit(new SpecialPrint(0x422, '*'));

		// TODO: are any of these one-shots?
		minorStart = new SingleEntry(minor.get(0));
		interStart = new SingleEntry(inter.get(0));
		majorStart = new SingleEntry(major.get(0));
		listStart = new SingleEntry(new PrintExit(prtCtl));
		printed = new LatchingEntry();
		summPU = new SummaryItem();

		_cwd = new File(System.getProperty("user.dir"));
		cvt = new CharConverter();

		stopped = true;
		hopper = new CardHopper("Input Hopper", 45, 100, 1, false);
		hopper.setListener(this);
		deckUpdate(hopper);
		stacker = new CardStacker("Stacker", 45, 100, 1, true);
		stacker.setListener(this);
		deckUpdate(stacker);
		start = makeButton("START", "start", Color.black, Color.white);
		stop = makeButton("STOP", "stop", Color.black, Color.white);
		total = makeButton("FINAL<BR>TOTAL", "total", Color.black, Color.white);
		// TODO: make indicators...
		idle = makeButton("", null, off, Color.black);
		idle.setFocusable(false);
		stopd = makeButton("STOP", null, off, Color.black);
		stopd.setFocusable(false);
		fuse = makeButton("FUSE", null, off, Color.black);
		fuse.setFocusable(false);
		form = makeButton("FORM", null, off, Color.black);
		form.setFocusable(false);
		feed = makeButton("CARD<BR>FEED<BR>STOP", null, off, Color.black);
		feed.setFocusable(false);
		errorStop.addLight(stopd);
		errorStop.addLight(fuse);
		errorStop.addLight(form);
		errorStop.addLight(feed);

		text = new JTextArea(20, 89);
		text.setEditable(false);
		text.setBackground(Color.white);
		text.setFont(new Font("Monospaced", Font.PLAIN, 10));
		text.setFocusable(false);
		scroll = new JScrollPane(text);
		scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scroll.setViewportBorder(new LineBorder(Color.white, 3));
		caret = 0;

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

		java.net.URL url = this.getClass().getResource("docs/Accounting.html");
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
		pn.setPreferredSize(new Dimension(10, 5));
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
		pn.setPreferredSize(new Dimension(40, 5));
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

		gc.gridwidth = 16;
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
		pn.setPreferredSize(new Dimension(200, 5));
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

		idle.setBackground(red);
	}

	private ProgItem getReadCycle(char t) {
		switch (t) {
		case '1': return read1;
		case '2': return read2;
		case '3': return read3;
		default: return null;
		}
	}

	private ProgItem parseEntry(String pm) {
		if (pm.equals("list")) {
			return printed;
		} else if (pm.equals("stmi")) {
			return minorStart;
		} else if (pm.equals("stin")) {
			return interStart;
		} else if (pm.equals("stma")) {
			return majorStart;
		} else if (pm.equals("spsupp")) {
			return prtCtl.SS();
		} else if (pm.equals("space1")) {
			return prtCtl.S1();
		} else if (pm.equals("space2")) {
			return prtCtl.S2();
		} else if (pm.equals("space3")) {
			return prtCtl.S3();
		} else if (pm.equals("sppu")) {
			return summPU;
		}
		return null;
	}

	private ProgItem parseExit(String pm) {
		if (pm.equals("final")) {
			return finalTotal;
		} else if (pm.equals("major")) {
			return major;
		} else if (pm.equals("inter")) {
			return inter;
		} else if (pm.equals("minor")) {
			return minor;
		} else if (pm.equals("fcma")) {
			return firstMajor;
		} else if (pm.equals("fcin")) {
			return firstInter;
		} else if (pm.equals("fcmi")) {
			return firstMinor;
		} else if (pm.equals("fcmb")) {
			return firstBody;
		} else if (pm.equals("cards")) {
			return allCards;
		} else if (pm.equals("all")) {
			return allCycles;
		} else if (pm.equals("aster3")) {
			return asterMajor;
		} else if (pm.equals("aster2")) {
			return asterInter;
		} else if (pm.equals("aster1")) {
			return asterMinor;
		} else if (pm.equals("asterf")) {
			return asterFinal;
		} else if (pm.equals("asterall")) {
			return asterAll;
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
		_getCounter(ctr);
		return ctr;
	}

	private void _getCounter(int ctr) {
		if (counter[ctr] == null) {
			int w = ((ctr & 0x1c) >> 1) + 2;
			counter[ctr] = new Counter(w);
			counter[ctr].TOTAL().get(0).addWatcher(printed.get(0));
		}
	}

	// Puncher interface
	public ProgItem counterExit(int ctr) {
		_getCounter(ctr);
		return counter[ctr].X();
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

	// Char Sources:
	//	FIRST/SECOND/THIRD READING
	//	CREDIT SYMBOL (per counter)
	//	(asterisk) SYMBOL (per group F,1,2,3)
	//	COUNTER EXIT (per counter, digit)
	//	SELECTOR C,N,T
	// Char Targets:
	//	ALPHAMERICAL PRINT
	//	NUMERICAL PRINT
	//	COUNTER ENTRY (per counter, digit)
	//	COMPARING ENTRY (per position, 2 each)
	//	SELECTOR X-PU, D-PU
	//	SELECTOR C,N,T
	// Impulse Sources:
	//	TOTAL PROGRAM (MINOR, INTER, MAJOR, ALL)
	//	CARD CYCLES
	//	FIRST CARD (MI, IN, MA, M?)
	//	FINAL TOTAL
	//	SELECTOR C,N,T (I-PU*)
	// Impulse Targets:
	//	START (MI, IN, MA)
	//	SPACE CTL (S,1,2,3)
	//	LIST
	//	SELECTOR I-PU
	//	SELECTOR C,N,T
	// * Not supported
	// Syntax:
	//	[123]\.[0-9]+		1st/2nd/3rd Reading column N
	//	\*[f123]		Asterisk final/major/intermediate/minor
	//	a[0-9]+			Alphamerical print column N
	//	n[0-9]+			Numerical print column N
	//	c[0-9]+			Comparing position N
	//	[2468][abcd][1-n]	Counter X digit N (n: 2,4,6,8)
	//	[2468][abcd]		Counter X carry exit or total entry
	//	[2468][abcd]cr		Counter X CR symbol exit
	//	[2468][abcd][pmt]	Counter X plus/minus/total control
	//	s[0-9]+[xdi]		Selector N entry X-PU, D-PU, I-PU
	//	s[0-9]+[cnt][0-9]+	Selector N contact C/N/T position M
	//	cards			Card cycles impulse
	//	final			Final total impulse
	//	major			Major group program total impulse
	//	inter			Intermediate group program total impulse
	//	minor			Minor group program total impulse
	//	all			All cycles impulse
	//	fcma/fcin/fcmi		Major/Intermediate/Minor First Card impulse
	//	list			List (print) control
	//	sps/sp1/sp2/sp3		Space control
	//	stma/stin/stmi		Major/Intermediate/Minor Start control
	//
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
			} else if (t == 'x') { // S.P. X CTRL PLUS/MINUS
				t = p.charAt(3);
				rd = counter[ctr].SPX();
				if (p.length() >= 4 && p.charAt(3) == '-') {
					c = 2;
				}
			} else if (t == 'r') { // TRANSFER
				rd = counter[ctr].TRANSFER();
				if (p.length() >= 4 && p.charAt(3) == '-') {
					c = 2;
				}
			}
		} else if (p.matches("[123]\\.[0-9]+")) {
			// 1st/2nd/3rd READING EXITs
			c = Integer.valueOf(p.substring(2));
			rd = getReadCycle(p.charAt(0));
		} else if (p.matches("c[0-9]+[xab]")) {
			// Comparing entry/exit
			char t = p.charAt(p.length() - 1);
			c = Integer.valueOf(p.substring(1, p.length() - 1));
			if (t == 'x') {
				rd = comparing.X();
			} else if (t == 'a') {
				rd = comparing.A();
			} else {
				rd = comparing.B();
			}
		} else if (p.matches("[an][0-9]+")) {
			// ALPHA/NUMERICAL PRINTING ENTRY
			c = Integer.valueOf(p.substring(1));
			rd = (p.charAt(0) == 'n' ? nprint : aprint);
		} else if (p.matches("s[0-9]+[xdi]")) {
			// SELECTOR X/D/I PU
			w = 1;
			char t = p.charAt(p.length() - 1);
			int sel = getSelector(p.substring(1, p.length() - 1));
			if (t == 'x') {
				rd = new SingleEntry(new XSelector(selector[sel]));
			} else if (t == 'd') {
				rd = new SingleEntry(new DSelector(selector[sel]));
			} else {
				rd = new SingleEntry(selector[sel]);
			}
		} else if (p.matches("s[0-9]+[cnt][0-9]+")) {
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
		} else if (p.matches("cs[0-9]+[xrdc]")) {
			// COLUMN SPLITS: R (12), X (11), 0-9 (d), and COM (c) hubs
			char t = p.charAt(p.length() - 1);
			c = Integer.valueOf(p.substring(2, p.length() - 1));
			if (t == 'r') {
				rd = csplits.R();
			} else if (t == 'x') {
				rd = csplits.X();
			} else if (t == 'd') {
				rd = csplits.D();
			} else {
				rd = csplits.C();
			}
		} else if (p.matches("sp[0-9]+")) {
			// S.P. CONTROL ENTRY
			w = 1; // TODO: only single width?
			c = Integer.valueOf(p.substring(2));
			if (summary != null) {
				rd = summary.summaryEntry();
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

	private void startSummaryPunch() {
		if (summary != null) {
			summary.connect(this);
			_frame.toFront(); // re-assert ourself
		}
	}

	private void stopSummaryPunch() {
		if (summary != null) {
			summary.connect(null);
		}
	}

	private void loadProgram(String prog) {
		major.linkEntry(0, 0, asterMajor.get(0));
		inter.linkEntry(0, 0, asterInter.get(0));
		minor.linkEntry(0, 0, asterMinor.get(0));
		finalTotal.linkEntry(0, 0, asterFinal.get(0));
		allCycles.linkEntry(0, 0, asterAll.get(0));
		String err = "";
		//
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
			if (prop.equals("sp") && p.equals("on")) {
				startSummaryPunch();
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
			boolean zero = false;
			for (String val : vals) {
				if (val.equals("zero")) {
					zero = true;
					continue;
				}
				ProgSet p2 = parseItem(val, 0);
				if (p2 == null) {
					err += String.format("%s = \"%s\"\n", prop, p);
					continue;
				}
				if (p2.wid > n) n = p2.wid;
				pv.add(p2);
			}
			// This is a royal hack...
			PrintItem z1 = null;
			if (zero && p1.pit instanceof PrintItem) {
				z1 = (PrintItem)p1.pit;
			}
			for (ProgSet p2 : pv) {
				int c1 = p1.col;
				int c2 = p2.col;
				int id = c1;
				for (int x = 0; x < n; ++x) {
					if (z1 != null && x + 1 < n) {
						z1.setZSupp(c1);
					}
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

	int count = 0;

	private void processRead(ReadingItem ents, byte[] card) {
		if (card == null) {
			return;
		}
		ents.processExits(card);
	}

	private void processFinalTotal() {
		finalTotal.set(0, true);
		// by definition, all these are true...
		minor.set(0, true);
		inter.set(0, true);
		major.set(0, true);
		impulseCycle(minor);
		impulseCycle(inter);
		impulseCycle(major);
		impulseCycle(finalTotal);
		stopd.setBackground(off);
	}

	private void resetStart() {
		minorStart.set(0, false);
		interStart.set(0, false);
		majorStart.set(0, false);
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
		if (printed.is(0)) {
			listStart.set(0, true);
			listStart.set(0, false);
			printed.set(0, false); // or at top of loop?
		}
		allCycles.set(0, false);
		changeSelectors();
	}

public static int ncards = 0;

	public void run() {
		if (errorStop.is()) {
			return;
		}
		_frame.setTitle(title + " (running)");
		idle.setBackground(off);
		// TODO: allow re-starting with cards still in place?
		byte[] card1 = null;
		byte[] card2 = null;
		byte[] card3 = null;
		// Any card is the first, when starting fresh...
		// TODO: same re-start issues...
		firstMinor.set(0, true);
		firstInter.set(0, true);
		firstMajor.set(0, true);
		while (!stopped) {
			if (card3 != null) {
				stacker.putCard(card3);
			}
			card3 = card2;
			card2 = card1;
			card1 = new byte[2*80];
			int c = hopper.getCard(card1);
			if (c < 0) {
				card1 = null;
				if (card2 == null && card3 == null) {
					stopped = true;
					break;
				}
			}
			resetStart();	// TODO: do this at bottom of last loop?
			// TODO: other resets... other cycle indicators?
			// TODO: any indicators for run-in?
			if (ibm403) {
				processRead(read1, card1);
			}
			processRead(read2, card2);
			// short-circuit run-in...
			if (card3 == null) {
				continue;
			}
			allCycles.set(0, true);
			allCards.set(0, true);
++ncards;
			processRead(read3, card3);
			csplits.commit(); // needed elsewhere? all cycles?
//System.err.format("at card %d %s\n", ncards, dumpSelectors());
			impulseCycle(allCards);	// End ALL CARDS cycle
			// On run-out, card2 might be null...
			if (card2 != null) {
				// TODO: ordering, placement... dependencies?
				boolean mi = firstMinor.is(0);
				boolean in = firstInter.is(0);
				boolean ma = firstMajor.is(0);
				// TODO: proper overlap of signals...
				if (ma) {
					impulseCycle(firstMajor);
				}
				if (ma || in) {
					impulseCycle(firstInter);
				}
				if (ma || in || mi) {
					impulseCycle(firstMinor);
				}
				//resetFirsts(); // already done
				// Sets major/inter/minor (possibly)
				comparing.processExits();
				mi = minor.is(0);
				in = inter.is(0);
				ma = major.is(0);
				if (mi || in || ma) {
					impulseCycle(minor);
				}
				if (in || ma) {
					impulseCycle(inter);
				}
				if (ma) {
					impulseCycle(major);
				}
				// Is next card a first-in-group?
				firstMinor.set(0, mi);
				firstInter.set(0, in);
				firstMajor.set(0, ma);
				// TODO: firstBody
			}
			// if programmed stop... {
			//	stopd.setBackground(red);
			//	errorStop.set(true);
			//	stopped = true;
			// }
			try {
				Thread.sleep(50);
			} catch (Exception ee) {}
		}
		idle.setBackground(red);
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
		java.net.URL url = this.getClass().getResource("docs/About3.html");
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

	private void unProg() {
		progSet = false;
		Arrays.fill(counter, null);
		Arrays.fill(selector, null);
		comparing.reset();
		read1.reset();
		read2.reset();
		read3.reset();
		aprint.reset();
		nprint.reset();
		minorStart.reset();
		interStart.reset();
		majorStart.reset();
		listStart.reset();
		printed.reset();
		prtCtl.reset();
		// clear exits
		firstMinor.reset();
		firstInter.reset();
		firstMajor.reset();
		firstBody.reset();
		minor.reset();
		inter.reset();
		major.reset();
		finalTotal.reset();
		allCards.reset();
		allCycles.reset();
		stopSummaryPunch();
	}

	private void getProg() {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getPanelDir();
		}
		File fi = pickFile("Get Prog", "40x", "IBM 40x Prog", dir, null);
		if (fi == null) {
			return;
		}
		unProg();
		loadProgram(fi.getAbsolutePath());
		if (manager != null) {
			manager.setPanelDir(fi);
		}
	}

	private void deckAdd() {
		File dir = _cwd;
		if (manager != null) {
			dir = manager.getCardDir();
		}
		acc_stk.setText(hopper.stackList('\n', false));
		acc_cb.setSelected(false);
		File fi = pickFile("Add Input", "pcd", "Punch Card Deck", dir, acc);
		if (fi == null) {
			return;
		}
		dir = fi;
		deckAdd(fi, acc_cb.isSelected());
		if (manager != null) {
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
				// TODO: indicate if no program is loaded...
				stopped = false;
				Thread t = new Thread(this);
				t.start();
			} else if (act.equals("stop")) {
				stopped = true;
			} else if (act.equals("total")) {
				errorStop.clear(); // yes?
				processFinalTotal();
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
		} else if (m.getMnemonic() == KeyEvent.VK_P) {
			getProg();
		} else if (m.getMnemonic() == KeyEvent.VK_U) {
			unProg();
		} else if (m.getMnemonic() == KeyEvent.VK_I) {
			deckAdd();
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper.addBlank(0); // close any input... we hope.
			// stacker.discardDeck(); // file should get removed
			if (quit != null) {
				quit.actionPerformed(new ActionEvent(this, e.getID(), "quit"));
			} else {
				System.exit(0);
			}
		} else if (m.getMnemonic() == KeyEvent.VK_S) {
			File dir = _cwd;
			if (manager != null) {
				dir = manager.getPaperDir();
			}
			File f = pickFile("Save Report",
					"txt", "Text Files", dir, null);
			if (f != null) try {
				FileOutputStream fo = new FileOutputStream(f);
				fo.write(text.getText(0, caret).getBytes());
				fo.close();
				if (manager != null) {
					manager.setPaperDir(f);
				}
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
