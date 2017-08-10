// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;
import java.util.Properties;
import java.util.Vector;

class CardAccounting implements ActionListener, Runnable
{
	static final Color red = new Color(255, 120, 120);
	static final Color off = new Color(190, 190, 180);

	class PrintExit extends ProgStart {
		private PrintControl ctl;
		private ProgStart list = null;

		public PrintExit(PrintControl pc, ProgStart lst) {
			super(true);
			ctl = pc;
			list = lst;
		}

		@Override
		public void set(boolean b) {
			super.set(b);	// triggers watchers... never any?
			if (!b) return;
			if (list != null && !list.is()) {
				return;
			}
			if (ctl.isSuppress()) {
				return;
			}
			// TODO: allow print spacing suppress...
			// perform zero-suppression...
			zeroSuppress(aprint, azsupp);
			zeroSuppress(nprint, nzsupp);
			// TODO: do not print if nothing was printed?
			String s = "\n";
			if (ctl.isDouble()) {
				s += '\n';
			}
			if (ctl.isTriple()) {
				s += '\n';
				s += '\n';
			}
			s += new String(aprint);
			s += ' ';
			s += new String(nprint);
			// TODO: support spacing options
			text.append(s);
			caret += s.length();
			text.setCaretPosition(caret);
			Arrays.fill(aprint, ' ');
			Arrays.fill(nprint, ' ');
		}
	}

	class SpecialPrint extends ProgStart {
		char[] _line;
		int _col;
		char _char;
		public SpecialPrint(char[] line, int col, char ch) {
			super(true);
			_col = col;
			_line = line;
			_char = ch;
		}
		@Override
		public void set(boolean b) {
			super.set(b);	// n/a ?
			if (!b) return;
			// TODO: enforce odd/even columns? Numeric?
			_line[_col] = _char;
		}
	}

	JFrame _frame;
	Font labels;
	File _cwd;
	File _prevProg;
	File _prevDeck;
	File _prevPapr;
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
	Vector<ProgEntry>[] read1;
	Vector<ProgEntry>[] read2;
	Vector<ProgEntry>[] read3;

	// These may have watchers - one-shots
	ProgStart firstMinor;
	ProgStart firstInter;
	ProgStart firstMajor;
	ProgStart minor;
	ProgStart interm;
	ProgStart major;
	ProgStart special;
	ProgStart finalTotal;
	ProgStart allCards;
	ProgStart allCycles;

	// These should only be polled
	ProgStart minorStart;
	ProgStart interStart;
	ProgStart majorStart;
	ProgStart minorFirst;
	ProgStart interFirst;
	ProgStart majorFirst;
	ProgStart specialStart;
	ProgStart cardsStart;
	ProgStart allStart;
	ProgStart finalStart;

	PrintControl prtCtl;

	Comparator comparing;
	char[] aprint;
	char[] nprint;
	boolean[] azsupp;
	boolean[] nzsupp;
	Counter[] counter;
	Selector[] selector;
	int caret;

	public JMenu[] getMenu() { return _menus; }

	public CardAccounting(JFrame frame) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		_frame = frame;
		ibm403 = false;	// TODO: configure
		initReaders();
		aprint = new char[43];
		nprint = new char[45];
		Arrays.fill(aprint, ' ');
		Arrays.fill(nprint, ' ');
		azsupp = new boolean[43];
		nzsupp = new boolean[45];
		counter = new Counter[16];
		selector = new Selector[12];
		comparing = new Comparator(20);
		prtCtl = new PrintControl();

		// TODO: are any of these one-shots?
		minorStart = new ProgStart(false);
		interStart = new ProgStart(false);
		majorStart = new ProgStart(false);
		minorFirst = new ProgStart(false);
		interFirst = new ProgStart(false);
		majorFirst = new ProgStart(false);
		specialStart = new ProgStart(false);
		allStart = new ProgStart(false);
		cardsStart = new ProgStart(false);
		finalStart = new ProgStart(false);

		// These are one-shots, used for watchers only
		firstMinor = new ProgStart(true);
		firstInter = new ProgStart(true);
		firstMajor = new ProgStart(true);
		minor = new ProgStart(true);
		interm = new ProgStart(true);
		major = new ProgStart(true);
		special = new ProgStart(true);
		finalTotal = new ProgStart(true);
		allCards = new ProgStart(true);
		allCycles = new ProgStart(true);

		_cwd = new File(System.getProperty("user.dir"));
		_prevProg = _prevDeck = _prevPapr = _cwd;
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
		stopd = makeButton("STOP", null, off, Color.black);
		fuse = makeButton("FUSE", null, off, Color.black);
		form = makeButton("FORM", null, off, Color.black);
		feed = makeButton("CARD<BR>FEED<BR>STOP", null, off, Color.black);

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
		mi = new JMenuItem("Prog", KeyEvent.VK_P);
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

	protected void zeroSuppress(char[] prt, boolean[] zsp) {
		boolean zsupp = false;
		boolean zlast = false;
		for (int x = 0; x < prt.length; ++x) {
			if (zsp[x]) {
				if (!zlast) zsupp = true;
			} else {
				zlast = false;
				zsupp = false;
			}
			if (zsupp) {
				if (prt[x] == '0') {
					prt[x] = ' ';
				} else {
					zsupp = false;
				}
			}
			zlast = zsp[x];
		}
	}

	private Vector<ProgEntry>[] getReadCycle(char t) {
		switch (t) {
		case '1': return read1;
		case '2': return read2;
		case '3': return read3;
		default: return null;
		}
	}

	// 'idx' is 1-based...
	private int setPrintEntry(char[] xts, int idx, String p) {
		--idx;	// 0-based
		int ret = 0;
		// TODO: enforce Numeric, odd/even column...
		if (p.startsWith("aster=")) {
			getExit(p.substring(6)).addWatcher(new SpecialPrint(xts, idx, '*'));
			return 0;
		} else if (p.startsWith("cr=")) {
			int ctr = getCounter(p.substring(3));
			if (ctr >= 0) {
				counter[ctr].setCredit(new SpecialPrint(xts, idx, '\u00a9'));
			}
		}
		byte c = (byte)-1;
		int n = 1;
		try {
			int ctr = getCounter(p);
			int j = p.indexOf('*');
			if (j > 2) {
				n = Integer.valueOf(p.substring(j + 1));
			} else {
				j = p.length();
			}
			ret = n;
			if (ctr >= 0) {
				// Use output of counter...
				c = (byte)(p.charAt(2) - '0');
				c -= 1;
				while (n > 0) {
					counter[ctr].setEntry(c, new PrintEntry(xts, idx));
					++idx;
					++c;
					--n;
				}
			} else {
				int i = p.indexOf('.');
				if (i < 0) return -1;
				c = Byte.valueOf(p.substring(i + 1, j));
				c -= 1;
				Vector<ProgEntry>[] rd = getReadCycle(p.charAt(0));
				while (n > 0) {
					addEntry(rd, c, new PrintEntry(xts, idx));
					++idx;
					++c;
					--n;
				}
			}
		} catch (Exception ee) {}
		return ret;
	}

	private ProgStart getExit(String pm) {
		if (pm.equals("final")) {
			return finalTotal;
		} else if (pm.equals("major")) {
			return major;
		} else if (pm.equals("inter")) {
			return interm;
		} else if (pm.equals("minor")) {
			return minor;
		} else if (pm.equals("special")) {
			return special;
		} else if (pm.equals("fcma")) {
			return firstMajor;
		} else if (pm.equals("fcin")) {
			return firstInter;
		} else if (pm.equals("fcmi")) {
			return firstMinor;
		// TODO: FC MB?
		} else if (pm.equals("cards")) {
			return allCards;
		} else if (pm.equals("all")) {
			return allCycles;
		}
		return null; // or some dummy ProgStart?
	}

	// 'dig' is 1-based...
	private void setCounterEntry(int ctr, int dig, String p) {
		--dig;	// 0-based
		String[] pp = p.split("\\s");
		int cc = getCounter(pp[0]);
		if (cc < 0 && !pp[0].matches("[123]\\.[0-9]+.*")) {
			return;	// TODO: error
		}
		for (int x = 1; x < pp.length; ++x) {
			if (pp[x].startsWith("plus=")) {
				counter[ctr].setPlus(getStart(pp[x].substring(5)));
			} else if (pp[x].startsWith("minus=")) {
				counter[ctr].setMinus(getStart(pp[x].substring(6)));
			} else if (pp[x].startsWith("carry=")) {
				int ct = getCounter(pp[x].substring(6));
				if (ct >= 0) {
					counter[ctr].setCarry(counter[ct]);
				}
			} else if (pp[x].startsWith("total=")) {
				String pm = pp[x].substring(6);
				ProgStart pms = getExit(pm);
				pms.addWatcher(counter[ctr]);
				// Also cause print... might be suppressed separately
				pms.addWatcher(new PrintExit(prtCtl, null));
			}
			// TODO: other params...
		}
		byte c = (byte)-1;
		int n = 1;
		try {
			int j = pp[0].indexOf('*');
			if (j > 2) {
				n = Integer.valueOf(pp[0].substring(j + 1));
			} else {
				j = pp[0].length();
			}
			if (cc >= 0) {
				// Use output of counter...
				c = (byte)(pp[0].charAt(2) - '0');
				c -= 1;
				while (n > 0) {
					counter[cc].setEntry(c,
						new CounterEntry(counter[ctr], dig));
					++dig;
					++c;
					--n;
				}
			} else {
				int i = pp[0].indexOf('.');
				if (i < 0) return;
				c = Byte.valueOf(pp[0].substring(i + 1, j));
				c -= 1;
				Vector<ProgEntry>[] rd = getReadCycle(pp[0].charAt(0));
				while (n > 0) {
					addEntry(rd, c, new CounterEntry(counter[ctr], dig));
					++dig;
					++c;
					--n;
				}
			}
		} catch (Exception ee) {}
	}

	private int getCounter(String p) {
		if (!p.matches("[2468][abcd].*")) {
			return -1;
		}
		int w = p.charAt(0) - '0';
		int ctr = (w - 2) << 1;		// 0, 4, 8, 12
		ctr |= (p.charAt(1) - 'a');	// 0..15
		if (counter[ctr] == null) {
			counter[ctr] = new Counter(w);
		}
		return ctr;
	}

	private ProgStart getStart(String p) {
		if (p.equals("fcmi")) {
			return minorFirst;
		} else if (p.equals("fcin")) {
			return interFirst;
		} else if (p.equals("fcma")) {
			return majorFirst;
		} else if (p.equals("minor")) {
			return minorStart;
		} else if (p.equals("inter")) {
			return interStart;
		} else if (p.equals("major")) {
			return majorStart;
		} else if (p.equals("special")) {
			return specialStart;
		} else if (p.equals("final")) {
			return finalStart;
		} else if (p.equals("cards")) {
			return cardsStart;
		} else if (p.equals("all")) {
			return allStart;
		}
		return null;
	}

	private void setComparing(Comparator cmp, int pos, String p) {
		String[] pp = p.split("\\s");
		if (!pp[0].matches("[123]\\.[0-9]+.*") ||
			!pp[1].matches("[123]\\.[0-9]+.*")) {
			return;
		}
		ProgStart start = null;
		for (int x = 2; x < pp.length; ++x) {
			if (pp[x].startsWith("start=")) {
				// TODO: only major,inter,minor[,special]...
				start = getStart(pp[x].substring(6));
			}
		}
		int na = 1;
		int nb = 1;
		try {
			Vector<ProgEntry>[] rda = getReadCycle(pp[0].charAt(0));
			Vector<ProgEntry>[] rdb = getReadCycle(pp[1].charAt(0));
			int j = pp[0].indexOf('*');
			if (j > 2) na = Integer.valueOf(pp[0].substring(j + 1));
			else j = pp[0].length();
			byte ca = Byte.valueOf(pp[0].substring(2, j));
			j = pp[0].indexOf('*');
			if (j > 2) nb = Integer.valueOf(pp[0].substring(j + 1));
			else j = pp[0].length();
			byte cb = Byte.valueOf(pp[1].substring(2, j));
			if (na < nb) na = nb; // error, probably, if even !=
			--ca;
			--cb;
			if (start != null) {
				// user error if not set...
				cmp.setExit(pos, na, start);
			}
			while (na > 0) {
				ComparingEntry ea = new ComparingEntry(pos);
				ComparingEntry eb = new ComparingEntry(pos);
				addEntry(rda, ca, ea);
				addEntry(rdb, cb, eb);
				cmp.setEntryA(pos, ea);
				cmp.setEntryB(pos, eb);
				++pos;
				++ca;
				++cb;
				--na;
			}
		} catch (Exception ee) {}
	}

	private void loadProgram(String prog) {
		Arrays.fill(read1, null);
		Arrays.fill(read2, null);
		Arrays.fill(read3, null);
		Arrays.fill(counter, null);
		Arrays.fill(azsupp, false);
		Arrays.fill(nzsupp, false);
		firstMinor.reset();
		firstInter.reset();
		firstMajor.reset();
		minor.reset();
		interm.reset();
		major.reset();
		special.reset();
		finalTotal.reset();
		allCards.reset();
		allCycles.reset();
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
			int ctr;
			if (prop.matches("[an][0-9]+")) {
				// Alphameric/Numeric Print Entry
				int col = Integer.valueOf(prop.substring(1));
				char[] prt = (prop.charAt(0) == 'n' ? nprint : aprint);
				boolean[] zsp = (prop.charAt(0) == 'n' ? nzsupp : azsupp);
				String[] pa = p.split("\\s");
				int wid = 0;
				for (String pp : pa) {
					if (pp.equals("zero")) {
						// 'col' is still 1-based
						// TODO: error if wid == 0...
						for (int x = 0; x < wid - 1; ++x) {
							zsp[x + col - 1] = true;
						}
					} else {
						int w = setPrintEntry(prt, col, pp);
						if (w > wid) wid = w;
					}
				}
			} else if ((ctr = getCounter(prop)) >= 0) {
				// Counters - all entries
				int dig = (prop.charAt(2) - '0');
				setCounterEntry(ctr, dig, p);
			} else if (prop.matches("c[0-9]+")) {
				int pos = Integer.valueOf(prop.substring(1));
				setComparing(comparing, pos, p);
			} else if (prop.equals("list")) {
				ProgStart ps = getStart(p);
				if (ps != null) {
					allCards.addWatcher(new PrintExit(prtCtl, ps));
				}
			} else if (prop.matches("s[0-3]")) {
				ProgStart ps = getStart(p);
				if (ps != null) switch (prop.charAt(1)) {
					case '0':
						prtCtl.addSuppress(ps);
						break;
					case '1':
						prtCtl.addSingle(ps);
						break;
					case '2':
						prtCtl.addDouble(ps);
						break;
					case '3':
						prtCtl.addTriple(ps);
						break;
				}
			}
		}
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
		p |= (card[ix * 2 + 1] & 0x0ff) << 8;
		return p;
	}

	private void processRead(Vector<ProgEntry>[] ents, byte[] card) {
		if (card == null) {
			return;
		}
		for (int c = 0; c < 80; ++c) {
			if (ents[c] == null) continue;
			int p = getCol(card, c);
			String t = cvt.punToAscii(p);
			if (t == null) continue;
			char h = t.charAt(0);
			for (ProgEntry ent : ents[c]) {
				ent.putCol(h);
			}
		}
	}

	private void addEntry(Vector<ProgEntry>[] ents, int pos, ProgEntry ent) {
		if (ents[pos] == null) {
			ents[pos] = new Vector<ProgEntry>();
		}
		ents[pos].add(ent);
	}

	private void processFinalTotal() {
		allStart.set(true);
		finalStart.set(true);
		impulse(minor);
		impulse(interm);
		impulse(major);
		impulse(finalTotal);
		impulse(allCycles);
		finalStart.set(false);
		allStart.set(false);
		stopd.setBackground(off);
	}

	private void resetStart() {
		minorStart.set(false);
		interStart.set(false);
		majorStart.set(false);
		specialStart.set(false);
	}

	private void resetFirsts() {
		minorFirst.set(false);
		interFirst.set(false);
		majorFirst.set(false);
	}

	private void impulse(ProgStart xt) {
		xt.set(true);	// should all be one-shots...
	}

	public void run() {
		idle.setBackground(off);
		feed.setBackground(off); // OFF by what?
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
			allStart.set(true);
			cardsStart.set(true);
			processRead(read3, card3);
			// TODO: do this in minor/inter/major?
			impulse(allCards);
			impulse(allCycles);
			cardsStart.set(false);	// End ALL CARDS cycle
			// On run-out, card2 might be null...
			if (card2 != null) {
				// TODO: ordering, placement... dependencies?
				if (majorFirst.is()) {
					impulse(firstMajor);
					impulse(allCycles);
				}
				if (majorFirst.is() || interFirst.is()) {
					impulse(firstInter);
					impulse(allCycles);
				}
				if (majorFirst.is() || interFirst.is() || minorFirst.is()) {
					impulse(firstMinor);
					impulse(allCycles);
				}
				resetFirsts();
				comparing.processExits();
				if (minorStart.is() || interStart.is() || majorStart.is()) {
					impulse(minor);
					impulse(allCycles);
				}
				if (interStart.is() || majorStart.is()) {
					impulse(interm);
					impulse(allCycles);
				}
				if (majorStart.is()) {
					impulse(major);
					impulse(allCycles);
				}
				// Is next card a first-in-group?
				minorFirst.set(minorStart.is());
				interFirst.set(interStart.is());
				majorFirst.set(majorStart.is());
			}
			allStart.set(false);	// or, always 'true'?
			// if programmed stop... {
			//	stopd.setBackground(red);
			//	stopped = true;
			// }
			try {
				Thread.sleep(50);
			} catch (Exception ee) {}
		}
		idle.setBackground(red);
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

	private void getProg() {
		File fi = pickFile("Get Prog", "40x", "IBM 40x Prog", _prevProg, null);
		if (fi == null) {
			return;
		}
		_prevProg = fi;
		loadProgram(fi.getAbsolutePath());
	}

	private void deckAdd() {
		acc_stk.setText(hopper.stackList('\n', false));
		acc_cb.setSelected(false);
		File fi = pickFile("Add Input", "pcd", "Punch Card Deck", _prevDeck, acc);
		if (fi == null) {
			return;
		}
		_prevDeck = fi;
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
				// TODO: indicate if no program is loaded...
				stopped = false;
				Thread t = new Thread(this);
				t.start();
			} else if (act.equals("stop")) {
				stopped = true;
			} else if (act.equals("total")) {
				stopd.setBackground(off);
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
		} else if (m.getMnemonic() == KeyEvent.VK_I) {
			deckAdd();
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			hopper.addBlank(0); // close any input... we hope.
			// stacker.discardDeck(); // file should get removed
			System.exit(0);
		} else if (m.getMnemonic() == KeyEvent.VK_S) {
			File f = pickFile("Save Report",
					"txt", "Text Files", _prevPapr, null);
			if (f != null) try {
				FileOutputStream fo = new FileOutputStream(f);
				fo.write(text.getText(0, caret).getBytes());
				fo.close();
				_prevPapr = f;
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

	@SuppressWarnings("unchecked")
	private void initReaders() {
		read1 = new Vector[80];
		read2 = new Vector[80];
		read3 = new Vector[80];
	}
}
