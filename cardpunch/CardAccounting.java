// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;
import java.util.Properties;

class CardAccounting implements ActionListener, Runnable
{
	static final Color red = new Color(255, 120, 120);
	static final Color off = new Color(190, 190, 180);

	class PrintExit extends ProgExit {
		private ProgStart list = null;
		public PrintExit() {}
		public PrintExit(ProgStart ps) {
			list = ps;
		}

		public void processExits() {
			// process other exits first, in case they print
			if (_next != null) {
				_next.processExits();
			}
			if (list != null && !list.is()) {
				return;
			}
			if (supSpace.is()) {
				return;
			}
			// TODO: allow print spacing suppress...
			// perform zero-suppression...
			zeroSuppress(aprint, azsupp);
			zeroSuppress(nprint, nzsupp);
			// TODO: do not print if nothing was printed?
			String s = new String(aprint);
			s += ' ';
			s += new String(nprint);
			s += '\n';
			if (dblSpace.is()) {
				s += '\n';
			}
			if (trpSpace.is()) {
				s += '\n';
				s += '\n';
			}
			// TODO: support spacing options
			text.append(s);
			caret += s.length();
			text.setCaretPosition(caret);
			Arrays.fill(aprint, ' ');
			Arrays.fill(nprint, ' ');
		}
	}

	class AsterExit extends ProgExit {
		char[] _line;
		int _col;
		public AsterExit(char[] line, int col) {
			super();
			_col = col;
			_line = line;
		}
		public void processExits() {
			_line[_col] = '*';
			if (_next != null) {
				_next.processExits();
			}
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
	ProgEntry[] read1;
	ProgEntry[] read2;
	ProgEntry[] read3;
	ProgExit minor;
	ProgExit interm;
	ProgExit major;
	ProgExit special;
	ProgExit finalTotal;
	ProgExit allCards;

	ProgStart firstMinor;
	ProgStart firstInter;
	ProgStart firstMajor;
	ProgStart minorStart;
	ProgStart interStart;
	ProgStart majorStart;
	ProgStart specialStart;
	ProgStart allStart;
	ProgStart finalStart;
	ProgStart listStart;
	ProgStart supSpace;
	ProgStart dblSpace;
	ProgStart trpSpace;

	Comparator comparing;
	char[] aprint;
	char[] nprint;
	boolean[] azsupp;
	boolean[] nzsupp;
	Counter[] counter;
	int caret;

	public JMenu[] getMenu() { return _menus; }

	public CardAccounting(JFrame frame) {
		labels = new Font("Sans-Serif", Font.PLAIN, 10);
		_frame = frame;
		ibm403 = false;	// TODO: configure

		read1 = new ProgEntry[80];
		read2 = new ProgEntry[80];
		read3 = new ProgEntry[80];
		aprint = new char[43];
		nprint = new char[45];
		Arrays.fill(aprint, ' ');
		Arrays.fill(nprint, ' ');
		azsupp = new boolean[43];
		nzsupp = new boolean[45];
		counter = new Counter[16];
		comparing = new Comparator(20);
		minorStart = new ProgStart();
		interStart = new ProgStart();
		majorStart = new ProgStart();
		specialStart = new ProgStart();
		allStart = new ProgStart();
		finalStart = new ProgStart();
		listStart = new ProgStart();
		firstMinor = new ProgStart();
		firstInter = new ProgStart();
		firstMajor = new ProgStart();
		supSpace = new ProgStart();
		dblSpace = new ProgStart();
		trpSpace = new ProgStart();

		_cwd = new File(System.getProperty("user.dir"));
		cvt = new CharConverter();

		stopped = true;
		hopper = new CardHopper("Input Hopper", 60, 100, 1, false);
		hopper.setListener(this);
		deckUpdate(hopper);
		stacker = new CardStacker("Stacker", 60, 100, 1, true);
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

	private ProgEntry[] getReadCycle(char t) {
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
		if (p.startsWith("aster=")) {
			ProgExit xt = new AsterExit(xts, idx);
			setExit(p.substring(6), xt, xt);
			return 0;
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
				ProgEntry[] rd = getReadCycle(p.charAt(0));
				while (n > 0) {
					rd[c] = new PrintEntry(xts, idx, rd[c]);
					++idx;
					++c;
					--n;
				}
			}
		} catch (Exception ee) {}
		return ret;
	}

	private void setExit(String pm, ProgExit xt, ProgExit ct) {
		if (pm.equals("final")) {
			xt.setNext(finalTotal);
			finalTotal = ct;
		} else if (pm.equals("major")) {
			xt.setNext(major);
			major = ct;
		} else if (pm.equals("inter")) {
			xt.setNext(interm);
			interm = ct;
		} else if (pm.equals("minor")) {
			xt.setNext(minor);
			minor = ct;
		} else if (pm.equals("special")) {
			xt.setNext(special);
			special = ct;
		} else if (pm.equals("all")) {
			xt.setNext(allCards);
			allCards = ct;
		}
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
				// Also cause print...
				ProgExit xt = new PrintExit();
				counter[ctr].setNext(xt);
				// TODO: reject duplicates...
				setExit(pm, xt, counter[ctr]);
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
				ProgEntry[] rd = getReadCycle(pp[0].charAt(0));
				while (n > 0) {
					rd[c] = new CounterEntry(counter[ctr], dig, rd[c]);
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
		if (p.equals("fcminor")) {
			return firstMinor;
		} else if (p.equals("fcinter")) {
			return firstInter;
		} else if (p.equals("fcmajor")) {
			return firstMajor;
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
				start = getStart(pp[x].substring(6));
			}
		}
		int na = 1;
		int nb = 1;
		try {
			ProgEntry[] rda = getReadCycle(pp[0].charAt(0));
			ProgEntry[] rdb = getReadCycle(pp[1].charAt(0));
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
				ComparingEntry ea = new ComparingEntry(pos, rda[ca]);
				ComparingEntry eb = new ComparingEntry(pos, rdb[cb]);
				rda[ca] = ea;
				rdb[cb] = eb;
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
		minor = null;
		interm = null;
		major = null;
		special = null;
		finalTotal = null;
		allCards = null;
		props = new Properties();
		try {
			InputStream is = new FileInputStream(prog);
			props.load(is);
		} catch (Exception ee) {
			return;
		}
		// TODO: aN=3.x requires zN=2.x, produce erroneous output if not wired.
		// TODO: printing requires "all" -> "list"
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
					ProgExit xt = new PrintExit(ps);
					xt.setNext(allCards);
					allCards = xt;
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

	private void processRead(ProgEntry[] ents, byte[] card) {
		if (card == null) {
			return;
		}
		for (int c = 0; c < 80; ++c) {
			if (ents[c] == null) continue;
			int p = getCol(card, c);
			String t = cvt.punToAscii(p);
			if (t == null) continue;
			ents[c].putCol(t.charAt(0));
		}
	}

	private void processFinalTotal() {
		finalStart.set(true);
		if (minor != null) {
			minor.processExits();
		}
		if (interm != null) {
			interm.processExits();
		}
		if (major != null) {
			major.processExits();
		}
		if (finalTotal != null) {
			finalTotal.processExits();
		}
		finalStart.set(false);
		// TODO: allow processing?
		stopd.setBackground(off);
	}

	private void resetStart() {
		minorStart.set(false);
		interStart.set(false);
		majorStart.set(false);
		specialStart.set(false);
	}

	private void resetFirsts() {
		firstMinor.set(false);
		firstInter.set(false);
		firstMajor.set(false);
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
			resetStart();
			// TODO: other resets
			if (ibm403) {
				processRead(read1, card1);
			}
			processRead(read2, card2);
			allStart.set(true);
			processRead(read3, card3);
			// TODO: other cycles (minor, inter, major)...
			// TODO: only print if something printed...
			if (allCards != null && card3 != null) {
				allCards.processExits();
			}
			// If card3 != null then check for print
			allStart.set(false);
			if (card2 != null && card3 != null) {
				comparing.processExits();
				if (minorStart.is() || interStart.is() || majorStart.is()) {
					if (minor != null) {
						minor.processExits();
					}
				}
				if (interStart.is() || majorStart.is()) {
					if (interm != null) {
						interm.processExits();
					}
				}
				if (majorStart.is()) {
					if (major != null) {
						major.processExits();
					}
				}
				// Is next card a first?
				firstMinor.set(minorStart.is());
				firstInter.set(interStart.is());
				firstMajor.set(majorStart.is());
			}
			// if programmed stop... {
			//	stopd.setBackground(red);
			//	stopped = true;
			// }
			try {
				Thread.sleep(10);
			} catch (Exception ee) {}
		}
		idle.setBackground(red);
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
		File fi = pickFile("Get Prog", false, "40x", "IBM 40x Prog", _cwd);
		if (fi == null) {
			return;
		}
		loadProgram(fi.getAbsolutePath());
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
