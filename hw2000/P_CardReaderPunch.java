// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.util.Arrays;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.border.*;
import java.util.concurrent.Semaphore;

// True single-path Reader-Punch:
//
// INPUT HOPPER -> [WAIT] -> READ STATION -> PUNCH STATION -> OUTPUT STACKER
//
// For punching "new" cards, INPUT HOPPER must be set to "BLANK" (i.e. no file).
// For reading cards only, OUTPUT STACKER should be set to "DISCARD" (i.e. no file).
//
// The 214-2 "Reader/Punch" is not a dual-path device. I.e. it cannot be used to
// make a copy of a card deck in a single pass. To do this one must:
//	1. Load cards to copy into INPUT HOPPER
//	2. Read cards (optionally modify data) and write to tape (or other media)
//	3. Load BLANK cards into INPUT HOPPER
//	4. Read tape (optionally modify data) and punch new cards
//
// It does, however, permit punching additional data on existing cards.

public class P_CardReaderPunch extends JFrame
		implements Peripheral, SequentialRecordIO, ActionListener, WindowListener {
	Semaphore stall;

	class CardStack extends JPanel {
		private PunchCardStatus pcs;
		private Color cards;
		private boolean topDown;
		private int scale;
		public CardStack(PunchCardStatus pcs, boolean topDown) {
			super();
			this.pcs = pcs;
			this.topDown = topDown;
			setPreferredSize(new Dimension(26, 106));
			setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
			setBackground(Color.white);
			cards = new Color(215,215,154);
			scale = 5;
		}
		@Override
		public void paint(Graphics g) {
			super.paint(g);
			Graphics2D g2d = (Graphics2D)g;
			g2d.translate(3, 3);
			g2d.setColor(cards);
			int n = (pcs.cards + scale - 1) / scale;
			boolean max = false;
			if (n > 100) {
				max = true;
				n = 100;
			}
			if (n > 0) {
				if (topDown) {
					g2d.fillRect(0, 0, 21, n + 1);
				} else {
					g2d.fillRect(0, 100 - n, 21, n + 1);
				}
				if (max) {
					g2d.setColor(Color.red);
					g2d.drawLine(0, 0, 20, 0);
				}
			}
		}
	}

	private class PunchCardStatus {
		boolean busy = false;
		boolean empty = true;
		boolean error = false;
		boolean illegal = false;
		boolean busy_if_ill = false;
		boolean busy_if_err = false;
		boolean offset_ill = false;
		boolean offset_err = false;
		boolean offset_next = false;
		boolean interrupt = false;
		int code = 0;
		int cards = 0;
		byte[] card;
		JLabel count_pn;
		JLabel deck_pn;
		CardStack hopr_pn;
		public PunchCardStatus() {
		}
	}

	PunchCardStatus[] sts;

	boolean readPunch = false;
	File _last = null;
	boolean isOn = false;
	InputStream idev;
	OutputStream odev;
	CharConverter cvt;
	int vUnit;
	JPanel acc; // Accessories for file chooser
	JTextField acc_nb;
	LightedButton start;
	LightedButton stop;
	LightedButton runout;
	boolean ready;

	public P_CardReaderPunch(CharConverter cvt) {
		super("H214-2 Card Reader/Punch");
		setIconImage(Toolkit.getDefaultToolkit().
			getImage(getClass().getResource("icons/pcd-96.png")));
		this.cvt = cvt;
		stall = new Semaphore(0);
		_last = new File(System.getProperty("user.dir"));
		GridBagLayout gb;
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
		acc = new JPanel();
		gb = new GridBagLayout();
		acc.setLayout(gb);
		JLabel lb = new JLabel("Num Blank");
		gb.setConstraints(lb, gc);
		acc.add(lb);
		++gc.gridy;
		lb = new JLabel("(use Cancel)");
		gb.setConstraints(lb, gc);
		acc.add(lb);
		++gc.gridy;
		acc_nb = new JTextField();
		acc_nb.setPreferredSize(new Dimension(40, 20));
		gb.setConstraints(acc_nb, gc);
		acc.add(acc_nb);
		gc.gridy = 0;

		sts = new PunchCardStatus[2];
		sts[0] = new PunchCardStatus(); // output - punch
		sts[0].card = new byte[160]; // 80 columns, 16 bits each
		sts[1] = new PunchCardStatus(); // input - reader
		sts[1].card = new byte[160]; // 80 columns, 16 bits each
		sts[0].count_pn = new JLabel();
		sts[0].count_pn.setPreferredSize(new Dimension(75, 20));
		sts[0].count_pn.setOpaque(true);
		sts[0].count_pn.setBackground(Color.white);
		sts[0].deck_pn = new JLabel();
		sts[0].deck_pn.setPreferredSize(new Dimension(300, 20));
		sts[0].deck_pn.setBackground(Color.white);
		sts[0].deck_pn.setOpaque(true);
		sts[0].deck_pn.setText("Discard");
		sts[0].hopr_pn = new CardStack(sts[0], true);
		sts[1].count_pn = new JLabel();
		sts[1].count_pn.setPreferredSize(new Dimension(75, 20));
		sts[1].count_pn.setOpaque(true);
		sts[1].count_pn.setBackground(Color.white);
		sts[1].deck_pn = new JLabel();
		sts[1].deck_pn.setPreferredSize(new Dimension(300, 20));
		sts[1].deck_pn.setBackground(Color.white);
		sts[1].deck_pn.setOpaque(true);
		sts[1].hopr_pn = new CardStack(sts[1], false);
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		gb = new GridBagLayout();
		setLayout(gb);

		JPanel pn;
		gc.gridwidth = 15;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		int top = gc.gridy;
		gc.gridwidth = 1;
		gc.gridheight = 4;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 20));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		gb.setConstraints(sts[1].hopr_pn, gc);
		add(sts[1].hopr_pn);
		++gc.gridx;
		int left = gc.gridx;
		gc.gridheight = 1;
		gc.gridwidth = 11;

		pn = new JPanel();
		pn.setLayout(new FlowLayout());
		JButton bt = new JButton("Input Hopper");
		bt.setPreferredSize(new Dimension(125, 20));
		bt.setMargin(new Insets(5, 5, 5, 5));
		bt.setActionCommand("reader");
		bt.addActionListener(this);
		JPanel pn2 = new JPanel();
		pn2.setPreferredSize(new Dimension(150, 20));
		pn2.setOpaque(false);
		pn.add(bt);
		pn.add(sts[1].deck_pn);
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		gc.gridwidth = 1;

		gc.gridx = left;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		lb = new JLabel("Hopper");
		gb.setConstraints(lb, gc);
		add(lb);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		lb = new JLabel("START");
		gb.setConstraints(lb, gc);
		add(lb);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(15, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		lb = new JLabel("STOP");
		gb.setConstraints(lb, gc);
		add(lb);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		lb = new JLabel("RUNOUT");
		gb.setConstraints(lb, gc);
		add(lb);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		lb = new JLabel("Stacker");
		gb.setConstraints(lb, gc);
		add(lb);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		//////////////////////////
		gc.gridx = left;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		gb.setConstraints(sts[1].count_pn, gc);
		add(sts[1].count_pn);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		start = new LightedButton(Peripheral.btnWhiteOn,
					Peripheral.btnWhiteOff);
		start.setActionCommand("start");
		start.addActionListener(this);
		gb.setConstraints(start, gc);
		add(start);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		stop = new LightedButton(Peripheral.btnWhiteOn,
					Peripheral.btnWhiteOff);
		stop.setActionCommand("stop");
		stop.addActionListener(this);
		gb.setConstraints(stop, gc);
		add(stop);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		runout = new LightedButton(Peripheral.btnWhiteOn,
					Peripheral.btnWhiteOff);
		runout.setActionCommand("runout");
		runout.addActionListener(this);
		gb.setConstraints(runout, gc);
		add(runout);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		gb.setConstraints(sts[0].count_pn, gc);
		add(sts[0].count_pn);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		gc.gridx = left;

		gc.gridwidth = 11;
		pn = new JPanel();
		pn.setLayout(new FlowLayout());
		bt = new JButton("Output Stacker");
		bt.setMargin(new Insets(5, 5, 5, 5));
		bt.setPreferredSize(new Dimension(125, 20));
		bt.setActionCommand("punch");
		bt.addActionListener(this);
		pn.add(bt);
		pn.add(sts[0].deck_pn);
		gb.setConstraints(pn, gc);
		add(pn);
		gc.gridx += 11;

		gc.gridwidth = 1;
		gc.gridy = top;
		gc.gridheight = 4;
		gb.setConstraints(sts[0].hopr_pn, gc);
		add(sts[0].hopr_pn);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 20));
		gb.setConstraints(pn, gc);
		add(pn);
		gc.gridx = 0;
		gc.gridy += 4;
		gc.gridwidth = 15;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);

		addWindowListener(this);
		pack();

		setBlank(100);
		setReady(true);
	}

	public void reset() {
		unStall(true);
	}

	public void setInterrupt(HW2000 sys) {
	}

	private void autoVisible(boolean on) {
		if (on != isOn) {
			isOn = on;
			setVisible(on);
		}
	}

	public void visible(boolean on) {
		autoVisible(on);
		if (on) {
			toFront();
		}
	}

	public void addInput(InputStream deck, String src, int count) {
		// TODO: reject if idev != null? close it? stack after?
		idev = deck;
		sts[1].deck_pn.setText(src == null ? "Program" : src);
		sts[1].cards = count > 0 ? count : 0;
		sts[1].count_pn.setText(String.format("%d", sts[1].cards));
		sts[1].hopr_pn.repaint();
		unStall(false);
	}

	private int getCol(PunchCardStatus pcs, int ix) {
		int p = pcs.card[ix * 2] & 0x0ff;
		p |= (pcs.card[ix * 2 + 1] & 0x0ff) << 8;
		return p;
	}

	private void putCol(PunchCardStatus pcs, int ix, int p) {
		pcs.card[ix * 2] = (byte)(p & 0x0ff);
		pcs.card[ix * 2 + 1] = (byte)((p >> 8) & 0x0ff);
	}

	public void io(RWChannel rwc) {
		// can't abort input if idev == null: means blank cards.
		// need to stall anyway.
		if (rwc.isInput()) {
			sts[1].busy = true;
		} else {
			sts[0].busy = true;
		}
	}

	public void run(RWChannel rwc) {
		if (rwc.isInput()) {
			if (!sts[1].busy) {
				return;
			}
			doIn(rwc, sts[1]);
			sts[1].busy = false;
		} else {
			if (!sts[0].busy) {
				return;
			}
			doOut(rwc, sts[0]);
			sts[0].busy = false;
		}
		if (!ready) {
			stop.setOn(!ready);
			start.setOn(ready);
		}
	}

	private void putCard(PunchCardStatus pcs) {
		if (odev != null) {
			try {
				odev.write(pcs.card);
			} catch (Exception ee) {
				// TODO: handle exceptions? pass along?
			}
		}
		pcs.empty = true;
		++pcs.cards;
		pcs.count_pn.setText(String.format("%d", pcs.cards));
		pcs.hopr_pn.repaint();
	}

	private void passCard() {
		System.arraycopy(sts[1].card, 0, sts[0].card, 0, sts[1].card.length);
		sts[1].empty = true;
		sts[0].empty = false;
	}

	private boolean getCard(PunchCardStatus pcs) {
		// 'pcs' must be sts[1]...
		int a;
		while (true) {
			if (pcs.cards == 0 || !ready) {
				// Not an error, just stall...
				doStall();
				if (pcs.cards == 0) { // implied abort
					// assumed to be INITIALIZE - abort I/O
					//pcs.error = true; // is this an error?
					return false;
				}
			}
			a = -1;
			if (idev != null) {
				try {
					// only one card read at a time... (?)
					a = idev.read(pcs.card);
				} catch (Exception ee) {
					// TODO: pass along EI/II exceptions
				}
			} else if (pcs.cards > 0) {
				// assume a *finite* stack of BLANK cards...
				Arrays.fill(pcs.card, (byte)0);
				a = pcs.card.length;
			}
			pcs.empty = !(a > 0);
			if (!pcs.empty) {
				break;
			}
			pcs.cards = 0;
			// what status to set?
			pcs.count_pn.setText("0");
			pcs.hopr_pn.repaint();
		}
		if (pcs.cards > 0) {
			--pcs.cards;
			pcs.count_pn.setText(String.format("%d", pcs.cards));
			pcs.hopr_pn.repaint();
		} else {
			pcs.count_pn.setText("?");
		}
		return true;
	}

	private void setReady(boolean rdy) {
		ready = rdy;
		if (sts[0].busy || sts[1].busy) {
			if (ready) {
				unStall(false);
			}
			return;
		}
		stop.setOn(!ready);
		start.setOn(ready);
	}

	private void runout() {
		if (ready) {
			return;
		}
		while (!sts[0].empty) {
			putCard(sts[0]);
			if (!sts[1].empty) {
				passCard();
			}
		}
	}

	private void vacatePunch() {
		if (sts[0].empty) {
			return;
		}
		// eject card from punch station...
		putCard(sts[0]);
	}

	private void vacateReader() {
		if (sts[1].empty) {
			return;
		}
		// must pass along any current card...
		vacatePunch();
		// move card from read to punch...
		passCard();
	}

	private void fillPunch() {
		if (!sts[0].empty) {
			return;
		}
		// need a card to punch...
		if (sts[1].empty) {
			// load card into read station...
			getCard(sts[1]);
		}
		// move card from read to punch...
		passCard();
	}

	private void doIn(RWChannel rwc, PunchCardStatus pcs) {
		rwc.startCLC();
		// 'pcs' must be sts[1]...
		vacateReader();	// just in case - should be no-op
		if (!getCard(pcs)) {
			return;
		}
		byte m;
		for (int x = 0; x < 80; ++x) {
			boolean stop = false;
			// Must not disturb punctuation...
			int p = getCol(pcs, x);
			if (pcs.code == 2) {
				// TODO: what is proper order...
				m = rwc.writeChar((byte)((p >> 6) & 077));
				// this would probably be an error...
				stop = ((m & 0300) == 0300);
				if (rwc.incrCLC() || stop) {
					break;
				}
				m = rwc.writeChar((byte)p);
				stop = ((m & 0300) == 0300);
			} else {
				int c = cvt.punToHW(p, (pcs.code == 1));
				if (c < 0) {
					pcs.illegal = true;
					c = 0; // TODO: error code?
				}
				m = rwc.writeChar((byte)c);
				stop = ((m & 0300) == 0300);
			}
			if (rwc.incrCLC() || stop) {
				break;
			}
		}
		// make sure we don't read same card twice
		vacateReader();
	}

	public void doOut(RWChannel rwc, PunchCardStatus pcs) {
		rwc.startCLC();
		fillPunch();
		for (int x = 0; x < 80; ++x) {
			int p = 0;
			if (pcs.code == 2) {
				p = (rwc.readChar() & 077) << 6;
				if (rwc.incrCLC()) {
					return;
				}
				p |= (rwc.readChar() & 077);
			} else {
				byte a = rwc.readChar();
				p = cvt.hwToPun(a, (pcs.code == 1));
			}
			putCol(pcs, x, p);
			if (rwc.incrCLC()) {
				return;
			}
		}
		// make sure we don't punch same card twice
		vacatePunch();
	}

	public boolean busy(byte c2) {
		int io = ((c2 & 040) >> 5);
		return sts[io].busy;
	}

	public boolean ctl(RWChannel rwc) {
		// Assume this device cannot read/punch at the same time
		// C3-Cn:
		//	10	Branch if busy
		//	41	Branch if punch check error
		//	42	Branch if illegal punch
		//	70	Control allow OFF
		//	71	Control allow ON
		//	74	Control interrupt OFF
		//	75	Branch if interrupt ON
		// "Branch to A if device unavailable"...
		// TODO: what is "unavailable"?
		//	27	Hollerith code *
		//	26	Special code
		//	25	direct-transcription code
		//	24	Generate busy if illegal punch
		//	23	Generate busy if punch check errors
		//	22	Offset illegal punch cards
		//	21	Offset punch error cards
		//	20	Punch feed read mode (a punch may follow read)
		//	31	Offset current punch card
		boolean branch = false;
		boolean in = rwc.isInput();
		PunchCardStatus pcs;
		if (in) {
			pcs = sts[1];
		} else {
			pcs = sts[0];
		}
		byte[] cx = new byte[]{ rwc.c3, rwc.c4, rwc.c5, rwc.c6, rwc.c7 };
		for (int x = 0; x < rwc.cn - 2; ++x) {
			// some are mode changes, stored
			// Allow multiple conditions?
			switch(cx[x]) {
			case 010:
				// What does "busy" mean in this context?
				if (pcs.busy) {
					branch = true;
				}
				break;
			case 041:
				// never errors?
				if (pcs.error) {
					branch = true;
					pcs.error = false;
				}
				break;
			case 042:
				// TODO: what constitues illegal punch?
				if (pcs.illegal) {
					branch = true;
					pcs.illegal = false;
				}
				break;
			case 027:
				readPunch = false;
				// FALLTHROUGH
			case 026:
			case 025:
				pcs.code = (3 - (cx[x] & 003)); // 0=Hollerith, 1=Spcl, 2=Dir
				break;
			case 024:
				// TODO: what resets this?
				pcs.busy_if_ill = true;
				break;
			case 023:
				// TODO: what resets this?
				pcs.busy_if_err = true;
				break;
			case 022:
				// TODO: what resets this?
				pcs.offset_ill = true;
				break;
			case 021:
				// TODO: what resets this?
				pcs.offset_err = true;
				break;
			case 020:
				// TODO: enforce punch-feed read mode?
				// We operate in this mode automatically now,
				// If a punch follows a read, the card is re-punched.
				// If a read follows a read, send card to stacker.
				// See fillPunch(), vacateReader(), vacatePunch().
				readPunch = true;
				break;
			case 031:
				pcs.offset_next = true;
				break;
			case 074:
				pcs.interrupt = false;
				break;
			case 075:
				if (pcs.interrupt) {
					branch = true;
				}
				break;
			}
		}
		return branch;
	}

	private File pickFile(String purpose) {
		File file = null;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			new String[]{"pcd"}, new String[]{"Punch Card Deck"}, _last,
			purpose.startsWith("Input") ? acc : null);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
			_last = file; // or use dev[unit]?
		}
		return file;
	}

	private void doStall() {
		ready = false;
		stop.setOn(!ready);
		start.setOn(ready);
		try {
			stall.drainPermits();
			stall.acquire();
		} catch (Exception ee) {}
	}

	private void unStall(boolean abort) {
		// TODO: implement abort option
		// TODO: prevent count > 0 ?
		if (abort) { // cleanup display
			sts[1].count_pn.setText(String.format("%d", sts[1].cards));
			sts[1].hopr_pn.repaint();
		}
		if (sts[1].cards > 0) {
			stall.release();
		}
	}

	private void setBlank(int num) {
		sts[1].deck_pn.setText("Blank");
		sts[1].cards = num;
		sts[1].count_pn.setText(String.format("%d", sts[1].cards));
		sts[1].hopr_pn.repaint();
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JButton)) {
			return;
		}
		JButton b = (JButton)e.getSource();
		String c = b.getActionCommand();
		if (c.equals("runout")) {
			runout();
			return;
		} else if (c.equals("start")) {
			setReady(true);
			return;
		} else if (c.equals("stop")) {
			setReady(false);
			return;
		}
		String s = "";
		if (c.equals("reader")) {
			s = "Input Hopper";
			if (idev != null) {
				try {
					idev.close();
				} catch (Exception ee) {}
				idev = null;
			}
			acc_nb.setText("100");
			setBlank(100);
		} else if (c.equals("punch")) {
			s = "Output Stacker";
			if (odev != null) {
				try {
					odev.close();
				} catch (Exception ee) {}
				odev = null;
			}
			sts[0].deck_pn.setText("Discard");
			sts[0].cards = 0;
			sts[0].count_pn.setText(String.format("%d", sts[0].cards));
			sts[0].hopr_pn.repaint();
		}
		File f = pickFile(s);
		if (f == null) {
			if (c.equals("reader")) {
				int num = 100;
				if (!acc_nb.getText().isEmpty()) try {
					num = Integer.valueOf(acc_nb.getText());
				} catch (Exception ee) {}
				setBlank(num);
				// do not unStall() until operator presses START
			}
			return;
		}
		if (c.equals("reader")) {
			try {
				idev = new FileInputStream(f);
			} catch (Exception ee) {
				PopupFactory.warning(this, s, ee.toString());
				return;
			}
			sts[1].deck_pn.setText(f.getName());
			sts[1].cards = (int)((f.length() + 159) / 160);
			sts[1].count_pn.setText(String.format("%d", sts[1].cards));
			sts[1].hopr_pn.repaint();
			// do not unStall() until operator presses START
		} else if (c.equals("punch")) {
			try {
				odev = new FileOutputStream(f);
			} catch (Exception ee) {
				PopupFactory.warning(this, s, ee.toString());
				return;
			}
			sts[0].deck_pn.setText(f.getName());
			sts[0].count_pn.setText("0");
			sts[0].hopr_pn.repaint();
		}
	}

	public void windowActivated(WindowEvent e) { }
	public void windowClosed(WindowEvent e) { }
	public void windowIconified(WindowEvent e) { }
	public void windowOpened(WindowEvent e) { }
	public void windowDeiconified(WindowEvent e) { }
	public void windowDeactivated(WindowEvent e) { }
	public void windowClosing(WindowEvent e) {
		isOn = false;
		setVisible(false);
	}

	public boolean begin(int unit) {
		// use unit for PCS code... TBD...
		vUnit = unit;
		return false; // input != output
	}
	public boolean ready() {
		// Don't know if input or output,
		// so only report the obvious case.
		return (odev != null || idev != null);
	}
	public boolean empty() {
		return (idev == null);
	}
	public boolean rewind() {
		// Rewind not possible
		return true;
	}
	public boolean backspace() {
		// Backspace not possible
		return true;
	}
	public byte[] nextRecord() {
		// TODO: is it safe to stall here?
		if (!getCard(sts[1])) {
			return new byte[0]; // like MagTape EOF mark...
		}
		byte[] b = new byte[vUnit == 2 ? 160 : 80];
		// TODO: conversion modes...
		for (int x = 0; x < 80; ++x) {
			int p = getCol(sts[1], x);
			if (vUnit == 2) { // raw mode...
				b[x * 2] = (byte)((p >> 6) & 077);
				b[x * 2 + 1] = (byte)p;
			} else {
				int c = cvt.punToHW(p, (vUnit == 1));
				if (c < 0) {
					c = 0; // TODO: error code?
				}
				b[x] = (byte)c;
			}
		}
		return b;
	}
	public boolean appendBulk(byte[] buf, int start, int len) {
		// TODO: fillPunch() (and stall) here?
		for (int x = 0; x < 80; ++x) {
			int p = 0;
			if (vUnit == 2) {
				if (x * 2 + 1 < buf.length) {
					p = (buf[x * 2] & 077) << 6;
					p |= (buf[x * 2 + 1] & 077);
				}
			} else if (x < buf.length) {
				p = cvt.hwToPun(buf[x], (vUnit == 1));
			}
			putCol(sts[0], x, p);
		}
		putCard(sts[0]);
		return true;
	}
	public boolean appendRecord(byte[] buf, int start, int len) {
		return appendBulk(buf, start, len);
	}
	public void end() {
		// No need to truncate output... by definition output is new file.
	}
	public int getError() { return 0; }
}
