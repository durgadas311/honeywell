// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.util.Arrays;
import java.util.LinkedList;
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
//
// NOTE: H800 documentation describes "transcription mode" as storing
// punch zones 9-4 for columns 1-80 in the first memory block, and
// then punch zones 3-0,X,R for columns 1-80 in the second memory block.
// Unknown if H200 transcription mode follows the same format.
// The zone-bit positions are reversed from that of the PCD file format.

public class P_CardReaderPunch extends JFrame
		implements Peripheral, SequentialRecordIO, ActionListener, WindowListener {
	static final int defBlank = 100;

	Semaphore stall;
	LinkedList<NamedInputStream> hopper;

	class NamedInputStream {
		InputStream real;
		String name;
		int count;
		public NamedInputStream(InputStream in, String nm, int num) {
			real = in;
			name = nm;
			count = num;
		}
	}

	class CardStack extends JPanel implements MouseListener {
		private PunchCardStatus pcs;
		private Color cards;
		private boolean topDown;
		private int scale;
		private ActionListener listener;
		public CardStack(PunchCardStatus pcs, boolean topDown) {
			super();
			this.pcs = pcs;
			this.topDown = topDown;
			setPreferredSize(new Dimension(26, 106));
			setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
			setBackground(Color.white);
			cards = new Color(215,205,154);
			scale = 5;
			listener = null;
			addMouseListener(this);
		}
		public void setListener(ActionListener lstn) {
			listener = lstn;
		}
		@Override
		public void paint(Graphics g) {
			super.paint(g);
			Graphics2D g2d = (Graphics2D)g;
			g2d.translate(3, 3);
			g2d.setColor(cards);
			int n = (pcs.rest + pcs.cards + scale - 1) / scale;
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

		public void mouseClicked(MouseEvent e) {
			if (listener == null) {
				return;
			}
			ActionEvent ae;
			if (e.getButton() == MouseEvent.BUTTON1) {
				ae = new ActionEvent(this, e.getID(), "left");
			} else if (e.getButton() == MouseEvent.BUTTON3) {
				ae = new ActionEvent(this, e.getID(), "right");
			} else {
				return;
			}
			listener.actionPerformed(ae);
		}
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		public void mousePressed(MouseEvent e) {}
		public void mouseReleased(MouseEvent e) {}
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
		int rest = 0;
		byte[] card;
		JLabel count_pn;
		JLabel deck_pn;
		CardStack hopr_pn;
		public PunchCardStatus() {
		}
	}

	PunchCardStatus[] sts;

	SuffFileChooser ch;
	boolean readPunch = false;
	File _last = null;
	boolean isOn = false;
	InputStream idev;
	OutputStream odev;
	File ofil;
	CharConverter cvt;
	int vUnit;
	JPanel acc; // Accessories for file chooser
	JCheckBox acc_cb;
	JTextArea acc_stk;
	LightedButton start;
	LightedButton stop;
	LightedButton runout;
	boolean ready;

	public P_CardReaderPunch(CharConverter cvt) {
		super("H214-2 Card Reader/Punch");
		java.net.URL url = getClass().getResource("icons/pcd-96.png");
		if (url != null) {
			setIconImage(Toolkit.getDefaultToolkit().getImage(url));
		}
		this.cvt = cvt;
		stall = new Semaphore(0);
		// First element is *current* deck...
		hopper = new LinkedList<NamedInputStream>();
		_last = new File(System.getProperty("user.dir"));
		try {
			ofil = File.createTempFile("h214-", ".pcd");
		} catch(Exception ee) {
			System.err.println(ee.getMessage());
			ofil = new File("/tmp/h214.pcd");
		}
		ofil.deleteOnExit();
		JButton bt;
		JPanel pn;
		JLabel lb;
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
		//acc_stk.setFont();
		acc_stk.setEditable(false);
		gb.setConstraints(acc_stk, gc);
		acc.add(acc_stk);
		++gc.gridy;
		gc.anchor = GridBagConstraints.CENTER;
		// Can't seem to get separator to be vertically centered...
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 10));
		gb.setConstraints(pn, gc);
		acc.add(pn);
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
		sts[0].hopr_pn = new CardStack(sts[0], true);
		sts[0].hopr_pn.setListener(this);
		sts[1].count_pn = new JLabel();
		sts[1].count_pn.setPreferredSize(new Dimension(75, 20));
		sts[1].count_pn.setOpaque(true);
		sts[1].count_pn.setBackground(Color.white);
		sts[1].deck_pn = new JLabel();
		sts[1].deck_pn.setPreferredSize(new Dimension(350, 20));
		sts[1].deck_pn.setBackground(Color.white);
		sts[1].deck_pn.setOpaque(true);
		sts[1].hopr_pn = new CardStack(sts[1], false);
		sts[1].hopr_pn.setListener(this);
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		gb = new GridBagLayout();
		setLayout(gb);

		gc.gridwidth = 15;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		int top = gc.gridy;
		gc.gridwidth = 1;
///
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
///
		gc.gridheight = 1;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		gc.gridwidth = 8;
		gb.setConstraints(sts[1].deck_pn, gc);
		add(sts[1].deck_pn);
		gc.gridx += 8;
		gc.gridwidth = 2;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 20));
		gb.setConstraints(pn, gc);
		add(pn);
		gc.gridwidth = 1;
		++gc.gridy;

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
		gc.gridwidth = 7;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		gc.gridx += 7;
		gc.gridwidth = 1;
		lb = new JLabel("Stacker");
		gb.setConstraints(lb, gc);
		add(lb);
		++gc.gridx;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
///
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
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		gc.anchor = GridBagConstraints.SOUTH;
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
		gc.anchor = GridBagConstraints.CENTER;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
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
///
		gc.gridwidth = 3;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		gc.gridx += 3;
		gc.gridwidth = 1;
		gc.anchor = GridBagConstraints.NORTH;
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
		gc.anchor = GridBagConstraints.CENTER;
		gc.gridwidth = 3;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		gc.gridx += 3;
		gc.gridwidth = 1;
///
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

		setBlank(defBlank);
		initStacker();
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
		// TODO: reject if hopper not empty? close it?
		// stack after? stack before?
		if (src == null) src = "Program";
		addDeck(new NamedInputStream(deck, src, count));
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

	// 'idev' is at EOF... regardless of whether 'null'
	private boolean finishDeck() {
		if (idev != null) {
			try {
				idev.close();
			} catch (Exception ee) {}
			idev = null;
		}
		hopper.remove();
		return nextDeck();
	}

	// Consistency:
	//	ASSERT if !hopper.isEmpty() then idev != null
	//	ASSERT if idev == null then hopper.isEmpty()
	//	if idev == null && cards > 0 then BLANK CARDS are loaded
	//	if idev == null && cards == 0 then NO CARDS at all are loaded

	// Hopper contains no cards at all.
	// Cannot be used during transitions (when idev or cards are inconsistent).
	private boolean hopperIsEmpty() {
		return idev == null && sts[1].cards == 0;
	}

	// Hopper contains BLANK cards (only)
	// Cannot be used during transitions (when idev or cards are inconsistent).
	private boolean hopperHasBlank() {
		return  idev == null && sts[1].cards > 0;
	}

	// Hopper contains non-BLANK cards
	private boolean hopperHasDeck() {
		return  idev != null;
	}

	// Caller confirms "idev == null" before calling.
	// Note, this is transition code: idev and cards are not consistent.
	// Upon return, the state should be consistent.
	private boolean nextDeck() {
		if (hopper.isEmpty()) {
			// NOTE: this is not BLANK cards...
			sts[1].deck_pn.setText("");
			sts[1].cards = 0;
			sts[1].rest = 0;
			sts[1].count_pn.setText("0");
			sts[1].hopr_pn.repaint();
			return false;
		}
		NamedInputStream nis = hopper.peek();
		idev = nis.real;
		sts[1].deck_pn.setText(stackList(',', true));
		sts[1].cards = nis.count;
		sts[1].rest = stackCount() - nis.count;
		sts[1].count_pn.setText(String.format("%d", sts[1].rest + sts[1].cards));
		sts[1].hopr_pn.repaint();
		return true;
	}

	private void addDeck(NamedInputStream nis) {
		hopper.add(nis);
		if (!hopperHasDeck()) {
			nextDeck(); // updates display
			return;
		}
		// update display... might be partway through
		// current deck, so must be surgical...
		// leave sts[1].cards alone...
		sts[1].deck_pn.setText(stackList(',', true));
		sts[1].rest = stackCount() - hopper.peek().count;
		sts[1].count_pn.setText(String.format("%d", sts[1].rest + sts[1].cards));
		sts[1].hopr_pn.repaint();
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
					// processor should not be running.
					return false;
				}
			}
			a = -1;
			if (hopperHasBlank()) {
				Arrays.fill(pcs.card, (byte)0);
				a = pcs.card.length;
			} else if (!hopperIsEmpty()) {
				try {
					// only one card read at a time... (?)
					a = idev.read(pcs.card);
				} catch (Exception ee) {
					// TODO: pass along EI/II exceptions
				}
				// Should not normally reach "a < 0", but...
				if (a < 0 && finishDeck()) {
					continue;
				}
			}
			pcs.empty = !(a > 0);
			if (!pcs.empty) {
				break;
			}
			pcs.cards = 0;
			pcs.rest = 0;
			// what status to set?
			pcs.count_pn.setText("0");
			pcs.hopr_pn.repaint();
		}
		--pcs.cards;
		if (pcs.cards <= 0) {
			finishDeck();
		} else {
			pcs.count_pn.setText(String.format("%d", pcs.rest + pcs.cards));
			pcs.hopr_pn.repaint();
		}
		return true;
	}

	private void setReady(boolean rdy) {
		ready = rdy;
		if (sts[0].busy || sts[1].busy) {
			if (ready) {
				unStall(false);
				// fallthrough and update display now
			} else {
				return;
			}
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

	private File pickFile(String purpose, boolean input) {
		File file = null;
		ch = new SuffFileChooser(purpose,
			new String[]{"pcd"}, new String[]{"Punch Card Deck"}, _last,
				input ? acc: null);
		int rv = ch.showDialog(this);
		if (rv != JFileChooser.APPROVE_OPTION) {
			return null;
		}
		file = ch.getSelectedFile();
		_last = file;
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
			sts[1].count_pn.setText(String.format("%d",
						sts[1].rest + sts[1].cards));
			sts[1].hopr_pn.repaint();
		}
		if (sts[1].cards > 0) {
			stall.release();
		}
	}

	// does not update any display elements
	private void clearHopper() {
		hopper.clear();	// TODO: need to close all?
		if (idev != null) {
			try {
				idev.close();
			} catch (Exception ee) {}
			idev = null;
		}
		sts[1].cards = 0;
		sts[1].rest = 0;
	}

	private void setBlank(int num) {
		clearHopper();
		sts[1].deck_pn.setText("BLANK");
		sts[1].cards = num;
		sts[1].count_pn.setText(String.format("%d", sts[1].cards));
		sts[1].hopr_pn.repaint();
	}

	private String stackList(char delim, boolean blanks) {
		if (hopperHasBlank() && blanks) {
			return "BLANK";
		}
		String ret = "";
		for (NamedInputStream nis : hopper) {
			if (!ret.isEmpty()) {
				ret += delim;
			}
			ret += nis.name;
		}
		return ret;
	}

	private int stackCount() {
		int ret = 0;
		for (NamedInputStream nis : hopper) {
			ret += nis.count;
		}
		return ret;
	}

	private void doInputHopperRight() {
		// Set/Add BLANK cards
		if (hopperHasDeck()) {
			clearHopper();
		}
		setBlank(sts[1].cards + defBlank);
	}

	private void doInputHopperLeft() {
		// Add/Replace file (card deck)
		acc_stk.setText(stackList('\n', false));
		acc_cb.setSelected(false);
		File f = pickFile("Input Hopper", true);
		if (f == null) { // cancel
			return;
		}
		InputStream in;
		try {
			in = new FileInputStream(f);
		} catch (Exception ee) {
			PopupFactory.warning(this, "Input Hopper", ee.toString());
			return;
		}
		if (acc_cb.isSelected()) {
			clearHopper();
		}
		int count = (int)((f.length() + 159) / 160);
		String n = f.getName();
		if (n.endsWith(".pcd")) {
			n = n.substring(0, n.length() - 4);
		}
		NamedInputStream nis = new NamedInputStream(in, n, count);
		addDeck(nis);
	}

	private void doOutputStacker(boolean left) {
		String s = "Output Stacker";
		File f = null;
		if (left) {
			f = pickFile("Save", false);
			if (f == null) { // Cancel - no change
				return;
			}
		}
		// Always close current file?
		if (odev != null) {
			try {
				odev.close();
			} catch (Exception ee) {}
			odev = null;
		}
		if (left) {
			boolean ok = false;
			try {
				f.delete();
			} catch (Exception ee) {}
			try {
				ok = ofil.renameTo(f);
				if (!ok) {
					PopupFactory.warning(this, s, "Cannot rename");
				}
			} catch (Exception ee) {
				PopupFactory.warning(this, s, ee.toString());
				ok = false;
			}
			if (!ok) {
				// Restore output deck...
				restoreStacker();
				return;
			}
		}
		initStacker();
	}

	private void restoreStacker() {
		sts[0].cards = (int)((ofil.length() + 159) / 160);
		sts[0].count_pn.setText(String.format("%d", sts[0].cards));
		sts[0].hopr_pn.repaint();
		try {
			odev = new FileOutputStream(ofil, true);
		} catch (Exception ee) {
			PopupFactory.warning(this, "Output Stacker", ee.toString());
			return;
		}
	}

	private void initStacker() {
		sts[0].cards = 0;
		sts[0].count_pn.setText(String.format("%d", sts[0].cards));
		sts[0].hopr_pn.repaint();
		try {
			ofil.delete();
			ofil.createNewFile();
			odev = new FileOutputStream(ofil);
		} catch (Exception ee) {
			PopupFactory.warning(this, "Output Stacker", ee.toString());
			return;
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof CardStack) {
			CardStack cs = (CardStack)e.getSource();
			boolean left = e.getActionCommand().equals("left");
			if (cs == sts[1].hopr_pn) { // reader
				if (left) {
					doInputHopperLeft();
				} else {
					doInputHopperRight();
				}
			} else {
				doOutputStacker(left);
			}
			return;
		}
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
		// ignore unknown buttons
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
		vUnit = unit;	// PCS code, not unit number
		return false; // input != output
	}
	// TODO: clean all this up...
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
		vacateReader();	// just in case - should be no-op
		if (!getCard(sts[1])) {
			return null;	// should only be abort...
		}
		byte[] b = new byte[vUnit == 2 ? 160 : 80];
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
		// make sure we don't read same card twice
		vacateReader();
		return b;
	}
	public boolean appendBulk(byte[] buf, int start, int len) {
		fillPunch();	// may stall if no cards
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
		// make sure we don't punch same card twice
		vacatePunch();
		return true;
	}
	public boolean appendRecord(byte[] buf, int start, int len) {
		return appendBulk(buf, start, len);
	}
	public void end() {
		// No need to truncate output... by definition output is new file.
		// User must save the output...
	}
	public int getError() { return 0; }
}
