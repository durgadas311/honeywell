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
// punch zones 9-4 for columns 1-80 in the first 10-word (80-char) memory block,
// and then punch zones 3-0,X,R for columns 1-80 in the second memory block.
// The zone-bit positions are reversed from that of the PCD file format.
//
// Manual for H214-1 Punch describes option 064 (direct transcription mode) as
// using pairs of adjacent characters in memory for each column, with the first
// character containing 9-4 and the second 3-0,X,R. This is still bit-reversed
// from the PCD file format.
//
// It appears that the H8200 follows the H200 memory pattern for direct transcription,
// with the high 6 bits of the word being the first character of the 8-char block.
// It also appears as though the H8200 uses only Series 200 peripherals, with the
// exception of the 8200-1 console. In general, the H8200 uses H200-style peripheral
// instructions, adapted to Word mode (i.e. same control chars).

public class P_CardReaderPunch extends JFrame
		implements Peripheral, SequentialRecordIO,
			ActionListener, WindowListener {
	static final byte[] rev4 = new byte[]{
		0b0000, 0b1000, 0b0100, 0b1100, 0b0010, 0b1010, 0b0110, 0b1110,
		0b0001, 0b1001, 0b0101, 0b1101, 0b0011, 0b1011, 0b0111, 0b1111,
	};

	Semaphore stall;
	CardHopper hopper;
	CardStacker stacker;
	CardViewer viewer;

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
		byte[] card;
		JLabel count_pn;
		JLabel deck_pn;
		CardHandler hopr_pn;
		public PunchCardStatus() {
		}
	}

	PunchCardStatus[] sts;

	boolean readPunch = false;
	File _last = null;
	boolean isOn = false;
	CharConverter cvt;
	int vUnit;
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
		viewer = null;
		stall = new Semaphore(0);
		hopper = new CardHopper("Hopper", 20, 100, 4, false);
		stacker = new CardStacker("Stacker", 20, 100, 4, true);
		hopper.setListener(this);
		stacker.setListener(this);
		_last = new File(System.getProperty("user.dir"));
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

		sts = new PunchCardStatus[2];
		// Output
		sts[0] = new PunchCardStatus(); // output - punch
		sts[0].card = new byte[160]; // 80 columns, 16 bits each
		sts[0].count_pn = new JLabel();
		sts[0].count_pn.setPreferredSize(new Dimension(75, 20));
		sts[0].count_pn.setOpaque(true);
		sts[0].count_pn.setBackground(Color.white);
		sts[0].deck_pn = new JLabel(); // not used...
		sts[0].hopr_pn = stacker;
		// Input
		sts[1] = new PunchCardStatus(); // input - reader
		sts[1].card = new byte[160]; // 80 columns, 16 bits each
		sts[1].count_pn = new JLabel();
		sts[1].count_pn.setPreferredSize(new Dimension(75, 20));
		sts[1].count_pn.setOpaque(true);
		sts[1].count_pn.setBackground(Color.white);
		sts[1].deck_pn = new JLabel();
		sts[1].deck_pn.setPreferredSize(new Dimension(350, 20));
		sts[1].deck_pn.setBackground(Color.white);
		sts[1].deck_pn.setHorizontalAlignment(SwingConstants.RIGHT);
		sts[1].deck_pn.setOpaque(true);
		sts[1].hopr_pn = hopper;
		setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		setFont(font);
		gb = new GridBagLayout();
		setLayout(gb);

		// Top border
		gc.gridwidth = 15;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		int top = gc.gridy;
		gc.gridwidth = 1;
///
		// Left border
		gc.gridheight = 6;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 20));
		gb.setConstraints(pn, gc);
		add(pn);
		// Left inner border
		gc.gridx = 2;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 20));
		gb.setConstraints(pn, gc);
		add(pn);
		// Right inner border
		gc.gridx = 12;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 20));
		gb.setConstraints(pn, gc);
		add(pn);
		// Right border
		gc.gridx = 14;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 20));
		gb.setConstraints(pn, gc);
		add(pn);

		// Left side card handler...
		gc.gridheight = 4;
		gc.gridy = 3;
		gc.gridx = 1;
		gb.setConstraints(sts[0].hopr_pn, gc);
		add(sts[0].hopr_pn);
		// Right side card handler...
		gc.gridy = top;
		gc.gridx = 13;
		gb.setConstraints(sts[1].hopr_pn, gc);
		add(sts[1].hopr_pn);
		int left = gc.gridx = 3;
		gc.gridheight = 1;
///
		// Card hopper contents (top)
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridx;
		gc.gridwidth = 8;
		gb.setConstraints(sts[1].deck_pn, gc);
		add(sts[1].deck_pn);
		++gc.gridy;

		gc.gridx = left;
		gc.gridwidth = 9;
		pn = new JPanel();
		// TODO: adjust height if needed
		pn.setPreferredSize(new Dimension(10, 50));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		gc.gridwidth = 1;

		// Card handler counter headers...
		lb = new JLabel("Stacker");
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
		lb = new JLabel("Hopper");
		gb.setConstraints(lb, gc);
		add(lb);
		++gc.gridy;
///
		// Card handler counters... and button headers
		gc.gridx = left;
		gb.setConstraints(sts[0].count_pn, gc);
		add(sts[0].count_pn);
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
		gb.setConstraints(sts[1].count_pn, gc);
		add(sts[1].count_pn);
		++gc.gridy;
		gc.gridx = left;
///
		// Buttons...
		gc.gridwidth = 2;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		gc.gridx += 2;
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
		gc.gridwidth = 2;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(50, 5));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
///
		gc.gridx = left;
		gc.gridwidth = 9;
		pn = new JPanel();
		// TODO: adjust height if needed
		pn.setPreferredSize(new Dimension(10, 30));
		gb.setConstraints(pn, gc);
		add(pn);
		++gc.gridy;
		// Bottom border
		gc.gridx = 0;
		gc.gridwidth = 15;
		pn = new JPanel();
		pn.setPreferredSize(new Dimension(10, 5));
		gb.setConstraints(pn, gc);
		add(pn);

		addWindowListener(this);
		pack();

		hopper.addBlank(50);
		updateStacker();
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
		// TODO: clear hopper before adding?
		if (src == null) src = "Program";
		hopper.addInput(deck, src, count, false);
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
		// can't abort input if hopper empty.
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

	private void putCard() {
		sts[0].empty = true;
		stacker.putCard(sts[0].card);
	}

	private void passCard() {
		System.arraycopy(sts[1].card, 0, sts[0].card, 0, sts[1].card.length);
		sts[1].empty = true;
		sts[0].empty = false;
	}

	private boolean getCard() {
		// 'pcs' must be sts[1]...
		int a;
		while (true) {
			if (hopper.stackCount() == 0 || !ready) {
				// Not an error, just stall...
				doStall();
				if (hopper.stackCount() == 0) { // implied abort
					// assumed to be INITIALIZE - abort I/O
					// processor should not be running.
					return false;
				}
			}
			a = hopper.getCard(sts[1].card);
			sts[1].empty = !(a > 0);
			if (!sts[1].empty) {
				break;
			}
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
			putCard();
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
		putCard();
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

	// Reverse the low 12 bits
	private int reversePunch(int p) {
		int pp = rev4[(p >> 8) & 0x0f];
		pp |= rev4[(p >> 4) & 0x0f] << 4;
		pp |= rev4[p & 0x0f] << 8;
		return pp;
	}

	private void fillPunch() {
		if (!sts[0].empty) {
			return;
		}
		// need a card to punch...
		if (sts[1].empty) {
			// load card into read station...
			getCard();
		}
		// move card from read to punch...
		passCard();
	}

	private void doIn(RWChannel rwc, PunchCardStatus pcs) {
		rwc.startCLC();
		// 'pcs' must be sts[1]...
		vacateReader();	// just in case - should be no-op
		if (!getCard()) {
			return;
		}
		byte m;
		for (int x = 0; x < 80; ++x) {
			boolean stop = false;
			// Must not disturb punctuation...
			int p = getCol(pcs, x);
			if (pcs.code == 2) {
				p = reversePunch(p);
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
				p = reversePunch(p);
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
			hopper.repaint();
		}
		if (hopper.stackCount() > 0) {
			stall.release();
		}
	}

	private void updateStacker() {
		sts[0].count_pn.setText(String.format("%d", stacker.stackCount()));
	}

	private void updateHopper() {
		sts[1].count_pn.setText(String.format("%d", hopper.stackCount()));
		sts[1].deck_pn.setText(hopper.stackList(',', true));
	}

	private void viewStacker() {
		if (viewer == null) {
			viewer = DataCenter.makeViewer();
		}
		viewer.viewDeck(stacker.getDeck(), false, false);
	}

	public void actionPerformed(ActionEvent e) {
		if (e instanceof CardHandlerEvent) {
			CardHandlerEvent ae = (CardHandlerEvent)e;
			CardHandler cs = (CardHandler)e.getSource();
			String act = e.getActionCommand();
			if (act.equals("repaint")) {
				// don't consume, we also want default...
				if (cs == sts[0].hopr_pn) {
					updateStacker();
				} else {
					updateHopper();
				}
			} else if (act.equals("LEFT") && cs == sts[0].hopr_pn) {
				ae.consume();
				viewStacker();
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
		// but output is already ready...
		return true;
	}
	public boolean empty() {
		return (hopper.stackCount() == 0);
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
		if (!getCard()) {
			return null;	// should only be abort...
		}
		byte[] b = new byte[vUnit == 2 ? 160 : 80];
		for (int x = 0; x < 80; ++x) {
			int p = getCol(sts[1], x);
			if (vUnit == 2) { // raw mode...
				p = reversePunch(p);
				b[x * 2] = (byte)((p >> 6) & 077);
				b[x * 2 + 1] = (byte)(p & 077);
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
					p = reversePunch(p);
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
