// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.util.Arrays;
import java.util.LinkedList;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.border.*;

// CardHandler interface:
//	public void setListener(ActionListener lstn);
//	public int stackCount();
//	public String stackList(char delim, boolean blanks);
//
// CardHopper interface:
//	public void addBlank(int num);
//	public void addInput(InputStream deck, String src, int count);
//	public int getCard(byte[] card);	// -1 if empty

public class CardHopper extends CardHandler implements MouseListener {

	private int cards = 0;
	private int rest = 0;

	private static final int bdw = 3;	// width of BevelBorder
	private Color clr;
	private boolean topDown;
	private int width;
	private int height;
	private int scale;
	private ActionListener listener;
	private LinkedList<NamedCardDeck> hopper;
	private InputStream idev;
	private String name;
	private JPanel acc;
	JCheckBox acc_cb;
	JTextArea acc_stk;
	private File iprv;

	public CardHopper(String name, int wid, int hit, int sca, boolean topDown) {
		super();
		this.name = name;
		width = wid;
		height = hit;
		scale = sca;
		this.topDown = topDown;
		// TODO: share?
		iprv = new File(System.getProperty("user.dir"));
		setPreferredSize(new Dimension(wid + 2 * bdw, hit + 2 * bdw));
		setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		setBackground(Color.white);
		clr = buff1;
		listener = null;
		hopper = new LinkedList<NamedCardDeck>();
		clearHopper();
		update();
		addMouseListener(this);

		// TODO: share this?
		acc = new JPanel();
		GridBagLayout gb = new GridBagLayout();
		acc.setLayout(gb);
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
	}

	public void setListener(ActionListener lstn) {
		listener = lstn;
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
		cards = 0;
		rest = 0;
	}

	public String getLabel() {
		return name;
	}

	public void emptyHopper() {
		clearHopper();
		repaint();
	}

	public void addInput(InputStream deck, String src, int count,
							boolean clear) {
		// TODO: reject if hopper not empty? close it?
		// stack after? stack before?
		if (src == null) src = "Program";
		if (clear) {
			clearHopper();
		}
		addDeck(new NamedCardDeck(deck, src, count));
	}

	public File addDialog(String purp, File prev) {
		acc_stk.setText(stackList('\n', false));
		acc_cb.setSelected(false);
		SuffFileChooser ch = new SuffFileChooser(purp,
			new String[]{"pcd"}, new String[]{"Punch Card Deck"}, prev, acc);
		int rv = ch.showDialog(this);
		if (rv != JFileChooser.APPROVE_OPTION) {
			return null;
		}
		File fi = ch.getSelectedFile();
		if (!fi.exists()) { // can't happen?
			return null;
		}
		try {
			InputStream f = new FileInputStream(fi);
			String n = fi.getName();
			if (n.endsWith(".pcd")) {
				n = n.substring(0, n.length() - 4);
			}
			int c = (int)((fi.length() + 159) / 160);
			addInput(f, n, c, acc_cb.isSelected());
		} catch (Exception ee) {
			// TODO: errors
			return null;
		}
		return fi;
	}

	// Consistency:
	//	ASSERT if !hopper.isEmpty() then idev != null
	//	ASSERT if idev == null then hopper.isEmpty()
	//	if idev == null && cards > 0 then BLANK CARDS are loaded
	//	if idev == null && cards == 0 then NO CARDS at all are loaded

	// Hopper contains no cards at all.
	// Cannot be used during transitions (when idev or cards are inconsistent).
	private boolean hopperIsEmpty() {
		return idev == null && cards == 0;
	}

	// Hopper contains BLANK cards (only)
	// Cannot be used during transitions (when idev or cards are inconsistent).
	private boolean hopperHasBlank() {
		return  idev == null && cards > 0;
	}

	// Hopper contains non-BLANK cards
	private boolean hopperHasDeck() {
		return  idev != null;
	}

	public String stackList(char delim, boolean blanks) {
		if (hopperHasBlank() && blanks) {
			return "BLANK";
		}
		String ret = "";
		for (NamedCardDeck nis : hopper) {
			if (!ret.isEmpty()) {
				ret += delim;
			}
			ret += nis.name;
		}
		return ret;
	}

	public int stackCount() {
		return cards + rest;
	}

	public void addBlank(int num) {
		if (hopperHasDeck()) {
			clearHopper();
		}
		cards += num;
		update();
	}

	private int getStackCount() {
		int ret = 0;
		for (NamedCardDeck nis : hopper) {
			ret += nis.count;
		}
		return ret;
	}

	// Caller must update other displays (name list, count)
	private void addDeck(NamedCardDeck nis) {
		hopper.add(nis);
		if (!hopperHasDeck()) {
			nextDeck(); // updates display
			return;
		}
		// update display... might be partway through
		// current deck, so must be surgical...
		// leave 'cards' alone...
		rest = getStackCount() - hopper.peek().count;
		update();
	}

	public int getCard(byte[] card) {
		// TODO: enforce length 160?
		int a;
		while (true) {
			a = -1;
			if (hopperHasBlank()) {
				Arrays.fill(card, (byte)0);
				a = card.length;
			} else if (!hopperIsEmpty()) {
				try {
					// only one card read at a time... (?)
					a = idev.read(card);
				} catch (Exception ee) {
					// TODO: pass along exception?
				}
				// Should not normally reach "a < 0", but...
				if (a < 0 && finishDeck()) {
					continue;
				}
			}
			// 'a' could still be -1, means "no more cards"
			if (a > 0) {
				break;
			}
			cards = 0;
			rest = 0;
			repaint();
			return a;
		}
		--cards;
		if (cards <= 0) {
			finishDeck();
		} else {
			update();
		}
		return a;
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

	// Caller confirms "idev == null" before calling.
	// Note, this is transition code: idev and cards are not consistent.
	// Upon return, the state should be consistent.
	private boolean nextDeck() {
		if (hopper.isEmpty()) {
			// NOTE: this is not BLANK cards...
			cards = 0;
			rest = 0;
			update();
			return false;
		}
		NamedCardDeck nis = hopper.peek();
		idev = nis.real;
		cards = nis.count;
		rest = getStackCount() - cards;
		update();
		return true;
	}

	private void update() {
		repaint();
		if (listener != null) {
			setToolTipText(""); // in case user does not update
			CardHandlerEvent ae = new CardHandlerEvent(this,
				ActionEvent.ACTION_PERFORMED, "repaint");
			listener.actionPerformed(ae);
			if (ae.isConsumed()) {
				return;
			}
		}
		// default behavior
		String tip = getLabel();
		tip += String.format(": %d", stackCount());
		String lst = stackList(',', true);
		if (lst != null) {
			tip += '(';
			tip += lst;
			tip += ')';
		}
		setToolTipText(tip);
	}

	@Override
	public void paint(Graphics g) {
		super.paint(g);
		Graphics2D g2d = (Graphics2D)g;
		g2d.translate(bdw, bdw);	// based on border width
		g2d.setColor(clr);
		int n = (rest + cards + scale - 1) / scale;
		boolean max = false;
		if (n > height) {
			max = true;
			n = height;
		}
		if (n > 0) {
			if (topDown) {
				g2d.fillRect(0, 0, width + 1, n + 1);
			} else {
				g2d.fillRect(0, height - n, width + 1, n + 1);
			}
			if (max) {
				g2d.setColor(Color.red);
				g2d.drawLine(0, 0, width, 0);
			}
		}
	}

	public void mouseClicked(MouseEvent e) {
		String act;
		CardHandlerEvent ae;
		if (e.getButton() == MouseEvent.BUTTON1) {
			act = "left";
		} else if (e.getButton() == MouseEvent.BUTTON2) {
			act = "middle";
		} else if (e.getButton() == MouseEvent.BUTTON3) {
			act = "right";
		} else {
			return;
		}
		if ((e.getModifiers() & InputEvent.SHIFT_MASK) != 0) {
			act = act.toUpperCase();
		}
		if (listener != null) {
			ae = new CardHandlerEvent(this, e.getID(), act);
			listener.actionPerformed(ae);
			if (ae.isConsumed()) {
				return;
			}
		}
		if (act.equals("left")) {
			File f = addDialog("Add Deck", iprv);
			if (f != null) {
				iprv = f;
			}
		} else if (act.equals("LEFT")) {
			// viewer, some day?
		} else if (act.equals("right")) {
			addBlank(50);
		} else if (act.equals("RIGHT")) {
			emptyHopper();
		}
	}
	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}
	public void mousePressed(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}
}
