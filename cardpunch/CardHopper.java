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

	public CardHopper(String name, int wid, int hit, int sca, boolean topDown) {
		super();
		this.name = name;
		width = wid;
		height = hit;
		scale = sca;
		this.topDown = topDown;
		setPreferredSize(new Dimension(wid + 2 * bdw, hit + 2 * bdw));
		setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		setBackground(Color.white);
		clr = new Color(215,205,154);
		listener = null;
		hopper = new LinkedList<NamedCardDeck>();
		clearHopper();
		addMouseListener(this);
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
		if (listener == null) {
			return;
		}
		listener.actionPerformed(new ActionEvent(this,
			ActionEvent.ACTION_PERFORMED, "repaint"));
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
		if (listener == null) {
			return;
		}
		ActionEvent ae;
		if (e.getButton() == MouseEvent.BUTTON1) {
			ae = new ActionEvent(this, e.getID(), "left");
		} else if (e.getButton() == MouseEvent.BUTTON2) {
			ae = new ActionEvent(this, e.getID(), "middle");
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
