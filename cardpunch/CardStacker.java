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
//	String stackList(char delim, boolean blanks);
//
// CardStacker interface:
//	public boolean saveDeck(File dest);
//	public void discardDeck();
//	public void putCard(byte[] card);

public class CardStacker extends CardHandler implements MouseListener {
	private int cards = 0;

	private static final int bdw = 3;	// width of BevelBorder
	private Color clr;
	private boolean topDown;
	private int width;
	private int height;
	private int scale;
	private ActionListener listener;
	private OutputStream odev;
	private File ofil;
	private File oprv;
	private String name;

	public CardStacker(String name, int wid, int hit, int sca, boolean topDown) {
		super();
		this.name = name;
		this.topDown = topDown;
		width = wid;
		height = hit;
		scale = sca;
		// TODO: share?
		oprv = new File(System.getProperty("user.dir"));
		this.topDown = topDown;
		setPreferredSize(new Dimension(wid + 2 * bdw, hit + 2 * bdw));
		setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		setBackground(Color.white);
		clr = buff1;
		listener = null;
		addMouseListener(this);
		try {
			ofil = File.createTempFile("PCD-", ".pcd");
		} catch(Exception ee) {
			System.err.println(ee.getMessage());
			ofil = new File("/tmp/PCD.pcd");
		}
		ofil.deleteOnExit();
		initStacker();
	}

	public void setListener(ActionListener lstn) {
		listener = lstn;
	}

	public String stackList(char delim, boolean blanks) {
		return null;
	}

	public int stackCount() {
		return cards;
	}

	public String getLabel() {
		return name;
	}

	private void restoreStacker() {
		cards = (int)((ofil.length() + 159) / 160);
		try {
			odev = new FileOutputStream(ofil, true);
		} catch (Exception ee) {
			// TODO: some indicator?
		}
		update();
	}

	private void initStacker() {
		cards = 0;
		try {
			ofil.delete();
			ofil.createNewFile();
			odev = new FileOutputStream(ofil);
		} catch (Exception ee) {
			// TODO: some indicator?
		}
		update();
	}

	public void putCard(byte[] card) {
		// TODO: enforce length 160?
		if (odev != null) {
			try {
				odev.write(card);
			} catch (Exception ee) {
				// TODO: handle exceptions? pass along?
			}
		}
		++cards;
		update();
	}

	// Use caution! should use read-only
	public File getDeck() { return ofil; }

	public void discardDeck() {
		if (odev != null) {
			try {
				odev.close();
			} catch (Exception ee) {}
			odev = null;
		}
		initStacker();
	}

	public boolean saveDeck(OutputStream dest) {
		if (odev != null) {
			try {
				odev.close();
			} catch (Exception ee) {}
			odev = null;
		}
		boolean ok = false;
		try {
			InputStream i = new FileInputStream(ofil);
			byte[] buf = new byte[(int)ofil.length()];
			if (i.read(buf) > 0) {
				dest.write(buf);
			}
			i.close();
			ok = true;
		} catch (Exception ee) {
			ok = false;
		}
		if (!ok) {
			// Restore output deck...
			restoreStacker();
			return false;
		}
		initStacker();
		return true;
	}

	public boolean saveDeck(File dest) {
		if (odev != null) {
			try {
				odev.close();
			} catch (Exception ee) {}
			odev = null;
		}
		boolean ok = false;
		try {
			dest.delete();
		} catch (Exception ee) {}
		try {
			ok = ofil.renameTo(dest);
		} catch (Exception ee) {
			ok = false;
		}
		if (!ok) {
			// Restore output deck...
			restoreStacker();
			return false;
		}
		initStacker();
		return true;
	}

	public File saveDialog(String purp, File prev) {
		SuffFileChooser ch = new SuffFileChooser(purp,
			new String[]{"pcd"}, new String[]{"Punch Card Deck"}, prev, null);
		int rv = ch.showDialog(this);
		if (rv != JFileChooser.APPROVE_OPTION) {
			return null;
		}
		if (!saveDeck(ch.getSelectedFile())) {
			return null;
		}
		return ch.getSelectedFile();
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
		setToolTipText(tip);
	}

	@Override
	public void paint(Graphics g) {
		super.paint(g);
		Graphics2D g2d = (Graphics2D)g;
		g2d.translate(bdw, bdw);	// based on border width
		g2d.setColor(clr);
		int n = (cards + scale - 1) / scale;
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
			File f = saveDialog("Save Deck", oprv);
			if (f != null) {
				oprv = f;
			}
		} else if (act.equals("LEFT")) {
			// we don't have access to viewer...
		} else if (act.equals("right")) {
			discardDeck();
		} else if (act.equals("RIGHT")) {
			// any action? remove last card?
		}
	}
	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}
	public void mousePressed(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}
}
