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
	private String name;

	public CardStacker(String name, int wid, int hit, int sca, boolean topDown) {
		super();
		this.name = name;
		this.topDown = topDown;
		width = wid;
		height = hit;
		scale = sca;
		this.topDown = topDown;
		setPreferredSize(new Dimension(wid + 2 * bdw, hit + 2 * bdw));
		setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		setBackground(Color.white);
		clr = new Color(215,205,154);
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

	public void discardDeck() {
		if (odev != null) {
			try {
				odev.close();
			} catch (Exception ee) {}
			odev = null;
		}
		initStacker();
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
