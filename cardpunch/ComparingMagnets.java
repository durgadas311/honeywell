// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import java.util.Arrays;

public class ComparingMagnets extends JPanel implements MouseListener {
	int width;

	class EntryItem extends ProgItem {
		public EntryItem(int w) {
			super(w);
			exit = false;
		}
		@Override
		public ProgStart get(int p) {
			if (ents[p] == null) {
				ents[p] = new ComparingEntry();
			}
			return ents[p];
		}
		public boolean compare(EntryItem other) {
			boolean eq = true;
			for (int x = 0; x < ents.length; ++x) {
				ComparingEntry e1 = (ComparingEntry)ents[x];
				ComparingEntry e2 = (ComparingEntry)other.ents[x];
				if (e1 == null && e2 == null) {
					continue; // OK
				}
				if (e1 == null || e2 == null || !e1.compare(e2)) {
					miss(x);
					eq = false;
				}
			}
			return eq;
		}
	}

	EntryItem ents1;
	EntryItem ents2;
	private static final int bdw = 3;
	private int height = 15;
	private boolean[] mag;
	private boolean any;
	private Color off = new Color(170, 170, 170);
	private Color on = new Color(200, 200, 170);

	public ComparingMagnets(int wid) {
		super();
		width = wid;
		ents1 = new EntryItem(wid);
		ents2 = new EntryItem(wid);
		mag = new boolean[wid];
		setPreferredSize(new Dimension(width * 2 + 2 * bdw, height + 2 * bdw));
		setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		addMouseListener(this);
		clear();
	}

	public void reset() {
		ents1.reset();
		ents2.reset();
	}

	public ProgItem A() { return ents1; }
	public ProgItem B() { return ents2; }

	public boolean processExits() {
		return ents1.compare(ents2);
	}

	public void clear() {
		Arrays.fill(mag, false);
		any = false;
		setBackground(off);
		repaint();
	}
	public void miss(int x) {
		mag[x] = true;
		if (!any) {
			any = true;
			setBackground(on);
		}
		repaint();
	}
	@Override
	public void paint(Graphics g) {
		super.paint(g);
		Graphics2D g2d = (Graphics2D)g;
		g2d.translate(bdw, bdw);
		g2d.setColor(Color.black);
		int xx = 1;
		for (int x = 0; x < mag.length; ++x) {
			if (mag[x]) {
				g2d.drawLine(xx, height - 10, xx, height);
			} else {
				g2d.drawLine(xx, height - 5, xx, height);
			}
			xx += 2;
		}
	}
	public void mouseClicked(MouseEvent e) {
		clear();
		repaint();
	}
	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}
	public void mousePressed(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}
}
