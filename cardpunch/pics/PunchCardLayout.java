// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import java.awt.geom.AffineTransform;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;

import java.awt.print.*;
import javax.print.attribute.*;
import javax.print.attribute.standard.*;

class PunchCardLayout extends JLabel
		implements java.awt.image.ImageObserver {

	protected Font font1;
	protected Font font2;
	protected Color ink;
	protected Color card;
	int row = 23;
	int col = 8;
	int left = 18;
	int top = 72; // err, minus X and R... at baseline
	int half = 10;
	int row1 = top + half;
	int row9 = row1 + 8 * row;

	Polygon clip = new Polygon(new int[]{0, 20, 0},
				new int[]{0, 0, 36},
				3);

	private int _cols_per_card = 80;
	static boolean punch;

	public void paint(Graphics g) {
		String ss;
		Graphics2D g2d = (Graphics2D)g;
		g2d.addRenderingHints(new RenderingHints(
			RenderingHints.KEY_ANTIALIASING,
			RenderingHints.VALUE_ANTIALIAS_ON));
		super.paint(g2d);
		g2d.setColor(card);
		g2d.fillRoundRect(0, 0, 681, 300, 40, 40);
		g2d.setColor(getBackground());
		g2d.fillPolygon(clip);
		g2d.setColor(ink);
		g2d.setFont(font1);
		int s;
		for (s = 0; s < _cols_per_card; ++s) {
			int x = col * s + left;
			int y = top; // err, plus X and R
			g2d.drawString("0", x, y);
			y += row;
			g2d.drawString("1", x, y);
			y += row;
			g2d.drawString("2", x, y);
			y += row;
			g2d.drawString("3", x, y);
			y += row;
			g2d.drawString("4", x, y);
			y += row;
			g2d.drawString("5", x, y);
			y += row;
			g2d.drawString("6", x, y);
			y += row;
			g2d.drawString("7", x, y);
			y += row;
			g2d.drawString("8", x, y);
			y += row;
			g2d.drawString("9", x, y);
		}
		g2d.setFont(font2);
		for (s = 0; s < _cols_per_card; ++s) {
			ss = String.format("%d", s + 1);
			int x = col * s + left;
			if (s < 9) {
				x += 1;
			}
			g2d.drawString(ss, x, row1);
			g2d.drawString(ss, x, row9);
		}
		if (punch) {
			g2d.setColor(Color.red);
			int y = top - 11 - (2 * row);
			for (s = 0; s < 12; ++s) {
				g2d.drawRect(left, y, col - 2, 13);
				y += row;
			}
		}
	}

	public PunchCardLayout() {
		super();
		ink = Color.black;
		card = new Color(243, 226, 182);
		setBackground(Color.gray);
		font1 = new Font("Sans-serif", Font.PLAIN, 11);
		font2 = new Font("Sans-serif", Font.PLAIN, 6);
		setOpaque(true);
		setPreferredSize(new Dimension(681, 300));
	}

	static JFrame frame;

	public static void main(String[] args) {
		punch = (args.length > 0 && args[0].equals("punch"));
		frame = new JFrame("Punch Card Layout");
		frame.add(new PunchCardLayout());
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}
}
