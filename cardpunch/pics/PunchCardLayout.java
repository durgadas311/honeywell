// Copyright (c) 2016 Douglas Miller

import java.awt.*;
import javax.swing.*;
import java.awt.geom.AffineTransform;
import java.io.*;
import java.awt.event.*;
import java.util.Arrays;
import java.awt.geom.Path2D;

import java.awt.print.*;
import javax.print.attribute.*;
import javax.print.attribute.standard.*;

class PunchCardLayout extends JLabel
		implements java.awt.image.ImageObserver, ActionListener {

	protected Font font1;
	protected Font font2;
	protected Color ink;
	protected Color ink2;
	protected Color card;
	int row = 23;
	int col = 8;
	int left = 18;
	int top = 72; // err, minus X and R... at baseline
	int half = 10;
	int row1 = top + half;
	int row9 = row1 + 8 * row;

	private Path2D.Float punchCard;

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
		g2d.fill(punchCard);
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
		g2d.setColor(ink2);
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

	public PunchCardLayout(JFrame frame) {
		super();
		ink = Color.black;
		ink2 = Color.gray;
		card = new Color(243, 226, 182);
		setOpaque(false);
		//setBackground(Color.gray);
		//setOpaque(true);
		font1 = new Font("Sans-serif", Font.PLAIN, 11);
		font2 = new Font("Sans-serif", Font.PLAIN, 6);
		int radius = 40;
		int width = 681;
		int height = 300;
		float delta = 1.553f;
		setPreferredSize(new Dimension(width, height));
		JMenuBar mb = new JMenuBar();
		JButton btn = new JButton("Snap");
		btn.addActionListener(this);
		mb.add(btn);
		frame.setJMenuBar(mb);
		punchCard = new Path2D.Float();
		punchCard.moveTo(20, 0);
		punchCard.lineTo(width - radius, 0);
		punchCard.curveTo(width, 0 - delta, width + delta, 0, width, radius);
		punchCard.lineTo(width, height - radius);
		punchCard.curveTo(width + delta, height, width, height + delta, width - radius, height);
		punchCard.lineTo(0 + radius, height);
		punchCard.curveTo(0, height + delta, 0 - delta, height, 0, height - radius);
		punchCard.lineTo(0, 36);
		punchCard.closePath();
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JButton) {
			saveImage(new File("cardImage01.png"), true);
		}
	}

	private void saveImage(File fn, boolean transparent) {
		Dimension d = getSize();
		java.awt.image.BufferedImage i = new java.awt.image.BufferedImage(
			d.width, d.height, transparent ?
				java.awt.image.BufferedImage.TYPE_INT_ARGB :
				java.awt.image.BufferedImage.TYPE_INT_RGB);
		paint(i.getGraphics());
		try {
			javax.imageio.ImageIO.write(i, "png", fn);
		} catch (IOException ee) {
			System.err.println("error writing " + fn.getAbsolutePath());
		}
	}

	static JFrame frame;

	public static void main(String[] args) {
		punch = (args.length > 0 && args[0].equals("punch"));
		frame = new JFrame("Punch Card Layout");
		frame.add(new PunchCardLayout(frame));
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setBackground(Color.red);
		frame.pack();
		frame.setVisible(true);
	}
}
