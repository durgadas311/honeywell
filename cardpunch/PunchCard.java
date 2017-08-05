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

class PunchCard extends JLabel
		implements java.awt.image.ImageObserver {
	static final long serialVersionUID = 311614000001L;

	protected Font font1;
	protected ImageIcon _image;
	protected int _tranX;
	protected int _tranY;
	protected boolean _noCard;
	protected boolean _animate;
	protected CharConverter _cvt;
	protected Color ink;
	protected Color hole;
	protected byte[] _curr;
	protected int _cursor;

	private double _bit_spacing = 37.8;
	private double _bit_start = 26.7;
	private double _row_spacing = 13.1;
	private double _row_start = 34.4;
	private int _bit_width = 9;
	private int _bit_height = 20;
	private int _cols_per_card = 80;
	private int _curs_y0 = 10;
	private int _curs_y1;

	protected int getCode(byte[] card, int x) {
		int c = 0;
		if (x < 80) {
			c = card[x * 2] & 0x0ff;
			c |= (card[x * 2 + 1] & 0x0ff) << 8;
		}
		return c;
	}

	public void paint(Graphics g) {
		String ss;
		Graphics2D g2d = (Graphics2D)g;
		g2d.addRenderingHints(new RenderingHints(
			RenderingHints.KEY_ANTIALIASING,
			RenderingHints.VALUE_ANTIALIAS_ON));
		if (_noCard || _animate) {
			g2d.setColor(hole);
			Dimension d = getSize();
			g2d.fillRect(0, 0, d.width, d.height);
			if (_noCard) {
				return;
			}
			g2d.translate(_tranX, _tranY);
		}
		super.paint(g2d);
		g2d.setColor(ink);
		g2d.setFont(font1);
		int s;
		for (s = 0; s < _cols_per_card; ++s) {
			int c = 0;
			c = getCode(_curr, s);
			if ((c & 0x1000) == 0) {
				continue;
			}
			double rx = s * _row_spacing + _row_start;
			ss = _cvt.punToAscii(c);
			if (ss != null) {
				g2d.drawString(ss, (int)Math.round(rx), 17);
			}
		}
		g2d.setColor(hole);
		for (s = 0; s < _cols_per_card; ++s) {
			int c = 0;
			c = getCode(_curr, s);
			double rx = s * _row_spacing + _row_start;
			int b;
			for (b = 0; b < 12; ++b) {
				double ry = (b * _bit_spacing) + _bit_start;
				boolean m = ((c & 0x800) != 0);
				c <<= 1;
				if (m) {
					g2d.fillRect((int)Math.round(rx),
						(int)Math.round(ry),
						_bit_width, _bit_height);
				}
			}
		}
		if (_cursor > 0) {
			if (_cursor > 81) {
				_cursor = 81;
			}
			int rx = (int)Math.round(_cursor * _row_spacing + _row_start - _row_spacing);
			g2d.setColor(Color.red);
			g2d.drawLine(rx, _curs_y0, rx, _curs_y1);
		}
	}

	public PunchCard(CardPunchOptions opts) {
		super();
		_animate = false;
		_cursor = 1;
		_cvt = new CharConverter(opts);
		_noCard = true;

		String fn;
		if (opts.ibm026) {
			ink = new Color(120,0,255,175);
			if (opts.dots) {
				fn = "IBM026d.ttf";
			} else {
				fn = "IBM026.ttf";
			}
		} else {
			ink = new Color(0,0,0,128);
			if (opts.dots) {
				fn = "IBM029d.ttf";
			} else {
				fn = "IBM029.ttf";
			}
		}
		java.io.InputStream ttf = this.getClass().getResourceAsStream(fn);
		if (ttf != null) {
			try {
				Font font = Font.createFont(Font.TRUETYPE_FONT, ttf);
				font1 = font.deriveFont(16f);
			} catch (Exception ee) {
				font1 = new Font("Monospaced", Font.PLAIN, 14);
			}
		}
		_image = new ImageIcon(getClass().getResource("PunchCard2.png"));
		setIcon(_image);
		hole = Color.gray;
		setBackground(hole);
		setOpaque(true);
		setPreferredSize(new Dimension(_image.getIconWidth(), _image.getIconHeight()));
		_curs_y1 = _image.getIconHeight() - _curs_y0;
	}
}
