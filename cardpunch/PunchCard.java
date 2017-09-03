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
	protected boolean _animate;
	protected CharConverter _cvt;
	protected Color ink;
	protected Color hole;
	protected byte[] _curr;
	protected int _cursor;

	private int _bit_spacing = 23;
	private int _bit_start = 15;
	private int _row_spacing = 8;
	private int _row_start = 18;
	private int _baseline = 10;
	private int _bit_width = _row_spacing - 2;
	private int _bit_height = 13;
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

	private byte[] _curr_card;
	private boolean _curr_anim;
	private int _curr_tranX;
	private int _curr_tranY;
	private synchronized void getState() {
		_curr_card = _curr;
		_curr_anim = _animate;
		_curr_tranX = _tranX;
		_curr_tranY = _tranY;
	}
	protected synchronized void setState(byte[] card, boolean animate) {
		_curr = card;
		_animate = animate;
		if (!animate) {
			_tranX = _tranY = 0;
		}
	}

	public void paint(Graphics g) {
		String ss;
		Graphics2D g2d = (Graphics2D)g;
		g2d.addRenderingHints(new RenderingHints(
			RenderingHints.KEY_ANTIALIASING,
			RenderingHints.VALUE_ANTIALIAS_ON));
		getState();
		if (_curr_card == null || _curr_anim) {
			g2d.setColor(hole);
			Dimension d = getSize();
			g2d.fillRect(0, 0, d.width, d.height);
			if (_curr_card == null) {
				return;
			}
			g2d.translate(_curr_tranX, _curr_tranY);
		}
		super.paint(g2d);
		g2d.setColor(ink);
		g2d.setFont(font1);
		int s;
		for (s = 0; s < _cols_per_card; ++s) {
			int c = 0;
			c = getCode(_curr_card, s);
			if ((c & 0x1000) == 0) {
				continue;
			}
			int rx = s * _row_spacing + _row_start + 1;
			ss = _cvt.punToAscii(c);
			if (ss != null) {
				g2d.drawString(ss, rx, _baseline);
			}
		}
		g2d.setColor(hole);
		for (s = 0; s < _cols_per_card; ++s) {
			int c = 0;
			c = getCode(_curr_card, s);
			int rx = s * _row_spacing + _row_start;
			int b;
			for (b = 0; b < 12; ++b) {
				int ry = (b * _bit_spacing) + _bit_start;
				boolean m = ((c & 0x800) != 0);
				c <<= 1;
				if (m) {
					g2d.fillRect(rx, ry,
						_bit_width, _bit_height);
				}
			}
		}
		if (_cursor > 0) {
			if (_cursor > 81) {
				_cursor = 81;
			}
			int rx = _cursor * _row_spacing + _row_start - _row_spacing;
			g2d.setColor(Color.red);
			g2d.drawLine(rx, _curs_y0, rx, _curs_y1);
		}
	}

	public PunchCard(CardPunchOptions opts) {
		super();
		_animate = false;
		_cursor = 1;
		_cvt = new CharConverter(opts);

		String fn;
		if (opts.ibm026) {
			ink = new Color(120,0,255,175);
			fn = "fonts/IBM026.ttf";
		} else {
			ink = new Color(0,0,0,128);
			fn = "fonts/IBM029.ttf";
		}
		java.io.InputStream ttf = this.getClass().getResourceAsStream(fn);
		if (ttf != null) {
			try {
				Font font = Font.createFont(Font.TRUETYPE_FONT, ttf);
				font1 = font.deriveFont(8f);
			} catch (Exception ee) {
				font1 = new Font("Monospaced", Font.PLAIN, 10);
			}
		}
		_image = new ImageIcon(getClass().getResource("PunchCard.png"));
		setIcon(_image);
		hole = Color.gray;
		setBackground(hole);
		setOpaque(true);
		setPreferredSize(new Dimension(_image.getIconWidth(), _image.getIconHeight()));
		_curs_y1 = _image.getIconHeight() - _curs_y0;
	}
}
