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

class PunchCardDeck extends JLabel
		implements KeyListener, ActionListener, java.awt.image.ImageObserver {
	static final long serialVersionUID = 311614000000L;

	Font font1;
	ImageIcon _image;

	byte[] _code;
	int _code_used;
	String _title;
	File _file;
	int _pgix;
	int _npg;
	boolean _changed;
	JMenu _menu;
	Rectangle _top, _bottom;
	CharConverter _cvt;
	byte[] bb;
	Color ink = new Color(120,0,255,175);
	Color hole;

	public JMenu getMenu() { return _menu; }

	double _bit_spacing = 37.8;
	double _bit_start = 26.7;
	double _row_spacing = 13.1;
	double _row_start = 34.4;
	int _bit_width = 9;
	int _bit_height = 20;
	int _cols_per_card = 80;
	int _cursor;

	public Color getBg() { return hole; }

	private int getCode(int x) {
		int c = _code[x * 2] & 0x0ff;
		c |= (_code[x * 2 + 1] & 0x0ff) << 8;
		c &= 0x0fff;
		return c;
	}

	public void paint(Graphics g) {
		String ss;
		Graphics2D g2d = (Graphics2D)g;
		g2d.addRenderingHints(new RenderingHints(
			RenderingHints.KEY_ANTIALIASING,
			RenderingHints.VALUE_ANTIALIAS_ON));
		super.paint(g2d);
		g2d.setColor(ink);
		g2d.setFont(font1);
		int s;
		for (s = 0; s < _cols_per_card; ++s) {
			int c = 0;
			if (s < _code_used) {
				c = getCode(s);
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
			if (s < _code_used) {
				c = getCode(s);
			}
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
			int rx = (int)Math.round(_cursor * _row_spacing + _row_start - _row_spacing);
			g2d.setColor(Color.red);
			g2d.drawLine(rx, 10, rx, _bottom.y);
		}
	}

	public PunchCardDeck(String pgm) {
		super();
		_cursor = 1;
		_cvt = new CharConverter();
		bb = new byte[1];

		java.io.InputStream ttf = this.getClass().getResourceAsStream("IBM029.ttf");
		if (ttf != null) {
			try {
				Font font = Font.createFont(Font.TRUETYPE_FONT, ttf);
				font1 = font.deriveFont(16f);
			} catch (Exception ee) {
				font1 = new Font("Monospaced", Font.PLAIN, 14);
			}
		}
		_image = new ImageIcon(getClass().getResource("PunchCard.png"));
		setIcon(_image);
		hole = Color.gray;
		setBackground(hole);
		setOpaque(true);
		setPreferredSize(new Dimension(getIcon().getIconWidth(), getIcon().getIconHeight()));
		_top = new Rectangle(0, 0, 10, 10);
		_bottom = new Rectangle(0, getIcon().getIconHeight() - 10, 10, 10);

		_code = new byte[2*80];

		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("New", KeyEvent.VK_N);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Open", KeyEvent.VK_O);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Save", KeyEvent.VK_S);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Quit", KeyEvent.VK_Q);
		mi.addActionListener(this);
		mu.add(mi);
		_menu = mu;

		if (pgm == null) {
			newFile();
		} else {
			setupFile(new File(pgm));
		}
	}

	private void newCard() {
		Arrays.fill(_code, (byte)0);
		_code_used = 0;
		_changed = false;
	}

	private void newFile() {
		_title = "untitled";
		_file = null;
		_pgix = 0;
		_npg = 1;
		newCard();
	}

	private File pickFile(String purpose) {
		File file;
		SuffFileChooser ch = new SuffFileChooser(purpose,
			null); // dir
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		} else {
			file = null;
		}
		return file;
	}

	private void setupFile(File file) {
		if (file == null) {
			// change nothing in this case...
			return;
		}
		_title = file.getName();
		_file = file;
		if (file.exists()) {
			// TEMP: lose cursor when displaying character patterns
			_cursor = 0;

			FileInputStream f;
			try {
				f = new FileInputStream(file);
				_code_used = f.read(_code);
				_code_used /= 2;
			} catch (Exception ee) {
			}
			_changed = false;
			_pgix = 0;
			_npg = (_code_used + _cols_per_card - 1) / _cols_per_card;
			// or, always have +1 cards?
			if (_npg == 0) _npg = 1;
		} else {
			_code_used = 0;
			_pgix = 0;
			_npg = 1;
			_changed = false;
		}
	}

	private void saveFile() {
		FileOutputStream f = null;
		try {
			f = new FileOutputStream(_file);
		} catch (Exception ee) {
		}
		if (f == null) {
			return;
		}
		// need to restore "EOF" marker...
		int saved = _code_used;
//		try {
//			f.write(_code, 0, saved);
//		} catch (Exception ee) {
//		}
	}

	private void finishCard() {
		Dimension d = getSize();
		java.awt.image.BufferedImage i = new java.awt.image.BufferedImage(
			d.width, d.height, java.awt.image.BufferedImage.TYPE_INT_RGB);
		_cursor = 0;
		paint(i.getGraphics());
		String fn = String.format("pcard%02d.png", _pgix);
		try {
			javax.imageio.ImageIO.write(i, "png", new File(fn));
		} catch (IOException ee) {
			System.err.println("error writing " + fn);
		}
		++_pgix;
		++_npg;
		_cursor = 1;
		newCard();
		repaint();
	}

	public void keyTyped(KeyEvent e) {
		char c = e.getKeyChar();
		if (c == '\n') {
			finishCard();
			return;
		}
		int p = _cvt.asciiToPun((int)c);
		if (p < 0) {
			return;
		}
		int cx = (_cursor - 1) * 2;
		_code[cx] = (byte)(p & 0x0ff);
		_code[cx + 1] = (byte)((p >> 8) & 0x00f);
		if (_cursor < 80) {
			++_code_used;
			++_cursor;
		}
		repaint();
	}

	public void keyPressed(KeyEvent e) { }

	public void keyReleased(KeyEvent e) { }

	private boolean confirmChanges(String op) {
		if (_code_used > 0 && _changed) {
//			int res = Wang_UI.confirm(op, "Changes have not been saved. " +
//							"Discard changes?");
//			if (res == JOptionPane.YES_OPTION) {
//				return true;
//			}
			return false;
		}
		return true;
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_N) {
			if (!confirmChanges("New File")) {
				return;
			}
			newFile();
			repaint();
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_O) {
			if (!confirmChanges("Open File")) {
				return;
			}
			setupFile(pickFile("Load Card Deck"));
			repaint();
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_S) {
			if (_file == null) {
				File nu = pickFile("Save Card Deck As");
				if (nu == null) return;
				_file = nu;
				_title = _file.getName();
			}
			saveFile();
			_changed = false;
			repaint();
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			if (!confirmChanges("Quit")) {
				return;
			}
			System.exit(0);
			return;
		}
	}

}
