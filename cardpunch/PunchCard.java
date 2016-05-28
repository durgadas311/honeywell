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
		implements KeyListener, ActionListener, java.awt.image.ImageObserver {
	static final long serialVersionUID = 311614000000L;

	Font font1 = new Font("Monospaced", Font.PLAIN, 12);
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

	public JMenu getMenu() { return _menu; }

	double _bit_spacing = 37.8;
	double _bit_start = 26.7;
	double _row_spacing = 13.1;
	double _row_start = 34.4;
	int _bit_width = 10;
	int _bit_height = 20;
	int _cols_per_card = 80;

	public void paint(Graphics g) {
		Graphics2D g2d = (Graphics2D)g;
		super.paint(g2d);
		g2d.setColor(Color.black);
		g2d.setFont(font1);
		int s;
		for (s = 0; s < _cols_per_card; ++s) {
			int cx = _pgix * _cols_per_card + s;
			int c = 0;
			if (cx < _code_used) {
				c = _code[cx * 2] & 0x0ff;
				c |= (_code[cx * 2 + 1] & 0x0ff) << 8;
				c &= 0x0fff;
			}
			double rx = s * _row_spacing + _row_start;
			bb[0] = _cvt.punToAscii(c);
			if (bb[0] != 0) {
				g2d.drawString(new String(bb), (int)Math.round(rx), 15);
			}
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
	}

	public PunchCard(String pgm) {
		super();
		_cvt = new CharConverter();
		bb = new byte[1];

		_image = new ImageIcon(getClass().getResource("PunchCard.png"));
		setIcon(_image);
		setBackground(Color.black);
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

	private void newFile() {
		_title = "untitled";
		_file = null;
		_code_used = 0;
		_pgix = 0;
		_npg = 1;
		_changed = false;
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

	public void keyTyped(KeyEvent e) { }

	public void keyPressed(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_LEFT) {
			if (_pgix * _cols_per_card == _code_used && _npg > 1) {
				--_npg;
			}
			--_pgix;
			if (_pgix < 0) {
				_pgix = 0;
			}
			repaint();
		} else if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
			++_pgix;
			if (_pgix >= _npg) {
				if (_pgix * _cols_per_card == _code_used) {
					++_npg;
				} else {
					_pgix = _npg - 1;
				}
			}
			// allow for adding new page???
			repaint();
		} else if (e.getKeyCode() == KeyEvent.VK_UP) {
			scrollRectToVisible(_top);
		} else if (e.getKeyCode() == KeyEvent.VK_DOWN) {
			scrollRectToVisible(_bottom);
		}
	}

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
