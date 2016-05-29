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
	String _title;
	File _progFile;
	File _cwd;
	FileOutputStream _outDeck = null;
	FileInputStream _inDeck = null;
	int _pgix;
	boolean _changed;
	JMenu[] _menus;
	Rectangle _top, _bottom;
	CharConverter _cvt;
	byte[] bb;
	Color ink = new Color(120,0,255,175);
	Color hole;
	byte[] _prog;
	byte[] _prev;
	byte[] _curr;
	boolean _currIsProg;
	boolean _saveImage;
	static final int _inset = 2;

	JCheckBox _autoSD_cb;
	JCheckBox _progSel_cb;
	JCheckBox _autoFeed_cb;
	JCheckBox _print_cb;

	public JMenu[] getMenu() { return _menus; }

	double _bit_spacing = 37.8;
	double _bit_start = 26.7;
	double _row_spacing = 13.1;
	double _row_start = 34.4;
	int _bit_width = 9;
	int _bit_height = 20;
	int _cols_per_card = 80;
	int _cursor;

	public Color getBg() { return hole; }

	private int getCode(byte[] card, int x) {
		int c = card[x * 2] & 0x0ff;
		c |= (card[x * 2 + 1] & 0x0ff) << 8;
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
			int rx = (int)Math.round(_cursor * _row_spacing + _row_start - _row_spacing);
			g2d.setColor(Color.red);
			g2d.drawLine(rx, 10, rx, _bottom.y);
		}
	}

	public PunchCardDeck(JFrame frame, String pgm) {
		super();
		_cursor = 1;
		_cvt = new CharConverter();
		bb = new byte[1];

		_cwd = new File(System.getProperty("user.dir"));
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
		_curr = _code;
		_currIsProg = false;
		_saveImage = false;
		_prev = null;
		_prog = new byte[2*80];
		// TODO: initialize program card from file...
		Arrays.fill(_prog, (byte)0);
		_progFile = null;

		_menus = new JMenu[2];
		JMenu mu;
		JMenuItem mi;
		mu = new JMenu("File");
		mi = new JMenuItem("Output", KeyEvent.VK_O);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Input", KeyEvent.VK_I);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Quit", KeyEvent.VK_Q);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[0] = mu;
		mu = new JMenu("Prog");
		mi = new JMenuItem("Load", KeyEvent.VK_L);
		mi.addActionListener(this);
		mu.add(mi);
		mi = new JMenuItem("Save", KeyEvent.VK_S);
		mi.addActionListener(this);
		mu.add(mi);
		_menus[1] = mu;

		_autoSD_cb = new JCheckBox("Auto SKIP/DUP");
		_autoSD_cb.setFocusable(false);
		_progSel_cb = new JCheckBox("Prog 2 (1)");
		_progSel_cb.setFocusable(false);
		_autoFeed_cb = new JCheckBox("Auto Feed");
		_autoFeed_cb.setFocusable(false);
		_print_cb = new JCheckBox("Print");
		_print_cb.setFocusable(false);
		_print_cb.setSelected(true);
		JPanel pn = new JPanel();
		pn.setPreferredSize(new Dimension(getIcon().getIconWidth() + 2 * _inset, 30));
		pn.add(_autoSD_cb);
		pn.add(_progSel_cb);
		pn.add(_autoFeed_cb);
		pn.add(_print_cb);

		if (pgm == null) {
			newFile();
		} else {
			setupFile(new File(pgm));
		}
		GridBagLayout gridbag = new GridBagLayout();
		frame.setLayout(gridbag);
		GridBagConstraints gc = new GridBagConstraints();
		gc.fill = GridBagConstraints.NONE;
		gc.gridx = 0;
		gc.gridy = 0;
		gc.weightx = 0;
		gc.weighty = 0;
		gc.gridwidth = 1;
		gc.gridheight = 1;
		gc.insets.bottom = 0;
		gc.insets.top = 0;
		gc.insets.left = 0;
		gc.insets.right = 0;
		gc.anchor = GridBagConstraints.NORTH;

		pn.setFocusable(false);
		gridbag.setConstraints(pn, gc);
		frame.add(pn);
		++gc.gridy;
		gc.insets.bottom = _inset;
		gc.insets.top = _inset;
		gridbag.setConstraints(this, gc);
		frame.add(this);

		frame.addKeyListener(this);
	}
	private void nextCol() {
		++_cursor;
		// TODO: handle auto-skip/dup
	}

	private void newCard() {
		if (_currIsProg) {
			_curr = _code;
			_currIsProg = false;
			_cursor = 1;
			repaint();
			return;
		}
		if (_prev != null) {
			// anything more required to free array?
			_prev = null;
		}
		_prev = _code;
		_code = new byte[2*80];
		_curr = _code;
		Arrays.fill(_code, (byte)0);
		if (_inDeck != null) {
			try {
				int n = _inDeck.read(_code);
				if (n <= 0) {
					_inDeck.close();
					_inDeck = null;
				}
			} catch (Exception ee) {
				ee.printStackTrace();
			}
		}
		if (_outDeck != null && _prev != null) {
			try {
				_outDeck.write(_prev);
			} catch (Exception ee) {
				ee.printStackTrace();
			}
		}
		++_pgix;
		_cursor = 1;
		_changed = false;
		repaint();
	}

	private void skipStart() {
		if (_currIsProg) {
			newCard();
			return;
		}
		do {
			if (_cursor >= 80) {
				newCard();
				return;
			}
			++_cursor;
			int c = getCode(_prog, _cursor - 1);
			// TODO: handle "program 2"
			if ((c & 0x800) == 0) {
				break;
			}
		} while (true);
		repaint();
	}

	private void newFile() {
		_pgix = 0;
		newCard();
	}

	private File pickFile(String purpose, File prev) {
		File file;
		SuffFileChooser ch = new SuffFileChooser(purpose, prev);
		int rv = ch.showDialog(this);
		if (rv == JFileChooser.APPROVE_OPTION) {
			file = ch.getSelectedFile();
		} else {
			file = null;
		}
		return file;
	}

	private void setupFile(File file) {
		if (_outDeck != null) {
			try {
				_outDeck.close();
			} catch (Exception ee) {}
			_outDeck = null;
		}
		try {
			_outDeck = new FileOutputStream(file);
		} catch (Exception ee) {
			ee.printStackTrace();
		}
	}

	private void inputFile(File file) {
		if (file == null) {
			// change nothing in this case...
			return;
		}
		if (_inDeck != null) {
			try {
				_inDeck.close();
			} catch (Exception ee) {}
			_inDeck = null;
		}
		if (file.exists()) {
			// TEMP: lose cursor when displaying character patterns
			_cursor = 0;

			try {
				_inDeck = new FileInputStream(file);
			} catch (Exception ee) {
				ee.printStackTrace();
			}
			_changed = false;
			_pgix = 0;
		} else {
			_pgix = 0;
			_changed = false;
		}
		newCard();
	}

	private void loadProg(File file) {
		FileInputStream f;
		try {
			f = new FileInputStream(file);
			int n = f.read(_prog);
			if (n < 160) {
				Arrays.fill(_prog, n, 160, (byte)0);
			}
		} catch (Exception ee) {
			// TODO: report errors
			ee.printStackTrace();
		}
	}

	private void saveProg(File file) {
		FileOutputStream f = null;
		try {
			f = new FileOutputStream(file);
		} catch (Exception ee) {
		}
		if (f == null) {
			return;
		}
		try {
			f.write(_prog);
		} catch (Exception ee) {
			// TODO: report errors
			ee.printStackTrace();
		}
	}

	private void finishCard() {
		if (_saveImage) {
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
		}
		// TODO: save card
		newCard();	// does repaint
	}

	private void punch(int p) {
		if (p != 0) {
			// this corrupts 'p'...
			if (_print_cb.isSelected()) {
				p |= 0x1000;
			}
			int cx = (_cursor - 1) * 2;
			_curr[cx] |= (byte)(p & 0x0ff);
			_curr[cx + 1] |= (byte)((p >> 8) & 0x0ff);
		}
		if (_cursor < 80) {
			// TODO: auto skip to next card...
			nextCol();
			// TODO: handle AUTO SKIP/DUP
			// if ((c & 0x400) == 0) { // SKIP
			// }
			// if ((c & 0x200) == 0) { // DUP
			// }
		}
	}

	private void dupStart() {
		// If start of field, then dup entire field...
		boolean cont = ((getCode(_prog, _cursor - 1) & 0x0800) == 0);
		do {
			int p = 0;
			if (_prev != null) {
				p = getCode(_prev, _cursor - 1) & 0x0fff;
			}
			punch(p);
			cont = cont && ((getCode(_prog, _cursor - 1) & 0x0800) != 0);
		} while (cont);
		repaint();
	}

	public void keyTyped(KeyEvent e) {
		char c = e.getKeyChar();
		int p = 0;
		if (c == '\n') {
			finishCard();
			return;
		}
		if (c == '\t') {
			skipStart();
			return;
		}
		if (c == '\001') {
			_currIsProg = true;
			_curr = _prog;
			_cursor = 1;
			repaint();
			return;
		}
		if (c == '\b') {
			if (_cursor > 1) {
				--_cursor;
				repaint();
			}
			return;
		}
		if (c == '\004') {	// DUP
			dupStart();
			return;
		}
		// TODO: handle ALHPA SHIFT
		// if ((c & 0x100) == 0) {
		// }
		c = Character.toUpperCase(c);
		p = _cvt.asciiToPun((int)c);
		if (p < 0) {
			return;
		}
		punch(p);
		repaint();
	}

	public void keyPressed(KeyEvent e) { }

	public void keyReleased(KeyEvent e) { }

	private boolean confirmChanges(String op) {
//			int res = Wang_UI.confirm(op, "Changes have not been saved. " +
//							"Discard changes?");
//			if (res == JOptionPane.YES_OPTION) {
//				return true;
//			}
		return true;
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JMenuItem)) {
			return;
		}
		JMenuItem m = (JMenuItem)e.getSource();
		if (m.getMnemonic() == KeyEvent.VK_O) {
			setupFile(pickFile("Set Output Card Deck", _cwd));
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_I) {
			inputFile(pickFile("Set Input Card Desk", _cwd));
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_Q) {
			if (_inDeck != null) {
				try {
					_inDeck.close();
				} catch (Exception ee) {}
				_inDeck = null;
			}
			if (_outDeck != null) {
				try {
					_outDeck.close();
				} catch (Exception ee) {}
				_outDeck = null;
			}
			System.exit(0);
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_L) {
			File nu = pickFile("Load Prog Card",
				_progFile == null ? _cwd : _progFile);
			if (nu == null) return;
			_progFile = nu;
			loadProg(_progFile);
			return;
		} else if (m.getMnemonic() == KeyEvent.VK_S) {
			if (_progFile == null) {
				File nu = pickFile("Save Prog Card As", _cwd);
				if (nu == null) return;
				_progFile = nu;
			}
			saveProg(_progFile);
			return;
		}
	}

}
