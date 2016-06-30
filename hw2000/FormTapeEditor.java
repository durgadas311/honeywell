import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.Arrays;

public class FormTapeEditor extends JFrame implements KeyListener {
	byte[] ftape;
	int idx;
	int tot;
	Font font;
	boolean changed;

	class TapePunch extends JPanel {
		static final int cell = 15;
		static final int num = 10;
		int tapew;
		int tapeh;
		int marg;
		int top;
		int chan1;
		int curs;

		public TapePunch() {
			super();
			tapew = 8 * cell + 5;
			tapeh = (num * 2 + 1) * cell;
			marg = 20;
			top = 15;
			curs = top + num * cell - 1;
			chan1 = marg + tapew - cell - 2;
			setPreferredSize(new Dimension(tapew + marg, tapeh + top));
			setBackground(Color.black);
		}

		public void paint(Graphics g) {
			super.paint(g);
			Graphics2D g2d = (Graphics2D)g;
			g2d.addRenderingHints(new RenderingHints(
				RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON));
			g2d.setColor(Color.gray);
			g2d.fillRect(marg, top, tapew, tapeh);
			g2d.setColor(Color.red);
			g2d.drawRect(0, curs, tapew + marg - 1, cell);
			g2d.setColor(Color.white);
			g2d.setFont(font);
			for (int x = 0; x < 8; ++x) {
				g2d.drawString(String.format("%1d", x + 1),
							chan1 - (x * cell), cell - 3);
			}
			int l = idx - num;
			if (l < 0) l += tot;
			for (int y = top + cell - 3; y < top + tapeh; y += cell) {
				g2d.drawString(String.format("%2d", l + 1), 0, y);
				if (++l >= tot) l = 0;
			}
			g2d.setColor(Color.black);
			l = idx - num;
			if (l < 0) l += tot;
			for (int y = top + 3; y < top + tapeh; y += cell) {
				for (int x = 0; x < 8; ++x) {
					if ((ftape[l] & (1 << x)) != 0) {
						g2d.fillOval(chan1 - (x * cell), y, 8, 8);
					}
				}
				if (++l >= tot) l = 0;
			}
		}
	}

	public FormTapeEditor(WindowListener lstr, byte[] tape) {
		super("Form Tape Editor");
		addWindowListener(lstr);
		ftape = Arrays.copyOf(tape, tape.length);
		changed = false;
		idx = 0;
		tot = ftape.length;
		font = new Font("Monospaced", Font.PLAIN, 12);
		add(new TapePunch());
		addKeyListener(this);
		pack();
		setVisible(true);
	}

	public boolean isChanged() {
		return changed;
	}

	public byte[] getTape() {
		return Arrays.copyOf(ftape, tot);
	}

	public void keyTyped(KeyEvent e) {
		char k = e.getKeyChar();
		if (k == ' ') {
			changed = true;
			ftape[idx] = 0;
		} else if (k < '1' && k > '8') {
			return;
		} else {
			k -= '1';
			ftape[idx] |= (1 << k);
			changed = true;
		}
		repaint();
	}
	public void keyPressed(KeyEvent e) {
		int c = e.getKeyCode();
		if (c == KeyEvent.VK_UP) {
			if (--idx < 0) idx += tot;
		} else if (c == KeyEvent.VK_DOWN) {
			if (++idx >= tot) idx = 0;
		} else if (c == KeyEvent.VK_DELETE) {
			if (tot <= 20) {
				return;
			}
			--tot;
			if (idx < tot) {
				for (int x = idx; x < ftape.length - 1; ++x) {
					ftape[x] = ftape[x + 1];
				}
			} else {
				idx = tot - 1;
			}
		} else if (c == KeyEvent.VK_INSERT) {
			if (tot >= 150) {
				return;
			}
			++tot;
			ftape = Arrays.copyOf(ftape, tot);
			for (int x = ftape.length - 1; x > idx; --x) {
				ftape[x] = ftape[x - 1];
			}
			ftape[idx] = 0;
		} else {
			return;
		}
		repaint();
	}
	public void keyReleased(KeyEvent e) { }

}
