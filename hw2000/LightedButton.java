import java.awt.*;
import javax.swing.*;

public class LightedButton extends JButton {
	Color _on, _off;
	boolean isOn;
	int id;
	LightedButton next;
	public LightedButton(Color on, Color off, ImageIcon icon, int id) {
		super();
		next = null;
		this.id = id;
		if (icon != null) {
			setIcon(icon);
		}
		setPreferredSize(new Dimension(30, 40));
		setBackground(off);
		setOpaque(true);
		_on = on;
		_off = off;
		isOn = false;
		LightedButton.add(this);
	}

	public boolean isOn() { return isOn; }

	public void lampTest(boolean test) {
		if (test || isOn) {
			setBackground(_on);
		} else {
			setBackground(_off);
		}
	}

	public void setOn(boolean on) {
		isOn = on;
		if (on) {
			setBackground(_on);
			LightedButton lb = next;
			while (lb != null && !this.equals(lb)) {
				lb.setOn(false);
				lb = lb.getNext();
			}
		} else {
			setBackground(_off);
		}
	}

	public int getId() { return id; }
	public LightedButton getNext() { return next; }
	public void setNext(LightedButton lb) { next = lb; }

	static LightedButton[] btns;
	static int nbtns;
	static public void init(int n) {
		btns = new LightedButton[n];
		nbtns = 0;
	}
	static public void add(LightedButton lb) {
		btns[nbtns++] = lb;
	}
	static public void doLampTest(boolean test) {
		for (int x = 0; x < nbtns; ++x) {
			btns[x].lampTest(test);
		}
	}
}
