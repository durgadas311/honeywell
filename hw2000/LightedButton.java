import java.awt.*;
import javax.swing.*;

public class LightedButton extends JButton {
	Color _on, _off;
	boolean isOn;
	int id;
	public LightedButton(Color on, Color off, ImageIcon icon, int id) {
		super();
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
	}

	public boolean isOn() { return isOn; }

	public void setOn(boolean on) {
		isOn = on;
		if (on) {
			setBackground(_on);
		} else {
			setBackground(_off);
		}
	}

	public int getId() { return id; }
}
