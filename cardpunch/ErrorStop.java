// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.awt.*;
import javax.swing.*;

public class ErrorStop extends ProgStart {
	boolean errorStop;
	Vector<JComponent> lights;
	Color no_err;

	// Lights are only ever turned on by caller, based on conditions.
	public ErrorStop(Color off) {
		super(false);
		no_err = off;
		errorStop = false;
		lights = new Vector<JComponent>();
	}
	public boolean stopped() { return errorStop; }
	public void clear() {
		for (JComponent cmp : lights) {
			cmp.setBackground(no_err);
		}
		errorStop = false;
	}
	public void addLight(JComponent cmp) {
		lights.add(cmp);
	}
	@Override
	public void set(boolean b) {
		if (!b) return; // latching, only clear() turns off
		if (errorStop) return; // redundant
		errorStop = true;
	}
}
