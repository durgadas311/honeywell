// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public abstract class CardHandler extends JPanel {
	static final Color buff1 = new Color(243, 226, 182);
	static final Color buff2 = new Color(247, 227, 179);
	static final Color buff3 = new Color(245, 231, 191);
	CardHandler() {
		super();
	}

	abstract String getLabel();
	abstract void setListener(ActionListener lstn);
	abstract String stackList(char delim, boolean blanks);
	abstract int stackCount();
}
