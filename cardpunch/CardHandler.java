// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.awt.event.*;
import javax.swing.*;

public abstract class CardHandler extends JPanel {
	CardHandler() {
		super();
	}

	abstract String getLabel();
	abstract void setListener(ActionListener lstn);
	abstract String stackList(char delim, boolean blanks);
	abstract int stackCount();
}
