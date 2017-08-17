// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.event.*;
import javax.swing.*;

public interface Machine {
	JFrame getFrame();
	JMenu[] getMenu();
	void setQuitListener(ActionListener lstn);
}
