// Copyright (c) 2011,2014 Douglas Miller
// $Id: SuffFileChooser.java,v 1.4 2014/01/14 21:53:51 drmiller Exp $

import java.awt.*;
import java.io.*;
import javax.swing.*;
import javax.swing.filechooser.FileView;

class HW2000FileView extends FileView {
	Icon dpi;
	Icon mti;
	Icon pcd;

	public HW2000FileView() {
		super();
		dpi = new ImageIcon(getClass().getResource("icons/dpi-24.png"));
		mti = new ImageIcon(getClass().getResource("icons/mti-24.png"));
		pcd = new ImageIcon(getClass().getResource("icons/pcd-24.png"));
	}
	public Icon getIcon(File fi) {
		String nm = fi.getName();
		if (nm.endsWith(".dpi")) {
			return dpi;
		} else if (nm.endsWith(".mti")) {
			return mti;
		} else if (nm.endsWith(".pcd")) {
			return pcd;
		}
		return super.getIcon(fi);
	}
}
