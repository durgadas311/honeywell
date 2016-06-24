// Copyright (c) 2011,2014 Douglas Miller
// $Id: SuffFileChooser.java,v 1.4 2014/01/14 21:53:51 drmiller Exp $

import java.awt.*;
import java.io.*;
import javax.swing.*;

class SuffFileChooser extends JFileChooser {
	static final long serialVersionUID = 311457692041L;
	private String _btn;
	private class Listing extends JComponent {
		static final long serialVersionUID = 31170769203L;
		public Checkbox btn;
		public Listing(String b) {
			btn = new Checkbox(b);
			setLayout(new FlowLayout());
			add(btn);
		}
	}
	private Listing _listing;
	public SuffFileChooser(String btn, File dir) {
		super(dir);
		_btn = btn;
		setApproveButtonText(btn);
		setApproveButtonToolTipText(btn);
		setDialogTitle(btn);
		setDialogType(JFileChooser.SAVE_DIALOG);
		_listing = new Listing("Listing");
		setAccessory(_listing);
	}
	public SuffFileChooser(String btn, String sfx, String dsc, File dir) {
		super(dir);
		SuffFileFilter f = new SuffFileFilter(sfx, dsc);
		setFileFilter(f);
		_btn = btn;
		setApproveButtonText(btn);
		setApproveButtonToolTipText(btn);
		setDialogTitle(btn);
		setDialogType(JFileChooser.SAVE_DIALOG);
		_listing = new Listing("Listing");
		setAccessory(_listing);
	}
	public SuffFileChooser(String btn, String[] sfx, String[] dsc, File dir) {
		super(dir);
		SuffFileFilter f = new SuffFileFilter(sfx[0], dsc[0]);
		setFileFilter(f);
		for (int i = 1; i < dsc.length; ++i) {
			f = new SuffFileFilter(sfx[i], dsc[i]);
			addChoosableFileFilter(f);
		}
		_btn = btn;
		setApproveButtonText(btn);
		setApproveButtonToolTipText(btn);
		setDialogTitle(btn);
		setDialogType(JFileChooser.SAVE_DIALOG);
		_listing = new Listing("Listing");
		setAccessory(_listing);
	}
	public int showDialog(Component frame) {
		int rv = super.showDialog(frame, _btn);
		if (rv == JFileChooser.APPROVE_OPTION) {
			javax.swing.filechooser.FileFilter ff = getFileFilter();
			if (ff instanceof SuffFileFilter) {
				SuffFileFilter fi = (SuffFileFilter)getFileFilter();
				String sfx = "." + fi.getSuffix();
				if (getSelectedFile().getName().endsWith(sfx)) {
					return rv;
				}
				File f = new File(getSelectedFile().getAbsolutePath().concat(sfx));
				setSelectedFile(f);
			}
		}
		return rv;
	}
	public boolean wantListing() {
		return _listing.btn.getState();
	}
}
