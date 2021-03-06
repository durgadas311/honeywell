// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Arrays;
import java.util.Vector;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

public class UtilExecutable extends JPanel
		implements ActionListener {
	HW2000 sys;
	CharConverter cvt;
	int error = 0;

	private JTextField xbl_lun;
	private ButtonGroup xbl_bg1;
	private JRadioButton xbl_brt;
	private JRadioButton xbl_brf;
	private ButtonGroup xbl_bg2;
	private JRadioButton xbl_res;
	private JRadioButton xbl_go;
	private JComboBox<String> xbl_act;
	private String[] xbl_cbo = new String[]{ "ADD", "REP", "DEL", "REN" };
	private JTextField xbl_pgm;
	private JTextField xbl_seg;
	private JTextField xbl_vis;
	private JTextField xbl_npg;
	private JTextField xbl_nsg;
	private JTextField xbl_nvs;

	CoreMemory blk;
	DiskVolume vol;
	byte[] pgm;
	byte[] seg;
	byte[] vis;
	byte[] npg;
	byte[] nsg;
	byte[] nvs;
	byte[] dst;
	byte[] src;

	public UtilExecutable(HW2000 sys) {
		super();
		this.sys = sys; // may be null
		cvt = sys.pdc.cvt;

		JPanel pn;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		xbl_lun = new JTextField("0");
		xbl_lun.setPreferredSize(new Dimension(20, 20));
		pn = new JPanel();
		pn.add(new JLabel("Disk Unit:"));
		pn.add(xbl_lun);
		add(pn);
		// TODO: allow selection of Tape Unit for BRT...
		xbl_bg1 = new ButtonGroup();
		xbl_brt = new JRadioButton("BRT");
		xbl_brf = new JRadioButton("BRF");
		xbl_brf.setSelected(true);
		xbl_bg1.add(xbl_brt);
		xbl_bg1.add(xbl_brf);
		xbl_bg2 = new ButtonGroup();
		xbl_res = new JRadioButton("RES");
		xbl_go = new JRadioButton("GO");
		xbl_res.setSelected(true);
		xbl_bg2.add(xbl_res);
		xbl_bg2.add(xbl_go);
		xbl_act = new JComboBox<String>(xbl_cbo);
		xbl_act.addActionListener(this);
		pn = new JPanel();
		pn.add(new JLabel("ACTION="));
		pn.add(xbl_act);
		add(pn);
		pn = new JPanel();
		pn.add(new JLabel("GO="));
		pn.add(xbl_brt);
		pn.add(xbl_brf);
		add(pn);
		pn = new JPanel();
		pn.add(new JLabel("DST="));
		pn.add(xbl_res);
		pn.add(xbl_go);
		add(pn);
		xbl_pgm = new JTextField();
		xbl_pgm.setPreferredSize(new Dimension(70, 20));
		xbl_seg = new JTextField();
		xbl_seg.setPreferredSize(new Dimension(20, 20));
		xbl_vis = new JTextField();
		xbl_vis.setPreferredSize(new Dimension(40, 20));
		xbl_npg = new JTextField();
		xbl_npg.setPreferredSize(new Dimension(70, 20));
		xbl_npg.setEnabled(false);
		xbl_nsg = new JTextField();
		xbl_nsg.setPreferredSize(new Dimension(20, 20));
		xbl_nsg.setEnabled(false);
		xbl_nvs = new JTextField();
		xbl_nvs.setPreferredSize(new Dimension(40, 20));
		xbl_nvs.setEnabled(false);
		pn = new JPanel();
		pn.add(new JLabel("PGM:"));
		pn.add(xbl_pgm);
		pn.add(new JLabel("SEG:"));
		pn.add(xbl_seg);
		pn.add(new JLabel("VIS:"));
		pn.add(xbl_vis);
		add(pn);
		pn = new JPanel();
		pn.add(new JLabel("NEW PGM:"));
		pn.add(xbl_npg);
		pn.add(new JLabel("SEG:"));
		pn.add(xbl_nsg);
		pn.add(new JLabel("VIS:"));
		pn.add(xbl_nvs);
		add(pn);
	}

	// ADD - Locate specific member/segment in source, create new in dest,
	//	copy.
	// REP - Locate specific member/segment in source, locate or create
	//	member in dest, copy.
	// REN - locate matching members in dest, rename each.
	// DEL - locate matching members in dest, delete each.

	private boolean isMatch(CoreMemory blk, int a) {
		if (!blk.compare(a, pgm, 0, 6)) {
			return false;
		}
		if (seg != null && !blk.compare(a + 6, seg, 0, 2)) {
			return false;
		}
		// TODO: compare, or mask?
		if (vis != null && !checkVis(blk, a + 8, vis)) {
			return false;
		}
		return true;
	}

	private CoreMemory copyKey(CoreMemory blk, int a) {
		CoreMemory k = new BufferMemory(14);
		blk.copyOut(a, k, 0, 14);
		return k;
	}

	private Vector<CoreMemory> allMatch(CoreMemory blk, DiskFile f) {
		Vector<CoreMemory> found = new Vector<CoreMemory>();
		int mode = DiskFile.IN; 
		while (f.setMemb(null, 0, mode)) {
			int a = f.getItemAdr();
			if (isMatch(blk, a)) {
				found.add(copyKey(blk, a));
			}
			mode = 011;	// continue
		}
		f.endMemb();
		return found;
	}

	// TODO: src might be Tape...
	private boolean doAdd(DiskFile dst, boolean add) {
		CoreMemory blk2 = new BufferMemory(250);
		DiskFile fi = vol.openFile(src, 0, blk2, 0, null, 0);
		if (fi == null) {
			return false;
		}
		try {
			Vector<CoreMemory> keys = allMatch(blk2, fi);
			if (keys.size() == 0) {
				error = 00403;
				return false;
			}
			for (CoreMemory key : keys) {
				if (!fi.setMemb(key, 0, DiskFile.IN)) {
					// should not be possible...
					return false;
				}
				if (add) {
					// fail if error != 00203?
					if (dst.setMemb(key, 0, DiskFile.IN)) {
						dst.endMemb();
						error = 00424;
						return false;
					}
					dst.endMemb();
				} else { // i.e. REP
					// ignore error? or fail if didn't exist?
					dst.alterMemb(key, 0, PartitionedSeqFile._DEL_,
								null, 0);
				}
				if (!dst.setMemb(key, 0, DiskFile.OUT)) {
					error = dst.getError();
					return false;
				}
				// This works for 1 item/block only...
				while (fi.getItem()) {
					if (!dst.putItem(blk2, 0)) {
						error = dst.getError();
						return false;
					}
				}
				// TODO: check fi error != 00401
				fi.endMemb();
				dst.endMemb();
			}
		} finally {
			fi.close();
		}
		return true;
	}

	private boolean doRen(DiskFile f, boolean ren) {
		while (f.setMemb(null, 0, 011)) {
			int a = f.getItemAdr();
			if (!isMatch(blk, a)) {
				continue;
			}
			// TODO: required permissions?
			//if (blk.readChar(a + 24) != PartitionedSeqFile._ALL_) {
			//}
			if (!ren) {
				blk.writeChar(a + 24, PartitionedSeqFile._DEL_);
			} else { // REN
				if (nsg != null) {
					blk.copyIn(a + 6, nsg, 0, 2);
				} else if (npg != null) {
					blk.copyIn(a, npg, 0, 6);
				}
				if (nvs != null) {
					blk.copyIn(a + 8, nvs, 0, 6);
				}
			}
			f.repItem(); // notify about dirty item
		}
		// TODO: error if none found?
		return true;
	}

	static public long visibility(String vis) {
		if (vis.equals("*")){
			return 0777777777777L;
		}
		long v = 0L;
		for (int x = 0; x < vis.length(); ++x) {
			char c = vis.charAt(x);
			if (c == ',' || c == ' ') {
				continue;
			}
			if (Character.isLetter(c)) {
				c -= 'A';
				v |= (1L << (35 - c));
			} else if (Character.isDigit(c)) {
				c -= '0';
				v |= (1L << (9 - c));
			} else {
				return -1;
			}
		}
		return v;
	}

	private byte[] visibility(long vis) {
		if (vis < 0) {
			return null;
		}
		byte[] vv = new byte[6];
		for (int y = 5; y >= 0; --y) {
			vv[y] = (byte)vis;
			vis >>= 6;
		}
		return vv;
	}

	private boolean checkVis(CoreMemory blk, int adr, byte[] vis) {
		for (int x = 0; x < 6; ++x) {
			if ((blk.readMem(adr + x) & vis[x] & 077) != 0) {
				return true;
			}
		}
		return false;
	}

	public boolean perform() {
		error = 0;
		int unit = 0;
		if (!xbl_lun.getText().isEmpty()) try {
			unit = Integer.valueOf(xbl_lun.getText());
			if (unit < 0 || unit > 7) {
				error = 00015;
				return false;
			}
		} catch (Exception ee) {
			error = 00015;
			return false;
		}
		P_Disk p = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);
		boolean ok = false;
		blk = new BufferMemory(250);
		vol = new DiskVolume(p, unit, cvt);
		DiskFile fi = null;
		pgm = null;
		seg = null;
		vis = null;
		npg = null;
		nsg = null;
		nvs = null;
		dst = cvt.hwString("*DRS1RES", 10);
		src = cvt.hwString("*DRS1GO", 10);
		if (xbl_go.isSelected()) {
			byte[] tmp = dst;
			dst = src;
			src = tmp;
		}
		String i = (String)xbl_act.getSelectedItem();
		if (xbl_pgm.getText().isEmpty()) {
			error = 00020;
			return false;
		}
		pgm = cvt.hwString(xbl_pgm.getText(), 6);
		if (!xbl_seg.getText().isEmpty()) {
			seg = cvt.hwString(xbl_seg.getText(), 2);
		}
		if (!xbl_vis.getText().isEmpty()) {
			vis = visibility(visibility(xbl_vis.getText()));
		}
		if (i.equals("REN")) {
			if (!xbl_npg.getText().isEmpty()) {
				npg = cvt.hwString(xbl_npg.getText(), 6);
			}
			if (!xbl_nsg.getText().isEmpty()) {
				nsg = cvt.hwString(xbl_nsg.getText(), 2);
			}
			if (!xbl_nvs.getText().isEmpty()) {
				nvs = visibility(visibility(xbl_nvs.getText()));
			}
			// Validate REN params...
			// program unit (pgm && !seg), then require npg && !nsg.
			// segment unit (pgm && seg), then require !npg && (nsg || nvs).
			//                                         (or ignore npg...)
			if (seg == null) { // program unit, all segments
				if (npg == null || nsg != null) {
					error = 00020;
					return false;
				}
			} else {	// segment unit, one segment only
				if (npg != null || (nsg == null && nvs == null)) {
					error = 00020;
					return false;
				}
			}
		}
		try {
			if (!vol.mount()) {
				return false;
			}
			fi = vol.openFile(dst, DiskFile.UPDATE, blk, 0, null, 0);
			if (fi == null) {
				return false;
			}
			if (i.equals("ADD") || i.equals("REP")) {
				return doAdd(fi, i.equals("ADD"));
			} else {
				return doRen(fi, i.equals("REN"));
			}
		} finally {
			if (fi != null) {
				if (error == 0) {
					error = fi.getError();
				}
				fi.close();
			} else {
				if (error == 0) {
					error = vol.getError();
				}
			}
			vol.unmount();
		}
	}

	public String getError() {
		return Errors.getError(error);
	}

	public void actionPerformed(ActionEvent e) {
		if ((e.getSource() instanceof JComboBox)) {
			JComboBox cb = (JComboBox)e.getSource();
			String i = (String)cb.getSelectedItem();
			Boolean on = i.equals("REN");
			xbl_npg.setEnabled(on);
			xbl_nsg.setEnabled(on);
			xbl_nvs.setEnabled(on);
			return;
		}
	}
}
