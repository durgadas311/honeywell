// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Arrays;
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
	private ButtonGroup xbl_bg;
	private JRadioButton xbl_brt;
	private JRadioButton xbl_brf;
	private JComboBox<String> xbl_act;
	private String[] xbl_cbo = new String[]{ "ADD", "REP", "DEL", "REN" };
	private JTextField xbl_pgm;
	private JTextField xbl_seg;
	private JTextField xbl_vis;
	private JTextField xbl_npg;
	private JTextField xbl_nsg;
	private JTextField xbl_nvs;

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
		xbl_bg = new ButtonGroup();
		xbl_brt = new JRadioButton("BRT");
		xbl_brf = new JRadioButton("BRF");
		xbl_brf.setSelected(true);
		xbl_bg.add(xbl_brt);
		xbl_bg.add(xbl_brf);
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

	private byte[] visibility(String vis) {
		byte[] vv = new byte[6];
		if (vis.equals("*")){
			Arrays.fill(vv, (byte)077);
			return vv;
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
				return null;
			}
		}
		for (int y = 5; y >= 0; --y) {
			vv[y] = (byte)v;
			v >>= 6;
		}
		return vv;
	}

	private byte[] hwString(String str, int len) {
		byte[] h = new byte[len];
		int x = 0;
		for (byte c : str.getBytes()) {
			h[x++] = cvt.asciiToHw(c);
			if (x >= len) break;
		}
		while (x < len) {
			h[x++] = 015;
		}
		return h;
	}

	public boolean perform() {
		error = 0;
		int unit = 0;
		if (!xbl_lun.getText().isEmpty()) try {
			unit = Integer.valueOf(xbl_lun.getText());
			if (unit < 0 || unit > 7) {
				error = 00501;
				return false;
			}
		} catch (Exception ee) {
			error = 00501;
			return false;
		}
		P_Disk p = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);
		boolean ok = false;
		CoreMemory blk = new BufferMemory(250);
		DiskVolume vol = new DiskVolume(p, unit);
		DiskFile fi = null;
		byte[] pgm = null;
		byte[] seg = null;
		byte[] vis = null;
		pgm = hwString(xbl_pgm.getText(), 6);
		if (!xbl_seg.getText().isEmpty()) {
			seg = hwString(xbl_seg.getText(), 2);
		}
		if (!xbl_vis.getText().isEmpty()) {
			vis = visibility(xbl_vis.getText());
		}
		try {
			if (!vol.mount()) {
				return false;
			}
			fi = vol.openFile(hwString("*DRS1RES", 10), DiskFile.UPDATE,
							blk, 0, null, 0);
			if (fi == null) {
				return false;
			}
			// TODO: ADD is different...
			while (fi.setMemb(null, 0, 011)) {
				int a = fi.getItemAdr();
				if (!blk.compare(a, pgm, 0, 6)) {
					continue;
				}
				if (seg != null &&
					!blk.compare(a + 6, seg, 0, 2)) {
					continue;
				}
				// TODO: compare, or mask?
				if (vis != null &&
					!blk.compare(a + 8, vis, 0, 6)) {
					continue;
				}
			}
		} finally {
			if (fi != null) {
				error = fi.getError();
				fi.close();
			} else {
				error = vol.getError();
			}
			vol.unmount();
		}
		return ok;
	}

	public String getError() {
		return FileVolSupport.getError(error);
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
