// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Map;
import java.util.TreeMap;
import java.util.Properties;
import java.awt.*;
import java.io.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.lang.reflect.Constructor;

public class UtilAllocate extends JPanel
		implements ActionListener {
	HW2000 sys;
	CharConverter cvt;
	int error = 0;

	private ButtonGroup file_bg;
	private JRadioButton file_seq;
	private JRadioButton file_par;
	private JRadioButton file_idx;
	private JRadioButton file_dir;
	private JTextField file_lun;
	private JTextField file_name;
	private JTextField file_itm;
	private JTextField file_rec;
	private JTextField file_blk;
	private JTextField file_rpt;
	private JTextField file_bpx;
	private JCheckBox file_a;
	private JCheckBox file_b;
	private JTextField[][] file_unt;

	public UtilAllocate(HW2000 sys) {
		super();
		this.sys = sys;
		cvt = sys.pdc.cvt;
		JPanel pn;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		file_lun = new JTextField("0");
		file_lun.setPreferredSize(new Dimension(20, 20));
		pn = new JPanel();
		pn.add(new JLabel("Disk Unit:"));
		pn.add(file_lun);
		add(pn);
		file_name = new JTextField();
		file_name.setPreferredSize(new Dimension(120, 20));
		pn = new JPanel();
		pn.add(new JLabel("Name:"));
		pn.add(file_name);
		add(pn);
		file_bg = new ButtonGroup();
		// TODO: alter rest of panel based on changes to these...
		file_seq = new JRadioButton("Sequential");
		file_par = new JRadioButton("Partitioned Sequential");
		file_idx = new JRadioButton("Indexed Sequential");
		file_dir = new JRadioButton("Direct Access");
		file_bg.add(file_seq);
		file_bg.add(file_par);
		file_bg.add(file_idx);
		file_bg.add(file_dir);
		file_seq.setSelected(true);
		add(file_seq);
		add(file_par);
		add(file_idx);
		add(file_dir);
		file_a = new JCheckBox("A-file protection");
		add(file_a);
		file_b = new JCheckBox("B-file protection");
		add(file_b);
		file_itm = new JTextField();
		file_itm.setPreferredSize(new Dimension(60, 20));
		pn = new JPanel();
		pn.add(new JLabel("Item Len:"));
		pn.add(file_itm);
		add(pn);
		file_rec = new JTextField();
		file_rec.setPreferredSize(new Dimension(60, 20));
		pn = new JPanel();
		pn.add(new JLabel("Rec Len:"));
		pn.add(file_rec);
		add(pn);
		// TODO: dynamically compute defaults for Rec/Blk and Rec/Trk
		file_rpt = new JTextField();
		file_rpt.setPreferredSize(new Dimension(60, 20));
		pn = new JPanel();
		pn.add(new JLabel("Rec/Trk:"));
		pn.add(file_rpt);
		add(pn);
		file_blk = new JTextField();
		file_blk.setPreferredSize(new Dimension(60, 20));
		pn = new JPanel();
		pn.add(new JLabel("Rec/Blk:"));
		pn.add(file_blk);
		add(pn);
		file_bpx = new JTextField();
		file_bpx.setPreferredSize(new Dimension(60, 20));
		pn = new JPanel();
		pn.add(new JLabel("Blk/Idx:"));
		pn.add(file_bpx);
		add(pn);
		add(new JLabel("Allocation Units:"));
		// TODO: allocation units...
		file_unt = new JTextField[6][];
		for (int x = 0; x < 6; ++x) {
			file_unt[x] = new JTextField[4];
			pn = new JPanel();
			file_unt[x][0] = new JTextField();
			file_unt[x][0].setPreferredSize(new Dimension(30, 20));
			pn.add(file_unt[x][0]);
			file_unt[x][1] = new JTextField();
			file_unt[x][1].setPreferredSize(new Dimension(30, 20));
			pn.add(file_unt[x][1]);
			pn.add(new JLabel(" - "));
			file_unt[x][2] = new JTextField();
			file_unt[x][2].setPreferredSize(new Dimension(30, 20));
			pn.add(file_unt[x][2]);
			file_unt[x][3] = new JTextField();
			file_unt[x][3].setPreferredSize(new Dimension(30, 20));
			pn.add(file_unt[x][3]);
			add(pn);
		}
	}

	private boolean getAlloc(DiskUnit[] units, int nCyl, int nTrk) {
		for (int x = 0; x < 6; ++x) {
			if (file_unt[x][0].getText().isEmpty()) {
				break;
			}
			int sc, st, ec, et;
			try {
				sc = Integer.valueOf(file_unt[x][0].getText());
				st = Integer.valueOf(file_unt[x][1].getText());
				ec = Integer.valueOf(file_unt[x][2].getText());
				et = Integer.valueOf(file_unt[x][3].getText());
				if (sc < 0 || sc >= nCyl || st < 0 || st >= nTrk ||
					ec < 0 || ec >= nCyl || et < 0 || et >= nTrk) {
					throw new NumberFormatException("Out of range");
				}
			} catch (Exception ee) {
				return false;
			}
			units[x] = new DiskUnit(sc, st, ec, et);
		}
		return true;
	}

	public boolean perform() {
		error = 0;
		int unit = 0;
		if (!file_lun.getText().isEmpty()) try {
			unit = Integer.valueOf(file_lun.getText());
			if (unit < 0 || unit > 7) {
				error = 00015;
				return false;
			}
		} catch (Exception ee) {
			error = 00015;
			return false;
		}
		int type = -1;
		if (file_seq.isSelected()) {
			type = DiskFile.SEQUENTIAL;
		} else if (file_par.isSelected()) {
			type = DiskFile.PART_SEQ;
		} else if (file_idx.isSelected()) {
			type = DiskFile.INDEXED_SEQ;
		} else if (file_dir.isSelected()) {
			type = DiskFile.DIRECT;
		} else {
			// Can't happen with radio buttons?
			error = 00123;
			return false;
		}
		P_Disk p = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);
		byte[] nm = cvt.hwString(file_name.getText(), 10);
		int itmLen = -1;
		int recLen = -1;
		int recTrk = -1;
		int recBlk = -1;
		int blkIdx = -1;
		try {
			if (file_rec.getText().isEmpty()) {
				recLen = DiskVolume.def_reclen;
			} else {
				recLen = Integer.valueOf(file_rec.getText());
			}
			if (file_itm.getText().isEmpty()) {
				itmLen = recLen;
			} else {
				itmLen = Integer.valueOf(file_itm.getText());
			}
			if (file_rpt.getText().isEmpty()) {
				recTrk = p.numRecords(recLen);
			} else {
				recTrk = Integer.valueOf(file_rpt.getText());
			}
			if (file_blk.getText().isEmpty()) {
				for (recBlk = 1; recBlk < recTrk; ++recBlk) {
					if (((recBlk * recLen) % itmLen) == 0) {
						break;
					}
				}
				if (recBlk >= recTrk) {
					throw new NumberFormatException("Overflow");
				}
			} else {
				recBlk = Integer.valueOf(file_blk.getText());
			}
			// TODO: ignore if not partitioned sequential
			if (file_bpx.getText().isEmpty()) {
				blkIdx = 1;
			} else {
				blkIdx = Integer.valueOf(file_bpx.getText());
			}
		} catch (Exception ee) {
			error = 00017;
			return false;
		}
		int flag = (file_a.isSelected() ? p.PERMIT_A : 0) |
				(file_b.isSelected() ? p.PERMIT_B : 0);
		DiskUnit[] units = new DiskUnit[6];
		boolean ok = getAlloc(units, p.numCylinders(), p.numTracks());
		if (!ok) {
			error = 00016;
			return false;
		}
		ok = FileVolSupport.initFile(p, unit, flag, nm, type,
				itmLen, recLen, recTrk, recBlk,
				blkIdx, units);
		error = FileVolSupport.getErrno();
		return ok;
	}

	public String getError() {
		return FileVolSupport.getError(error);
	}

	public void actionPerformed(ActionEvent e) {
	}
}
