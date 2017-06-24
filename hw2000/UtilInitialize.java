// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.awt.*;
import javax.swing.*;

public class UtilInitialize extends JPanel {
	HW2000 sys;
	CharConverter cvt;
	int error;

	private JTextField vol_lun;
	private JTextField vol_name;
	private JTextField vol_snum;

	public UtilInitialize(HW2000 sys) {
		super();
		this.sys = sys;
		cvt = sys.pdc.cvt;
		error = 0;

		JPanel pn;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		vol_lun = new JTextField("0");
		vol_lun.setPreferredSize(new Dimension(20, 20));
		pn = new JPanel();
		pn.add(new JLabel("Disk Unit:"));
		pn.add(vol_lun);
		add(pn);
		vol_name = new JTextField();
		vol_name.setPreferredSize(new Dimension(60, 20));
		pn = new JPanel();
		pn.add(new JLabel("Name:"));
		pn.add(vol_name);
		add(pn);
		vol_snum = new JTextField();
		vol_snum.setPreferredSize(new Dimension(60, 20));
		pn = new JPanel();
		pn.add(new JLabel("Serial:"));
		pn.add(vol_snum);
		add(pn);
	}

	public boolean perform() {
		error = 0;
		int unit;
		try {
			unit = Integer.valueOf(vol_lun.getText());
			if (unit < 0 || unit > 7) {
				error = 00015;
				return false;
			}
		} catch (Exception ee) {
			error = 00015;
			return false;
		}
		byte[] nm = cvt.hwString(vol_name.getText(), 6);
		byte[] sn = cvt.hwString(vol_snum.getText(), 6);
		P_Disk p = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);
		boolean ok = FileVolSupport.initVolume(p, unit, nm, sn);
		error = FileVolSupport.getErrno();
		return ok;
	}

	public String getError() {
		return Errors.getError(error);
	}
}
