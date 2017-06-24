// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.awt.*;
import javax.swing.*;

public class UtilDeallocate extends JPanel {
	HW2000 sys;
	CharConverter cvt;
	int error;

	private JTextField rel_lun;
	private JTextField rel_name;

	public UtilDeallocate(HW2000 sys) {
		super();
		this.sys = sys;
		cvt = sys.pdc.cvt;
		error = 0;

		JPanel pn;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		rel_lun = new JTextField("0");
		rel_lun.setPreferredSize(new Dimension(20, 20));
		pn = new JPanel();
		pn.add(new JLabel("Disk Unit:"));
		pn.add(rel_lun);
		add(pn);
		rel_name = new JTextField();
		rel_name.setPreferredSize(new Dimension(120, 20));
		pn = new JPanel();
		pn.add(new JLabel("Name:"));
		pn.add(rel_name);
		add(pn);
	}

	public boolean perform() {
		error = 0;
		int unit;
		try {
			unit = Integer.valueOf(rel_lun.getText());
			if (unit < 0 || unit > 7) {
				error = 00015;
				return false;
			}
		} catch (Exception ee) {
			error = 00015;
			return false;
		}
		byte[] nm = cvt.hwString(rel_name.getText(), 10);
		P_Disk p = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);
		boolean ok = FileVolSupport.releaseFile(p, unit, sys, nm);
		error = FileVolSupport.getErrno();
		return ok;
	}

	public String getError() {
		return Errors.getError(error);
	}
}
