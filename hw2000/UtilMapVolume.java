// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.awt.*;
import javax.swing.*;

public class UtilMapVolume extends JPanel {
	HW2000 sys;
	int error;

	private JTextField map_lun;
	private JCheckBox cylmap_cb;
	private JCheckBox mmblst_cb;

	public UtilMapVolume(HW2000 sys) {
		super();
		this.sys = sys;
		error = 0;

		JPanel pn;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		map_lun = new JTextField("0");
		map_lun.setPreferredSize(new Dimension(20, 20));
		cylmap_cb = new JCheckBox("Cylinder Map");
		mmblst_cb = new JCheckBox("Member Lists");
		pn = new JPanel();
		pn.add(new JLabel("Disk Unit:"));
		pn.add(map_lun);
		add(pn);
		add(cylmap_cb);
		add(mmblst_cb);
	}

	public boolean perform() {
		error = 0;
		int unit;
		try {
			unit = Integer.valueOf(map_lun.getText());
			if (unit < 0 || unit > 7) {
				error = 00015;
				return false;
			}
		} catch (Exception ee) {
			error = 00015;
			return false;
		}
		P_Disk p = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);
		boolean ok = FileVolSupport.mapVolume(p, unit, sys,
			cylmap_cb.isSelected(), mmblst_cb.isSelected());
		error = FileVolSupport.getErrno();
		return ok;
	}

	public String getError() {
		return FileVolSupport.getError(error);
	}
}
