// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.awt.*;
import java.io.*;
import javax.swing.*;

public class UtilBootstrapGen extends JPanel {
	HW2000 sys;
	int error;

	private JTextField bsg_lun;

	public UtilBootstrapGen(HW2000 sys) {
		super();
		this.sys = sys; // may be null

		JPanel pn;
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		bsg_lun = new JTextField("0");
		bsg_lun.setPreferredSize(new Dimension(20, 20));
		pn = new JPanel();
		pn.add(new JLabel("Disk Unit:"));
		pn.add(bsg_lun);
		add(pn);
	}

	public boolean perform() {
		error = 0;
		int unit;
		try {
			unit = Integer.valueOf(bsg_lun.getText());
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
		if (p.begin(unit)) try {
			InputStream r1 = getClass().getResourceAsStream("bringup/brfloader.out");
			InputStream r2 = getClass().getResourceAsStream("bringup/mod1loader.out");
			int n1 = r1.available();
			int n2 = r2.available();
			byte[] buf = new byte[n1 + n2];
			r1.read(buf, 0, n1);
			if ((buf[n1 - 1] & 0300) != 0) {
				--n1;
			}
			r2.read(buf, n1, n2);
			CoreMemory mb = new BufferMemory(buf);
			int r = 0;
			int x = 0;
			for (x = 0; x < n1 + n2; x += 250) {
				if (p.seekRecord(0, 0, r++) < 0) {
					break;
				}
				// This writes only 250 chars (fmt rec len) each time.
				if (!p.writeRecord(mb, x, -1)) {
					break;
				}
			}
			if (x >= n1 + n2) {
				ok = true;
			}
		} catch (Exception ee) {
		} finally {
			error = p.getError();
			p.end();
		}
		return ok;
	}

	public String getError() {
		return Errors.getError(error);
	}
}
