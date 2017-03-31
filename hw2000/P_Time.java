import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;

import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.Timer;

public class P_Time
		implements Peripheral, ActionListener {
	boolean isOn = false;
	boolean interrupt = false;
	Timer timer;
	HW2000 sys;

	public static SimpleDateFormat _timestamp =
		new java.text.SimpleDateFormat("HH:mm:ss.S");

	public P_Time() {
		timer = new Timer(500, this);
		// do not start timer until program asks for it.
	}

	public void setOutput(OutputStream dev) {
	}

	public OutputStream getOutput() {
		return null;
	}

	public void setInput(InputStream dev) {
	}

	public InputStream getInput() {
		return null;
	}

	public void reset() {
		// TODO
	}

	// I.e. Front Panel switch
	public void setInterrupt(HW2000 sys) {
	}

	public void visible(boolean on) {
	}


	// This device handles bot TOD and INTVL under same
	// device address. PDT is never issued on INTVL and
	// the only common PCB code is 10 "branch if busy"
	// and this device is never (?) busy in that context.
	public void io(RWChannel rwc) {
		// PDT, this must be for TOD.
		// Also, we satisfy the request now, so
		// there is never a BUSY status.
		// check input/output? or ignore?
		rwc.startCLC();
		Date dt = new Date();
		String tod = _timestamp.format(dt);
		for (int x = 0; x < 10; ++x) {
			byte c = rwc.sys.pdc.cvt.asciiToHw((byte)tod.charAt(x));
			rwc.writeChar(c);
			rwc.incrCLC();
		}
	}

	public void run(RWChannel rwc) {
		// only called for TOD - nothing to do.
	}

	public String input(HW2000 sys) { return null; }

	public void output(String s) { }

	public boolean busy(byte c2) {
		return false;
	}

	public boolean ctl(RWChannel rwc) {
		this.sys = rwc.sys;
		boolean branch = false;
		if (rwc.c3 == 010) {
			// never busy
		} else if ((rwc.c3 & 070) == 070) {
			// must be INTVL
			switch(rwc.c3 & 007) {
			case 000:
				// allow OFF
				timer.stop();
				break;
			case 001:
				// allow ON
				timer.start();
				break;
			case 003:
				// allow ON with interval
				int intvl = (rwc.c4 & 077) << 12;
				intvl |= (rwc.c5 & 077) << 6;
				intvl |= (rwc.c6 & 077);
				// TODO: what units are 'intvl'???
				// 0 - 262143 count,
				// using milliseconds:
				//     = 0 - 262.143 seconds (4.37 minutes).
				// using seconds:
				//     = 0 - 72.8 hours (3.03 days).
				// using (2uS) clock ticks (ATR):
				//     = 0 - 0.524 seconds.
				timer.stop();
				timer.setDelay(intvl); // use mS
				timer.setInitialDelay(intvl);
				timer.start();
				break;
			case 004:
			case 006:
				interrupt = false;
				break;
			case 005:
			case 007:
				if (interrupt) {
					branch = true;
				}
				break;
			}
		}
		return branch;
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof Timer)) {
			return;
		}
		interrupt = true;
		sys.CTL.setEI(HW2000CCR.EIR_PC);
	}
}
