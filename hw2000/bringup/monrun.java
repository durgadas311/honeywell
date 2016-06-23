import java.io.*;
import java.awt.event.*;
import javax.swing.*;

public class monrun implements ActionListener {

	public static void main(String[] args) {
		monrun x = new monrun(args);
	}

	String[] args;
	int argx;
	HW2000 hw;
	HW2000FrontPanel fp;
	boolean trace = false;
	boolean lists = false;
	boolean started = false;

	public monrun(String[] argv) {
		args = argv;
		argx = 0;
		String mon = "monitor.ezc";
		fp = null;
		while (argx < args.length && args[argx].startsWith("-")) {
			if (args[argx].equals("-t")) {
				trace = true;
			} else if (args[argx].equals("-f")) {
				fp = new HW2000FrontPanel(hw);
			} else if (args[argx].equals("-L")) {
				lists = true;
			}
			++argx;
		}
		hw = new HW2000();
		if (fp != null) {
			hw.setFrontPanel(fp);
			fp.setPanelListener(this);
		} else {
			while (go());
		}
	}

	// return 'false' if no more to do..
	private boolean go() {
		String lst = null;
		while (argx < args.length && args[argx].startsWith("-")) {
			if (args[argx].equals("-t")) {
				trace = true;
			} else if (args[argx].equals("-L")) {
				lists = true;
			}
			++argx;
		}
		if (argx >= args.length) {
			return false;
		}

		if (lists) {
			// TODO: remove ".ezc"
			lst = args[argx] + ".lst";
		}
		if (!started) {
			// first program must be monitor...
			started = true;
			hw.asmNGo(args[argx], lst, trace);
		} else {
			hw.monGo(args[argx], lst, trace);
		}
		++argx;
		return (argx < args.length);
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JButton)) {
			return;
		}
		JButton btn = (JButton)e.getSource();
		String act = btn.getActionCommand();
		if (act.equals("run") && hw.halt) {
			go();
		} else if (act.equals("stop")) {
			hw.halt = true;
		}
	}
}
