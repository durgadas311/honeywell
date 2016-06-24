import java.io.*;
import java.awt.event.*;
import javax.swing.*;

public class monrun implements ActionListener, Runnable {

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
	boolean suspend = false;
	boolean more = true;
	Thread thrd = null;

	public monrun(String[] argv) {
		args = argv;
		argx = 0;
		String mon = "monitor.ezc";
		fp = null;
		hw = new HW2000();
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
		if (fp != null) {
			hw.setFrontPanel(fp);
			fp.setPanelListener(this);
		} else {
			while (more) {
				run();
			}
		}
	}

	// return 'false' if no more to do..
	public void run() {
		if (suspend) {
			suspend = false;
			hw.run();
			return;
		}
		FileOutputStream lst = null;
		while (argx < args.length && args[argx].startsWith("-")) {
			if (args[argx].equals("-t")) {
				trace = true;
			} else if (args[argx].equals("-L")) {
				lists = true;
			}
			++argx;
		}
		if (argx >= args.length) {
			more = false;
			return;
		}

		if (lists) {
			// TODO: remove ".ezc"
			try {
				lst = new FileOutputStream(new File(args[argx] + ".lst"));
			} catch (Exception ee) {
				more = false;
				return;
			}
		}
		Assembler asm = new Assembler(new File(args[argx]));
		int e = asm.passOne();
		if (e < 0) {
			if (lst != null) {
				hw.listOut(lst, asm.getErrors());
			} else {
				System.err.println(asm.getErrors());
			}
			more = false;
			return;
		}
		int low = asm.getMin();
		int hi = asm.getMax();
		int start = asm.getStart();
		int reloc = 0;
		int brr = 0;
		int ibr = 0;
		if (started) {
			brr = 2; // TODO: manage memory and allocate space
			ibr = 2;
			reloc = (brr << 12);
		}
		e = asm.passTwo(hw, reloc, lst);
		if (e < 0) {
			if (lst != null) {
				hw.listOut(lst, asm.getErrors());
			} else {
				System.err.println(asm.getErrors());
			}
			more = false;
			return;
		}
		if (lst != null) {
			asm.listSymTab();
		}
		if (trace) {
			hw.setTrace(reloc + low, reloc + hi);
		}
		if (lst != null) {
			hw.listOut(lst, "Line Printer:\n");
			hw.pdc.setOutput(PeriphDecode.P_LP, lst);
		}
		if (!started) {
			// first program must be monitor... or else the only program to run.
			started = true;
			System.err.format("Running %s %07o %07o %07o\n", asm.getName(), low, hi, start);
			hw.SR = start;
		} else {
			System.err.format("Running %s via monitor %07o %07o %07o\n", asm.getName(), low, hi, start);
			hw.setField(0007, ibr);
			hw.setField(0005, brr);
			hw.setField(0003, start);
			hw.SR = hw.CSR;
		}
		hw.run();
		if (lst != null) {
			hw.listOut(lst, "\n");
			hw.pdc.setOutput(PeriphDecode.P_LP, null);
			hw.dumpHW(lst, reloc + low, reloc + hi - 1);
			try {
				lst.close();
			} catch (Exception ee) {}
		}
		hw.setTrace(0, 0);
		++argx;
		more = (argx < args.length);
	}

	public void actionPerformed(ActionEvent e) {
		if (!(e.getSource() instanceof JButton)) {
			return;
		}
		JButton btn = (JButton)e.getSource();
		String act = btn.getActionCommand();
		if (act.equals("run") && hw.halt) {
			thrd = new Thread(this);
			thrd.start();
		} else if (act.equals("stop")) {
			suspend = true;
			hw.halt = true;
		}
	}
}
