// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.Arrays;

public class MOD1MSR {
	static final String name = "MOD1MSR";
	class Mod1File {
		public boolean free = true;
	}

	static final int NFILE = 16;
	HW2000 sys;
	P_Disk dsk;
	private int base = 0;
	private int numOps = 0;
	private int comm = 0;
	private int commLen = 0;

	Mod1File[] files;

	public MOD1MSR(HW2000 sys) {
		this.sys = sys;
		dsk = (P_Disk)sys.pdc.getPeriph(PeriphDecode.P_DK);;
		sys.SR += name.length();
		// TODO: set these values
		base = 0;
		comm = 0;
		numOps = 0;
		commLen = 0;

		files = new Mod1File[NFILE];
		for (int x = 0; x < files.length; ++x) {
			files[x] = new Mod1File();
		}
	}

	static public String name() { return name; }
	static public boolean check(HW2000 sys) {
		if ((sys.rawReadMem(sys.SR) & 0100) == 0 ||
			(sys.rawReadMem(sys.SR + name.length()) & 0100) == 0) {
			return false;
		}
		String s = "";
		for (int a = 0; a < name.length(); ++a) {
			s += sys.pdc.cvt.hwToLP((byte)(sys.rawReadMem(sys.SR + a) & 077));
		}
		return name.equals(s);
	}

	public boolean doTrap() {
		if (sys.SR < base || sys.SR - base >= numOps) {
			return false;
		}
		// TODO: extract params, perform operation...
		return true;
	}

	private int open(String file, int unit) {
		Mod1File f = null;
		int x;
		for (x = 0; x < files.length && !files[x].free; ++x);
		if (x >= files.length) {
			return -1;
		}
		// TODO: ... open 'file' on 'unit'...
		return x;
	}
}
