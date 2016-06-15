public class Assembler {
	File inFile;
	BufferedReader in;
	FileOutputStream out;
	private Map<String,Integer> symTab;

	public Assembler(File input) {
		inFile = input;
		symTab = new HashMap<String,Integer>();
		try {
			in = new BufferedReader(new FileReader(inFile));
		} catch (Exception ee) {
			ee.printStackTrace();
			System.exit(1);
		}
	}

	public int passOne() {
		asmPass = false;
		int ret;
		currLoc = 0;
		while ((ret = scanOne()) >= 0) {
		}
		try {
			in.close();
		} catch (Exception ee) {}
	}

	public int passTwo(File output) {
		try {
			in = new BufferedReader(new FileReader(inFile));
			out = new FileOutputStream(output);
		} catch (Exception ee) {
			// 'in' should never fail - already validated in ctor.
			ee.printStackTrace();
			return -1;
		}
		asmPass = true;
		int ret;
		currLoc = 0;
		while ((ret = scanOne()) >= 0) {
		}
		try {
			in.close();
		} catch (Exception ee) {}
	}

	private int scanOne() {
		String line = "";
		try {
			line = in.readLine();
			if (line == null) {
				return -1;
			}
		} catch (Exception ee) {
			ee.printStackTrace();
			return -1;
		}
		// TODO: handle D data cards... C/L continuation and macro...
		String typ = line.substring(5, 6);
		if (typ.equals("*") || typ.equals("T")) {
			return 0;
		}
		String mrk = line.substring(6, 7);
		String loc;
		String opc;
		String opd;
		if (true) {
			loc = line.substring(7, 14);
			opc = line.substring(14, 20).trim();
			opd = line.substring(20);
		} else {
			loc = line.substring(7, 18);
			opc = line.substring(18, 24).trim();
			opd = line.substring(24);
		}
		boolean rev = false;
		if (loc.startsWith(" ")) {
			rev = true;
			loc = loc.substring(1).trim();
		} else {
			loc = loc.trim();
		}
		int e = opd.indexOf(' ');
		if (e >= 0) {
			opd = opd.substring(0, e).trim();
		}
		byte op = idc.getOp(opc);
		if (op != InstrDecode.OP_ILL) {
			e = processMachCode(op, loc, rev, opd);
		} else {
			e = processAsmDir(opc, mrk, loc, rev, opd);
		}
		return e;
	}

	private void setLabel(String loc, boolean set) {
		if (loc.length() > 0 && set) {
			symTab.put(loc, currLoc);
		}
	}

	private int processAsmDir(String opc, String mrk, String loc, boolean rev,
						String opd) {
		if (opc.equals("DCW")) {
			return processDefCon(mrk, loc, rev, opd, true);
		} else if (opc.equals("DC")) {
			return processDefCon(mrk, loc, rev, opd, false);
		} else if (opc.equals("RESV")) {
			return processResv(mrk, loc, rev, opd);
		} else if (opc.equals("DSA")) {
			return processDefSym(mrk, loc, rev, opd);
		} else if (opc.equals("DA")) {
			return processDefAre(mrk, loc, rev, opd);
		} else if (opc.equals("PROG")) {
		} else if (opc.equals("SEG")) {
		} else if (opc.equals("EX")) {
		} else if (opc.equals("XFR")) {
		} else if (opc.equals("ORG")) {
			if 
		} else if (opc.equals("MORG")) {
		} else if (opc.equals("LITORG")) {
		} else if (opc.equals("ADMODE")) {
		} else if (opc.equals("EQU")) {
		} else if (opc.equals("CEQU")) {
		} else if (opc.equals("SKIP")) {
		} else if (opc.equals("SFX")) {
		} else if (opc.equals("REP")) {
		} else if (opc.equals("GEN")) {
		} else if (opc.equals("SETLIN")) {
		} else if (opc.equals("XBASE")) {
		} else if (opc.equals("RANGE")) {
		} else if (opc.equals("CLEAR")) {
		} else if (opc.equals("END")) {
		}
	}

	private int processDefCon(String mrk, String loc, String rev, String opd, boolean mark) {
		setLabel(loc, !rev);
		int len = parseExpr(opd, mrk, mark);
		currLoc += len;
		setLabel(loc, rev);
		return len;
	}

	private int processResv(String mrk, String loc, String rev, String opd) {
		setLabel(loc, !rev);
		int len = parseVal(opd);
		currLoc += len;
		setLabel(loc, rev);
		return len;
	}

	private int processDefSym(String mrk, String loc, String rev, String opd) {
		setLabel(loc, !rev);
		int len = parseArg(opd, mark);
		currLoc += len;
		setLabel(loc, rev);
		return len;
	}

	// TODO: this requires more work... this is not right as-is.
	private int processDefAre(String mrk, String loc, String rev, String opd) {
		setLabel(loc, !rev);
		int len = parseArea(opd);
		currLoc += len;
		setLabel(loc, rev);
		return len;
	}
}
