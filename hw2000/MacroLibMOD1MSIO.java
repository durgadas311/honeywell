// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;

// Usage:
//	"MAC   MOD1MSIO" must be present after a valid ORG and ADMODE,
//		but not in any direct execution path (e.g. should be
//		before START).
//	"MIOC  params..." must be in the program initialization code path.
//	"MCA   params..." one for each file to be used. Outside normal code path.
//	"MSxxx params..." will expand to calls to "the runtime".
//
public class MacroLibMOD1MSIO implements MacroDef {
	Assembler asm;

	static Map<String, Integer> cmds;
	static {
		cmds = new HashMap<String, Integer>();
		cmds.put("MIOC", 1);
		cmds.put("MPIOC", 2);
		cmds.put("MCA", 3);
		cmds.put("MSOPEN", 4);
		cmds.put("MSCLOS", 5);
		cmds.put("MSGET", 6);
		cmds.put("MSREP", 7);
		cmds.put("MSPUT", 8);
		cmds.put("SETM", 9);
		cmds.put("ENDM", 10);
		cmds.put("MALTER", 11);
		cmds.put("MSREL", 12);
		// These do not produce a call to runtime
		cmds.put("MUCA", 20);
		cmds.put("MLCA", 21);
	}

	public MacroLibMOD1MSIO(Assembler asm) {
		this.asm = asm;
		// TODO: how to exit/cleanup...
	}

	private int assemble(char pun, String tag, String opc, String opds) {
		return asm.assemble(String.format("      %c%-7s%-6s%s",
						pun, tag, opc, opds));
	}

	public boolean handles(String mac) {
		return (cmds.containsKey(mac));
	}

	private String defParm(String[] parms, int x, String def) {
		if (x >= parms.length || parms[x].isEmpty()) {
			return def;
		}
		return parms[x];
	}

	public int expand(String mac, String tag, String[] parms) {
		int cmd = cmds.get(mac);
		int np = parms.length;
		int ret = -1;
		int x;
		int mode = 0;
		String mca;
		switch (cmd) {
		case 0:
			// Invalid. should never happen.
			break;
		case 1:	// MIOC
			// Currently allows only one per program
			// ADMODE: parms.size() >= 29 ? parms[28] : "3"
			// Operator: parms.size() >= 30 ? parms[29] : " " // "220"/""
			// 50-56: MPIOC call
			if (assemble(' ', "$MIOC", "DCW", "#1B0") < 0) break; // trap
			if (assemble(' ', "", "DCW", "#1B0") < 0) break; // ret from exits
			if (assemble(' ', "$MINIT", "DCW", "#1B1") < 0) break;
			if (assemble(' ', "$MIOCI", "SCR", "$MIOCZ,70") < 0) break;
			if (assemble(' ', "", "B", "0-1") < 0) break;
			if (assemble(' ', "", "DCW",
					"@" + MOD1MSIORunTime.name() + "@") < 0) break;
			if (assemble(' ', "", "DSA", "$MIOC") < 0) break;
			if (assemble('R', "", "DSA", "$VBUF") < 0) break;
			if (assemble(' ', "", "BS", "$MINIT") < 0) break;
			if (assemble(' ', " $MIOCZ", "B", "0") < 0) break;
			if (assemble(' ', "", "NOP","") < 0) break;
			if (assemble(' ', " $VBUF", "RESV,",
				String.format("%d", DiskVolume.descrItmLen)) < 0) break;
			if (assemble('R', "", "DCW", "#1A") < 0) break;
			ret = 0;
			break;
		case 2:	// MPIOC - TBD
			break;
		case 3:	// MCA
			if (tag == null || tag.isEmpty() ||
					np < 20 || parms[19].isEmpty() ||
					parms[9].isEmpty() || parms[12].isEmpty()) {
				asm.errsAdd("Missing required MCA parameters");
				break;
			}
			mode = 0;	// LOCATE
			if (parms[11].equals("MOVE")) {
				mode = 1;
			}
			// requires tag - file tag prefix, and parms[0] - MIOC char
			mca = "MCA" + tag;
			if (assemble(' ', mca, "RESV", "0") < 0) break;
			if (assemble(' ', "FID" + tag, "DCW", "#10A" + parms[19]) < 0) break;
			if (assemble(' ', "", "DCW", "#1B0") < 0) break;  // result code
			if (assemble(' ', "", "DCW", "#1A" + parms[0]) < 0) break;
			if (assemble(' ', "", "DCW",	// Protection
					"#1C" + defParm(parms, 30, "00")) < 0) break;
			if (assemble(' ', "", "DCW",	// Item delivery mode
					String.format("#1B%d", mode)) < 0) break;
			if (assemble(' ', "", "DSA", defParm(parms, 1, "0")) < 0) break;
			if (assemble(' ', "PBL" + tag, "DSA",
						defParm(parms, 9, "0")) < 0) break;
			// TODO: alternate buffer?
			if (assemble(' ', "CBL" + tag, "DSA",
						defParm(parms, 12, "0")) < 0) break;
			// EXITs
			if (assemble(' ', "", "DSA", defParm(parms, 39, "0")) < 0) break;
			if (assemble(' ', "", "DSA", defParm(parms, 40, "0")) < 0) break;
			// [41] reserved
			if (assemble(' ', "", "DSA", defParm(parms, 42, "0")) < 0) break;
			if (assemble(' ', "", "DSA", defParm(parms, 43, "0")) < 0) break;
			// Auxiliary fields (not MCA params)
			if (assemble(' ', "APD" + tag, "DSA", "0") < 0) break;
			if (assemble(' ', "CAD" + tag, "DCW", "#8B0") < 0) break;
			if (assemble(' ', "RIC" + tag, "DCW", "#10B0") < 0) break;
			// Modern extensions
			if (assemble(' ', "VNM" + tag, "DCW", "#6A") < 0) break;
			if (assemble(' ', "VSN" + tag, "DCW", "#6A") < 0) break;
			// TODO: terminate structure?
			ret = 0;
			break;
		case 20: // MUCA
			if (np < 3 || (np & 1) == 0) {
				asm.errsAdd("Missing required MUCA parameters");
				break;
			}
			for (x = 1; x < np; x += 2) {
				if (assemble(' ', tag, "EXM", parms[x + 1] + parms[0] +
						"," + parms[x] + ",21") < 0) break;
			}
			if (x < np) {
				break;
			}
			ret = 0;
			break;
		case 21: // MLCA
			if (np < 3 || (np & 1) == 0) {
				asm.errsAdd("Missing required MLCA parameters");
				break;
			}
			// TODO: check for valid fields
			for (x = 1; x < np; x += 2) {
				if (assemble(' ', tag, "EXM", parms[x] + "," +
					parms[x + 1] + parms[0] + ",21") < 0) break;
			}
			if (x < np) {
				break;
			}
			ret = 0;
			break;
		default:
			if (!doFileTag(cmd, tag, parms)) {
				break;
			}
			ret = 0;
			break;
		}
		return ret;
	}

	private boolean doFileTag(int cmd, String tag, String[] parms) {
		int np = parms.length;
		int ret = -1;
		int mode = 0; // not MSOPEN
		int x;
		String mca;
		mca = "MCA" + parms[0];
		if (cmd == 4) { // MSOPEN must be first call ever made...
			if (assemble(' ', tag, "BCE", "$MIOCI,$MINIT,01") < 0) return false;
			tag = "";
		}
		if (assemble(' ', tag, "B", "$MIOC") < 0) return false;
		if (assemble(' ', "", "DSA", mca) < 0) return false;
		switch (cmd) {
		case 4: // MSOPEN special handling
			mode = 0;
			if (np >= 2 && !parms[1].isEmpty()) {
				if (parms[1].equals("IN")) {
					mode = DiskFile.IN;
				} else if (parms[1].equals("IN/OUT")) {
					mode = DiskFile.IN_OUT;
				} else if (parms[1].equals("OUT")) {
					mode = DiskFile.OUT;
				} else if (parms[1].equals("UPDATE")) {
					mode = DiskFile.UPDATE;
				} else {
					asm.errsAdd("Invalid MSOPEN mode value");
					break;
				}
			}
			if (assemble(' ', "", "DSA",
					String.format("%d", mode)) < 0) break;
			ret = 0;
			break;
		case 9: // SETM file,member,mode special handling
			if (np != 3 || parms[1].isEmpty() || parms[2].isEmpty()) {
				asm.errsAdd("Missing required SETM parameters");
				break;
			}
			mode = DiskFile.IN;
			if (!parms[2].isEmpty()) {
				if (parms[2].equals("IN")) {
					//
				} else if (parms[2].equals("IN/OUT")) {
					mode = DiskFile.IN_OUT;
				} else if (parms[2].equals("OUT")) {
					mode = DiskFile.OUT;
				} else {
					asm.errsAdd("Invalid SETM mode value");
					break;
				}
			}
			if (assemble(' ', "", "DSA", parms[1]) < 0) break;
			if (assemble(' ', "", "DSA",
					String.format("%d", mode)) < 0) break;
			ret = 0;
			break;
		case 11: // MALTER special handling
			if (np < 3 || np > 4 || parms[1].isEmpty() ||
					(parms[2].isEmpty() && parms[3].isEmpty())) {
				asm.errsAdd("Missing required MALTER parameters");
				break;
			}
			if (!parms[2].isEmpty()) {
				if (parms[2].equals("AVAIL")) {
					mode = 1;
				} else if (parms[2].equals("UNAVAIL")) {
					mode = 2;
				} else if (parms[2].equals("DELETE")) {
					mode = 3;
				} else {
					asm.errsAdd("Invalid MALTER status value");
					break;
				}
			}
			if (assemble(' ', "", "DSA", parms[1]) < 0) break;
			if (assemble(' ', "", "DSA",
					String.format("%d", mode)) < 0) break;
			if (assemble(' ', "", "DSA", defParm(parms, 3, "0")) < 0) break;
			ret = 0;
			break;
		default:
			ret = 0;
			break;
		}
		if (assemble('R', "", "DCW",
				String.format("#1B%d", cmd)) < 0) return false;
		return ret == 0;
	}
}
