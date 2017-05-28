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

	public int expand(String mac, String tag, String[] parms) {
		int cmd = cmds.get(mac);
		int np = parms.length;
		int ret = -1;
		int mode = 0; // not MSOPEN
		String mca;
		switch (cmd) {
		case 0:
			// Invalid. should never happen.
			break;
		case 1:	// MIOC
			// Currently allows only one per program
			// ADMODE: parms.size() >= 29 ? parms[28] : "3"
			if (assemble(' ', "$MIOC", "DCW", "#1B0") < 0) break; // trap
			if (assemble(' ', "", "DCW", "#1B0") < 0) break; // ret from exits
			if (assemble(' ', "$MINIT", "DCW", "#1B1") < 0) break;
			if (assemble(' ', "$MIOCI", "SCR", "$MIOCZ,70") < 0) break;
			if (assemble(' ', "", "B", "0-1") < 0) break;
			if (assemble(' ', "", "DCW",
					"@" + MOD1MSIORunTime.name() + "@") < 0) break;
			if (assemble('R', "", "DSA", "$MIOC") < 0) break;
			if (assemble(' ', " $MIOCZ", "B", "0") < 0) break;
			// TODO: use this or not?
			if (assemble(' ', "$MIOCE", "SCR", "$MIOCY,70") < 0) break;
			if (assemble(' ', "", "B", "$MIOC") < 0) break;
			if (assemble(' ', " $MIOCY", "B", "0") < 0) break;
			ret = 0;
			break;
		case 2:	// MPIOC - TBD
			break;
		case 3:	// MCA
			if (tag == null || tag.isEmpty() || np < 20) {
				asm.errsAdd("Missing required MCA parameters");
				break;
			}
			// requires tag - file tag prefix, and parms[0] - MIOC char
			mca = "MCA" + tag;
			// NOTE! mac points to right-most char of string!
			if (assemble(' ', mca, "DCW", "#10A" + parms[19]) < 0) break;
			// NOTE! "mca+1" points to result/error return code...
			if (assemble(' ', "", "DCW", "#1B0") < 0) break;  // result code
			if (assemble(' ', "", "DCW", "#1A" + parms[1]) < 0) break;
			if (np >= 40 &&
				assemble(' ', "DIR" + tag, "DSA", parms[39]) < 0) break;
			if (np >= 43 &&
				assemble(' ', "DAT" + tag, "DSA", parms[42]) < 0) break;
			// TODO: terminate structure?
			ret = 0;
			break;
		case 4: // MSOPEN special handling
			mode = 1; // IN (only)
			if (np >= 2) {
				if (parms[1].equals("IN/OUT")) {
					mode = 3;
				} else if (parms[1].equals("OUT")) {
					mode = 2;
				}
			}
			// MSOPEN must be first call ever made...
			if (assemble(' ', tag, "BCE", "$MIOCI,$MINIT,01") < 0) break;
			// FALLTHROUGH
		default:
			mca = "MCA" + parms[0];
			if (assemble(' ', tag, "B", "$MIOC") < 0) break;
			if (assemble(' ', "", "DSA", mca) < 0) break;
			if (mode > 0) {
				if (assemble(' ', "", "DSA",
					String.format("%d", mode)) < 0) break;
			}
			if (assemble('R', "", "DCW",
					String.format("#1B%d", cmd)) < 0) break;
			ret = 0;
			break;
		}
		return ret;
	}
}
