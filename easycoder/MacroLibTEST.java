// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// Usage:
//	"MAC   TEST" must be present after a valid ORG and ADMODE,
//		but not in any direct execution path (e.g. should be
//		before START).
//	"TESTI" (no parameters) must be in the program initialization code path.
//	"TEST  params..." will expand to calls to "the runtime".
//
// This example assumes a runtime named "TEST", but no such implementation exists.
// Therefore, this can be used to create code but that code cannot be executed.
//
public class MacroLibTEST implements MacroDef {
	Assembler asm;

	public MacroLibTEST(Assembler asm) {
		this.asm = asm;
		asm.assemble("       $TEST  DCW   #1B1");
	}

	public boolean handles(String name) {
		return (name.startsWith("TEST"));
	}

	public int expand(String name, String tag, String[] parms) {
		int ret = -1;
		if (name.equals("TEST")) {
			ret = test(tag, parms);
		} else if (name.equals("TESTI")) {
			ret = init(tag, parms);
		} else {
			asm.errsAdd("Unsupported macro " + name);
		}
		return ret;
	}

	private int init(String tag, String[] parms) {
		// Setup runtime library...
		int ret = asm.assemble(String.format("       %-7sB     0-1", tag));
		if (ret < 0) return ret;
		return asm.assemble("      R       DCW   @TEST@");
	}

	private int test(String tag, String[] parms) {
		int ret = asm.assemble(String.format("       %-7sB     $TEST", tag));
		if (ret < 0) return ret;
		int x;
		for (x = 0; x < parms.length - 1; ++x) {
			ret = asm.assemble(String.format("              DSA   %s", parms[x]));
			if (ret < 0) return ret;
		}
		return asm.assemble(String.format("      R       DSA   %s", parms[x]));
	}
}
