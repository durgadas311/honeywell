// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;

public class FortranLibCalls extends FortranLibrary implements RunTimeLibrary {
	Map<String, FortranLibFunc> libs;
	Map<String, FortranLibFunc> calls;

	public FortranLibCalls(FortranParser pars) {
		libs = new HashMap<String, FortranLibFunc>();
		calls = new HashMap<String, FortranLibFunc>();

		// System runtime calls - not callable from FORTRAN statements
		libs.put("$FEXIT", new FortranLibFunc(null, _EXIT,
					FortranOperand.VOID, 0, null));
		libs.put("$ACBOIO", new FortranLibFunc(null, _ACBOIO | (_ACBOIO_ << 8),
					FortranOperand.VOID, 0, null));
		libs.put("$ACBFPH", new FortranLibFunc(null, _ACBFPH,
					FortranOperand.VOID, 0, null));
		libs.put("$ACBFXP", new FortranLibFunc(null, _ACBFXP,
					FortranOperand.VOID, 0, null));
		libs.put("$ENDFIL", new FortranLibFunc(null, _ENDFIL,
					FortranOperand.VOID, 0, null));
		libs.put("$REWIND", new FortranLibFunc(null, _REWIND,
					FortranOperand.VOID, 0, null));
		libs.put("$BKSPAC", new FortranLibFunc(null, _BKSPAC,
					FortranOperand.VOID, 0, null));
		// FORTRAN builtin functions
		libs.put("EOF", new FortranLibFunc("EOF", EOF,
					FortranOperand.VOID, 1, null));
		libs.put("EOT", new FortranLibFunc("EOT", EOT,
					FortranOperand.VOID, 1, null));
		libs.put("AINT", new FortranLibFunc("AINT", AINT,
					FortranOperand.REAL, 1, null));
		libs.put("INT", new FortranLibFunc("INT", INT,
					FortranOperand.INTEGER, 1, null));
		libs.put("SQRT", new FortranLibFunc("SQRT", SQRT,
					FortranOperand.REAL, 1, null));
		libs.put("IAND", new FortranLibFunc("IAND", IAND,
					FortranOperand.INTEGER, 2, null));
		libs.put("IOR", new FortranLibFunc("IOR", IOR,
					FortranOperand.INTEGER, 2, null));
		libs.put("ICOMPL", new FortranLibFunc("ICOMPL", ICOMPL,
					FortranOperand.INTEGER, 1, null));
		libs.put("IEXCLR", new FortranLibFunc("IEXCLR", IEXCLR,
					FortranOperand.INTEGER, 2, null));
		libs.put("FLOAT", new FortranLibFunc("FLOAT", FLOAT,
					FortranOperand.REAL, 1, null));
		libs.put("IFIX", new FortranLibFunc("IFIX", IFIX,
					FortranOperand.INTEGER, 1, null));
		libs.put("ABS", new FortranLibFunc("ABS", ABS,
					FortranOperand.REAL, 1, null));
		libs.put("IABS", new FortranLibFunc("IABS", IABS,
					FortranOperand.INTEGER, 1, null));
		libs.put("ATAN", new FortranLibFunc("ATAN", ATAN,
					FortranOperand.REAL, 1, null));
		libs.put("ATAN2", new FortranLibFunc("ATAN2", ATAN2,
					FortranOperand.REAL, 1, null));
		libs.put("COS", new FortranLibFunc("COS", COS,
					FortranOperand.REAL, 1, null));
		libs.put("SIN", new FortranLibFunc("SIN", SIN,
					FortranOperand.REAL, 1, null));
		libs.put("TANH", new FortranLibFunc("TANH", TANH,
					FortranOperand.REAL, 1, null));
		libs.put("ALOG", new FortranLibFunc("ALOG", ALOG,
					FortranOperand.REAL, 1, null));
		libs.put("ALOG10", new FortranLibFunc("ALOG10", ALOG10,
					FortranOperand.REAL, 1, null));
		libs.put("AMOD", new FortranLibFunc("AMOD", AMOD,
					FortranOperand.REAL, 2, null));
		libs.put("MOD", new FortranLibFunc("MOD", MOD,
					FortranOperand.INTEGER, 2, null));
		libs.put("EXP", new FortranLibFunc("EXP", EXP,
					FortranOperand.REAL, 2, null));

		refLibFunc("$FEXIT");
		// TODO: include these only if needed...?
		refLibFunc("$ACBOIO");
		refLibFunc("$ACBFXP");
		refLibFunc("$ACBFPH");
	}

	public FortranSubprogram refLibFunc(String fnc) {
		if (calls.containsKey(fnc)) {
			return calls.get(fnc);
		}
		if (!libs.containsKey(fnc)) {
			return null;
		}
		FortranLibFunc ff = libs.get(fnc);
		calls.put(fnc, ff);
		return ff;
	}

	public void genLib(FortranParser pars) {
		pars.emit("  $FLIB  RESV  0");
		for (Map.Entry<String, FortranLibFunc> entry : calls.entrySet()) {
			FortranLibFunc ff = entry.getValue();
			String n = ff.name();
			if (n == null) {
				n = entry.getKey();
			}
			int f = ff.code();
			pars.emit(String.format("  %-7sDC    #1B%d", n, f & 077));
			f >>= 8;
			while (f > 0) {
				pars.emit(String.format("         DC    #1B%d", f & 077));
				f >>= 8;
			}
			ff.setRetVal(pars); // last-minute registration of variable
		}
		pars.emit("  $FLIB1 RESV  0");
		pars.emit("  $IOFLG DCW   #1B0");
		// TODO: more communications data
		pars.emit("  $FLIB2 RESV  0");
	}

	public void genCode(FortranParser pars) {
		pars.emit("         B     0-1");
		pars.emit(String.format("         DCW   @%s@", name));
		pars.emit("         DSA   $FLIB");
		pars.emit("         DSA   $FLIB1");
		pars.emit(" R       DSA   $FLIB2");
	}

	public void genExit(FortranParser pars) {
		pars.emit("         B     $FEXIT");
	}
}
