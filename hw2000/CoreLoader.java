// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class CoreLoader implements Loader {
	private HW2000 sys;
	private FrontPanel fp;
	private String program;
	private String segment;
	private String revision;
	private long visibility;
	private LoaderMonitorC mon;

	public CoreLoader(CoreMemory sys, FrontPanel fp) {
		// TODO: fix this cast
		this.sys = (HW2000)sys;
		this.fp = fp;
	}

	public boolean begin(int adr, String prg, String seg) {
		program = prg;
		segment = seg;
		return true;
	}

	public boolean setCode(int adr, byte[] code) {
		for (int y = 0; y < code.length; ++y) {
			sys.rawWriteMem(adr + y, code[y]);
		}
		return true;
	}

	public boolean clear(int start, int end, byte fill) {
		for (int y = start; y <= end; ++y) {
			sys.rawWriteMem(y, fill);
		}
		return true;
	}

	public boolean range(int start, int end) {
		// Nothing for us. Assembler already tracks.
		return true;
	}

	// TODO: run? or just pause?
	public boolean exec(int start) {
		mon = new LoaderMonitorC(sys, program, segment);
		sys.addTrap(mon);
		sys.SR = start;
		fp.doRun();
		mon.waitReturn(0); // TODO: choose a timeout
		fp.doStop(); // 'mon' should have already done this
		sys.removeTrap(mon);
		return true;
	}

	public boolean segment(String prg, String seg) {
		program = prg;
		segment = seg;
		return true;
	}

	public boolean end(int start) {
		return true;
	}

	public int getError() { return 0; }

	// A public service...
	public void listOut(String str) {
		fp.listOut(str);
	}
}
