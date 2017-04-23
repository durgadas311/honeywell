// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

public class CoreLoader implements Loader {
	CoreMemory sys;
	public CoreLoader(CoreMemory sys) {
		this.sys = sys;
	}

	public void begin(int adr, String prg, String seg, String rev, int vis) {
	}

	public void setCode(int adr, byte[] code) {
		for (int y = 0; y < code.length; ++y) {
			sys.rawWriteMem(adr + y, code[y]);
		}
	}

	public void clear(int start, int end, byte fill) {
		for (int y = start; y <= end; ++y) {
			sys.rawWriteMem(y, fill);
		}
	}

	// TODO: run? or just pause? Must setup "monitor" traps...
	public void exec(int start) {
	}

	public void segment(String prg, String seg, String rev, int vis) {
	}

	public void end(int start) {
	}
}
