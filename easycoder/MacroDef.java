// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

// Must have ctor "<macro-lib>(Assembler asm)",
// expand() makes callbacks to Assember.
public interface MacroDef {
	boolean handles(String name);
	int expand(String name, String tag, String[] parms);
}
