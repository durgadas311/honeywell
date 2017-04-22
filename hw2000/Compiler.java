// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.util.Map;
import java.io.*;

public interface Compiler {
	int compile(CoreMemory sys, boolean lst);
	int compile(PrintStream lst);
	int generate(File ezc);
	String getErrors();
	boolean listEasyCoder();
	boolean listSymbols();
	boolean wantsDump();
	boolean hasData();
	int lineCount();
	BufferedReader getData();
	void listSymTab();
	Map<String, String> getSymTab();
}
