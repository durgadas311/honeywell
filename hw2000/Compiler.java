// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;

public interface Compiler {
	int compile(CoreMemory sys, boolean lst);
	int compile(File lst);
	int generate(File ezc, File lst);
	String getErrors();
}
