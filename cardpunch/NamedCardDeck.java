// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.io.*;

public class NamedCardDeck {
	public InputStream real;
	public String name;
	public int count;
	public NamedCardDeck(InputStream in, String nm, int num) {
		real = in;
		name = nm;
		count = num;
	}
}
