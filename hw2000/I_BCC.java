// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_BCC implements Instruction {
	public String mnem() { return "BCC"; }
	// Branch on Character Condition
	public void execute(HW2000 sys) {
		if (sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
		byte v = sys.CTL.getV();
		byte b = sys.readMem(sys.BAR);
		sys.incrBAR(-1);
		boolean taken = false;
		switch(v & 007) {
		case 000:
			taken = true;
			break;
		case 001:
			taken = ((b & 020) != 0);
			break;
		case 002:
			taken = ((b & 040) != 0);
			break;
		case 003:
		case 007:
			taken = ((b & 060) == 060);
			break;
		case 004:
			taken = ((b & 060) == 000);
			break;
		case 005:
			taken = ((b & 060) == 020);
			break;
		case 006:
			taken = ((b & 060) == 040);
			break;
		}
		switch(v & 070) {
		case 000:
			// taken = taken && true;
			break;
		case 010:
			taken = taken && ((b & 0100) != 0);
			break;
		case 020:
			taken = taken && ((b & 0200) != 0);
			break;
		case 030:
			taken = taken && ((b & 0300) == 0300);
			break;
		case 040:
			taken = taken && ((b & 0300) == 0000);
			break;
		case 050:
			taken = taken && ((b & 0300) == 0100);
			break;
		case 060:
			taken = taken && ((b & 0300) == 0200);
			break;
		case 070:
			// TODO: word WM set, or *only* WM set?
			taken = taken || ((b & 0100) != 0);
			break;
		}
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
		sys.addTics(2);
	}
}
