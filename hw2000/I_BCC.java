public class I_BCC implements Instruction {
	// Branch on Character Condition
	public void execute(HW2000 sys) {
		if (sys.op_xtra.length > 0) {
			sys.CTL.setV(sys.op_xtra[0]);
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
			taken = taken && ((b & 100) != 0);
			break;
		case 020:
			taken = taken && ((b & 200) != 0);
			break;
		case 030:
			taken = taken && ((b & 300) == 300);
			break;
		case 040:
			taken = taken && ((b & 300) == 000);
			break;
		case 050:
			taken = taken && ((b & 300) == 100);
			break;
		case 060:
			taken = taken && ((b & 300) == 200);
			break;
		case 070:
			// TODO: word WM set, or *only* WM set?
			taken = taken || ((b & 100) != 0);
			break;
		}
		if (taken) {
			sys.BAR = sys.SR;
			sys.SR = sys.AAR;
		}
	}
}
