// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
public class I_CAM implements Instruction {
	public String mnem() { return "CAM"; }
	// Change Address Mode
	public void execute(HW2000 sys) {
		byte v = sys.CTL.getV();
		sys.CTL.setS_MODE((v & 010) != 0);
		sys.CTL.setTRAP((v & 004) != 0);
		switch(v & 060) {
		case 020:
			sys.setAM(HW2000CCR.AIR_AM_2C);
			break;
		case 060:
			sys.setAM(HW2000CCR.AIR_AM_4C);
			break;
		default:
			sys.setAM(HW2000CCR.AIR_AM_3C);
			break;
		}
		sys.addTics(1);
	}
}
