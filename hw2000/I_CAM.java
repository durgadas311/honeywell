public class I_CAM implements Instruction {
	// Change Address Mode
	public void execute(HW2000 sys) {
		if (sys.numXtra() > 0) {
			sys.CTL.setV(sys.getXtra(0));
		}
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
	}
}