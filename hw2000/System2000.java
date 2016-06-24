public class System2000 {
	public static void main(String[] args) {
		// TODO: process options
		HW2000 hw = new HW2000();
		HW2000FrontPanel fp = new HW2000FrontPanel(hw);
		hw.setFrontPanel(fp);
	}
}
