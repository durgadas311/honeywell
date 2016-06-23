import java.awt.event.*;

public interface FrontPanel {
	public void setContents(int v);
	public void setAddress(int v);
	public void setControl(int v);
	public void setRunStop(boolean run);
	public void setAdrMode(int v);
	public void setInterrupt(boolean intr);	// Indicator only

	// Actions are:
	//	VK_R	Run
	//	VK_S	Stop
	//	VK_I	Initialize
	//	VK_B	Bootstrap
	//	VK_C	Central Clear
	//	VK_N	Instruct
	//	VK_2,VK_3,VK_4	Address Mode
	// TODO: how many of these are directly sent to core system?
	// (vs. being directly performed on core system object)
	//
	public void setPanelListener(ActionListener lstr);

	// Are these ever queried?
	public int getContents();
	public int getAddress();
	public int getControl();
	public boolean getRunStop();
	public int getAdrMode();
}
