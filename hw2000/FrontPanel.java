import java.awt.*;
import java.awt.event.*;

public interface FrontPanel {
	public void setContents(int v);
	public void setAddress(int v);
	public void setControl(int v);
	public void setRunStop(boolean run);
	public void setAdrMode(int v);
	public void setInterrupt(int type);	// Indicator only
	public int getSense();

	// ActionCommands are:
	//	"run"	Run
	//	"stop"	Stop
	//	"init"	Initialize
	//	"boot"	Bootstrap
	//	"clear"	Central Clear
	//	"instr"	Instruct
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

	// From JFrame
	public Container getContentPane();
}
