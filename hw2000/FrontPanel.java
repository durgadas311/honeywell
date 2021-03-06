// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
import java.awt.*;
import java.awt.event.*;

public interface FrontPanel {
	public void setContents(int v);
	public void setAddress(int v);
	public void setControl(int v);
	public void setRunStop(boolean run);
	public void setAdrMode(int v);
	public void setInterrupt(int type);	// Indicator only
	public void setProtect(int type);	// Indicator only
	public void setActive(boolean on);	// Indicator only
	public void setProgram(boolean on);	// Indicator only
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

	public void traceOut(String str); // Debug Trace output
	public void listOut(String str); // general output to LP
	public void doRun(); // Same as user clicking RUN
	public void doStop(); // Same as user clicking STOP
}
