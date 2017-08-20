// Copyright (c) 2011,2014 Douglas Miller

import java.io.*;

public interface AppManager {
	CardViewer getViewer();
	File getCardDir();
	void setCardDir(File dir);
	File getPanelDir();
	void setPanelDir(File dir);
	File getPaperDir();
	void setPaperDir(File dir);
	File getDrumDir();
	void setDrumDir(File dir);
}
