// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>

import java.util.Map;
import java.util.HashMap;

public class Errors {
	// param 40 = directory exit (01)
	// param 41 = index exit     (02)
	// param 42 = not used       (03)
	// param 43 = data exit      (04)
	// param 44 = device exit    (05)
	static private Map<Integer, String> errmsg;
	static {
		// This represents all documented EXITs, many are not used.
		errmsg = new HashMap<Integer, String>();
		errmsg.put(00005, "Invalid Action");	// <<<<< catch-all
		//
		// These are errors outside of MIOC (MOD1MSIO never uses).
		// Mainly used between FileVolSupport and HW2000FrontPanel.
		errmsg.put(00011, "Directory full");
		errmsg.put(00012, "Allocation conflict");
		errmsg.put(00013, "File exists");
		errmsg.put(00015, "Invalid Unit Number");
		errmsg.put(00016, "Invalid Allocation Unit");
		errmsg.put(00017, "Invalid File Parameters");
		errmsg.put(00020, "Missing Required Parameters");
		//
		// "Directory" errors - MSOPEN, MSCLOS
		errmsg.put(00101, "OPEN CALLOUT 1");
		errmsg.put(00103, "File Not Found");
		errmsg.put(00104, "U-A Table Too Small");
		errmsg.put(00105, "Error in *VOLALLOC*");
		errmsg.put(00111, "CLOSE CALLOUT");
		errmsg.put(00113, "Volume Sequence Number Error");
		errmsg.put(00114, "Password Error");	// password incorrect
		errmsg.put(00121, "Dismount Previous Volume");	// Open callout 2
		errmsg.put(00123, "File Type Unknown");	// NODOC
		errmsg.put(00124, "Password Error");	// no password given
		errmsg.put(00133, "Device Table Too Small");
		errmsg.put(00134, "Mount Next Volumes");
		errmsg.put(00143, "Volume Sequence Number Error");
		errmsg.put(00153, "No Data On File-Volume");
		// "Index" errors - Partitioned or Indexed files
		errmsg.put(00203, "Member Not Found");	// SETM
		errmsg.put(00213, "Member Not Found");	// MALTER
		errmsg.put(00204, "Member Index Full");
		errmsg.put(00214, "Member Cannot Be Output Only");
		errmsg.put(00224, "Member Cannot Be Deleted");
		// "Every Index Entry" - non-fatal/advisory
		errmsg.put(00301, "SETM CALLOUT");
		// "Data" errors -
		errmsg.put(00401, "End File (Input)");
		errmsg.put(00411, "End File (Output)");
		errmsg.put(00412, "MSINS CALLOUT 1");
		errmsg.put(00422, "MSINS CALLOUT 2");
		errmsg.put(00434, "No Space For New Member");
		errmsg.put(00403, "Item Not Found");
		errmsg.put(00413, "No Space To Insert Item");
		errmsg.put(00404, "Invalid Bucket");
		errmsg.put(00423, "No Space For Moved Item");
		errmsg.put(00414, "Key Verification Failure");
		errmsg.put(00424, "Duplicate Item");
		// "Device" (hardware) errors
		errmsg.put(00501, "Inoperable");
		errmsg.put(00502, "Inoperable");	// Protection violation
		errmsg.put(00503, "Positioning Error");	// seek error
		errmsg.put(00504, "Write Error");	// format violation or overflow
		errmsg.put(00505, "Positioning Error");	// record not found
		errmsg.put(00506, "Read Error");	// data xfer
		errmsg.put(00507, "Read Error");	// no data xfer
		errmsg.put(00510, "Write Error");
		errmsg.put(00511, "Miscellaneous");	// Track-linking record
		errmsg.put(00512, "Read Error");	// Track-Linking error
	}

	static public String getError(int err) {
		if (errmsg.containsKey(err)) {
			return errmsg.get(err);
		} else {
			return String.format("Unknown error %04o", err);
		}
	}
}
