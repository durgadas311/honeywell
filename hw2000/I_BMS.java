public class I_BMS implements Instruction {
	// Binary Mantissa Shift

	public void execute(HW2000 sys) {
		if (sys.numXtra() != 2) {
			throw new FaultException("BMS malformed");
		}
		byte x = (byte)(sys.getXtra(0) & 070);
		byte m = (byte)(sys.getXtra(0) & 007);
		int sh = (sys.getXtra(1) & 077);

		if (sh == 0) {
			return;
		}

		boolean rot = ((m & 04) == 0);
		boolean left = ((m & 01) == 0);
		boolean sngl = ((m & 02) == 0);

		long mant = I_FMA.nativeToMant(sys.AC[x], sys.denorm[x]);
		// mant is fully sign-extended!
		boolean minus = (mant < 0);
		long lor = 0;
		if (!left) {
			mant <<= 28;
		}
		// shift in increments of 20 to avoid fall-off.
		// TODO: double-precision
		while (sh > 0) {
			int s = (sh > 20 ? 20 : sh);
			if (left) {
				mant <<= s; // sign lost, might need repair
				if (rot) {
					// don't include sign bit...
					long ov = mant & (((1 << s) - 1) << 35);
					mant |= (ov >> 35);
				}
			} else if (rot) {
				mant >>>= s; // sign lost, as intended
				long ov = mant & (((1 << s) - 1) << (28 - s));
				mant |= (ov << (64 - s));
			} else {
				mant >>= s; // sign preserved...
			}
			sh -= s;
		}
		if (left) {
			// sign repair may be required
			if (!rot) {
				mant &= 0x07ffffffffL; // clear garbage out
				if (minus) {
					mant |= ~0x07ffffffffL;
				}
			}
		} else {
			// no sign repair
			mant >>= 28;
		}
		sys.AC[x] = I_FMA.mergeMant(sys.AC[x], mant);
		sys.denorm[x] = ((mant & 0x400000000L) == 0);
	}
}
