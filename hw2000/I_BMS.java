// Copyright (c) 2017 Douglas Miller <durgadas311@gmail.com>
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
		sys.addTics(sh / 4); // some approximation

		boolean rot = ((m & 04) == 0);
		boolean left = ((m & 01) == 0);
		boolean sngl = ((m & 02) == 0);

		// NOTE: LOR exponent is *always* set to zero
		long mant = I_FMA.nativeToMant(sys.AC[x], sys.denorm[x]);
		long lor = I_FMA.nativeToMant(sys.AC[HW2000.LOR], sys.denorm[HW2000.LOR]);
		// mant is fully sign-extended!
		boolean minus = (mant < 0);
		boolean lormi = (lor < 0);
		if (!left) {
			mant <<= 28;
			lor <<= 28;
		}
		// shift in increments of 20 to avoid fall-off.
		// TODO: double-precision
		while (sh > 0) {
			int s = (sh > 20 ? 20 : sh);
			long rom = (((1 << s) - 1) << (28 - s));
			long rrm = rom << 36;
			if (left) {
				mant <<= s; // sign lost, might need repair
				if (rot) {
					// don't include sign bit...
					long ov = mant & (((1 << s) - 1) << 35);
					mant |= (ov >> 35);
				}
			} else if (rot) {
				// right rotate
				mant >>>= s; // sign lost, as intended
				long ov = mant & rom;
				if (!sngl) {
					lor >>>= s; // sign lost, as intended
					lor &= ~rrm;
					lor |= (ov << 36);
					ov = lor & rom;
				}
				mant &= ~rrm;
				mant |= (ov << 36);
			} else {
				// right arith
				mant >>= s; // sign preserved...
				if (!sngl) {
					lor >>= s; // sign preserved...
					long ov = mant & rom;
					lor &= ~(rrm >> 1);
					lor |= (ov << 35);
				}
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
				if (!sngl) {
					lor &= 0x07ffffffffL; // clear garbage out
					if (lormi) {
						lor |= ~0x07ffffffffL;
					}
				}
			}
		} else {
			// no sign repair
			mant >>= 28;
			lor >>= 28;
		}
		sys.AC[x] = I_FMA.mergeMant(sys.AC[x], mant);
		sys.AC[HW2000.LOR] = I_FMA.mergeMant(sys.AC[HW2000.LOR], lor);
		sys.denorm[x] = ((mant & 0x400000000L) == 0);
		sys.denorm[HW2000.LOR] = ((lor & 0x400000000L) == 0);
	}
}
