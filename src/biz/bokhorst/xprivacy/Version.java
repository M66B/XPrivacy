package biz.bokhorst.xprivacy;

public class Version implements Comparable<Version> {

	private String mVersion;

	public Version(String version) {
		mVersion = version;
	}

	private String get() {
		return mVersion;
	}

	@Override
	public int compareTo(Version other) {
		String[] lhs = this.get().split("\\.");
		String[] rhs = other.get().split("\\.");
		int length = Math.max(lhs.length, rhs.length);
		for (int i = 0; i < length; i++) {
			int vLhs = (i < lhs.length ? Integer.parseInt(lhs[i]) : 0);
			int vRhs = (i < rhs.length ? Integer.parseInt(rhs[i]) : 0);
			if (vLhs < vRhs)
				return -1;
			if (vLhs > vRhs)
				return 1;
		}
		return 0;
	}

	@Override
	public String toString() {
		return mVersion;
	}
}