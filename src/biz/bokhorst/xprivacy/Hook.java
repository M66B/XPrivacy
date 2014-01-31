package biz.bokhorst.xprivacy;

public class Hook implements Comparable<Hook> {
	private String mRestrictionName;
	private String mMethodName;
	private boolean mDangerous;
	private boolean mRestart;
	private boolean mNoUsageData;
	private String[] mPermissions;
	private int mSdk;
	private Version mFrom;
	private String mReplaces;

	public Hook(String restrictionName, String methodName) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
	}

	public Hook(String restrictionName, String methodName, boolean dangerous, boolean restart, String permissions,
			int sdk, String from, String replaces) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mDangerous = dangerous;
		mRestart = restart;
		mNoUsageData = restart;
		mPermissions = (permissions == null ? null : permissions.split(","));
		mSdk = sdk;
		mFrom = (from == null ? null : new Version(from));
		mReplaces = replaces;
	}

	public Hook noUsageData() {
		mNoUsageData = true;
		return this;
	}

	public String getRestrictionName() {
		return mRestrictionName;
	}

	public String getName() {
		return mMethodName;
	}

	public boolean isDangerous() {
		String name = String.format("%s.%s", PrivacyManager.cSettingDangerous, this.getName());
		return PrivacyManager.getSettingBool(null, 0, name, mDangerous, false);
	}

	public void toggleDangerous() {
		String name = String.format("%s.%s", PrivacyManager.cSettingDangerous, this.getName());
		PrivacyManager.setSetting(null, 0, name, Boolean.toString(!isDangerous()));
	}

	public boolean isRestartRequired() {
		return mRestart;
	}

	public boolean hasNoUsageData() {
		return mNoUsageData;
	}

	public String[] getPermissions() {
		return mPermissions;
	}

	public int getSdk() {
		return mSdk;
	}

	public Version getFrom() {
		return mFrom;
	}

	public String getReplaces() {
		return mReplaces;
	}

	@Override
	public int hashCode() {
		return (mRestrictionName.hashCode() ^ mMethodName.hashCode());
	}

	@Override
	public boolean equals(Object obj) {
		Hook other = (Hook) obj;
		return (mRestrictionName.equals(other.mRestrictionName) && mMethodName.equals(other.mMethodName));
	}

	@Override
	public int compareTo(Hook another) {
		int x = mRestrictionName.compareTo(another.mRestrictionName);
		return (x == 0 ? mMethodName.compareTo(another.mMethodName) : x);
	}

	@Override
	public String toString() {
		return String.format("%s/%s", mRestrictionName, mMethodName);
	}
}
