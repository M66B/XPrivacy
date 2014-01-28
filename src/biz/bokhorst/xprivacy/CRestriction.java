package biz.bokhorst.xprivacy;

import java.util.Date;

public class CRestriction {
	private long mTimestamp;
	private int mUid;
	private String mRestrictionName;
	private String mMethodName;
	private boolean mRestricted;

	public CRestriction(int uid, String restrictionName, String methodName) {
		mTimestamp = new Date().getTime();
		mUid = uid;
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mRestricted = false;
	}

	public void setRestricted(boolean restricted) {
		mRestricted = restricted;
	}

	public boolean isExpired() {
		return (mTimestamp + PrivacyManager.cRestrictionCacheTimeoutMs < new Date().getTime());
	}

	public boolean isRestricted() {
		return mRestricted;
	}

	@Override
	public boolean equals(Object obj) {
		CRestriction other = (CRestriction) obj;
		return (this.mUid == other.mUid && this.mRestrictionName.equals(other.mRestrictionName)
				&& this.mMethodName == null ? other.mMethodName == null : this.mMethodName.equals(other.mMethodName));
	}

	@Override
	public int hashCode() {
		int hash = mUid ^ mRestrictionName.hashCode();
		if (mMethodName != null)
			hash = hash ^ mMethodName.hashCode();
		return hash;
	}
}
