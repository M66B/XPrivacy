package biz.bokhorst.xprivacy;

import java.util.Date;

public class CRestriction {
	private long mExpiry;
	private int mUid;
	private String mRestrictionName;
	private String mMethodName;
	private String mExtra;
	public boolean restricted;
	public boolean asked;

	public CRestriction(int uid, String restrictionName, String methodName, String extra) {
		mExpiry = new Date().getTime() + PrivacyManager.cRestrictionCacheTimeoutMs;
		mUid = uid;
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mExtra = extra;
		restricted = false;
		asked = false;
	}

	public CRestriction(PRestriction restriction, String extra) {
		mExpiry = new Date().getTime() + PrivacyManager.cRestrictionCacheTimeoutMs;
		mUid = restriction.uid;
		mRestrictionName = restriction.restrictionName;
		mMethodName = restriction.methodName;
		mExtra = extra;
		restricted = restriction.restricted;
		asked = restriction.asked;
	}

	public void setExpiry(long time) {
		mExpiry = time;
	}

	public boolean isExpired() {
		return (new Date().getTime() > mExpiry);
	}

	public int getUid() {
		return mUid;
	}

	public void setMethodName(String methodName) {
		mMethodName = methodName;
	}

	public void setExtra(String extra) {
		mExtra = extra;
	}

	public boolean isSameMethod(PRestriction restriction) {
		// @formatter:off
		return (restriction.uid == mUid
				&& restriction.restrictionName.equals(mRestrictionName)
				&& (restriction.methodName == null || restriction.methodName.equals(mMethodName)));
		// @formatter:on
	}

	@Override
	public boolean equals(Object obj) {
		CRestriction other = (CRestriction) obj;
		// @formatter:off
		return (mUid == other.mUid
				&& mRestrictionName.equals(other.mRestrictionName)
				&& (mMethodName == null ? other.mMethodName == null : mMethodName.equals(other.mMethodName))
				&& (mExtra == null ? other.mExtra == null : mExtra.equals(other.mExtra)));
		// @formatter:on
	}

	@Override
	public int hashCode() {
		int hash = mUid;
		if (mRestrictionName != null)
			hash = hash ^ mRestrictionName.hashCode();
		if (mMethodName != null)
			hash = hash ^ mMethodName.hashCode();
		if (mExtra != null)
			hash = hash ^ mExtra.hashCode();
		return hash;
	}

	@Override
	public String toString() {
		return mUid + ":" + mRestrictionName + "/" + mMethodName + "(" + mExtra + ")" + "=" + restricted + "/" + asked;
	}
}
