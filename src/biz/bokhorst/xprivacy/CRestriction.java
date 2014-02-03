package biz.bokhorst.xprivacy;

import java.util.Date;

public class CRestriction {
	private long mTimestamp;
	private PRestriction mRestriction;

	public CRestriction(PRestriction restriction) {
		mTimestamp = new Date().getTime();
		mRestriction = new PRestriction(restriction);
	}

	public PRestriction getRestriction() {
		return mRestriction;
	}

	public void setRestriction(PRestriction restriction) {
		mRestriction = new PRestriction(restriction);
	}

	public boolean isExpired() {
		return (mTimestamp + PrivacyManager.cRestrictionCacheTimeoutMs < new Date().getTime());
	}

	@Override
	public boolean equals(Object obj) {
		CRestriction other = (CRestriction) obj;
		return (this.mRestriction.uid == other.mRestriction.uid
				&& this.mRestriction.restrictionName.equals(other.mRestriction.restrictionName)
				&& this.mRestriction.methodName == null ? other.mRestriction.methodName == null
				: this.mRestriction.methodName.equals(other.mRestriction.methodName));
	}

	@Override
	public int hashCode() {
		int hash = mRestriction.uid ^ mRestriction.restrictionName.hashCode();
		if (mRestriction.methodName != null)
			hash = hash ^ mRestriction.methodName.hashCode();
		return hash;
	}
}
