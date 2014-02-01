package biz.bokhorst.xprivacy;

import java.util.Date;

public class CRestriction {
	private long mTimestamp;
	private ParcelableRestriction mRestriction;

	public CRestriction(ParcelableRestriction restriction) {
		mTimestamp = new Date().getTime();
		mRestriction = restriction;
	}

	public ParcelableRestriction getRestriction() {
		return mRestriction;
	}

	public void setRestriction(ParcelableRestriction restriction) {
		mRestriction = restriction;
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
