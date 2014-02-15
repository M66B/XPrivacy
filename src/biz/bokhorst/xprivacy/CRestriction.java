package biz.bokhorst.xprivacy;

import java.util.Date;

public class CRestriction {
	private long mTimestamp;
	public int uid;
	public String restrictionName;
	public String methodName;
	public boolean restricted;
	public boolean asked;

	public CRestriction(int _uid, String _restrictioName, String _methodName) {
		mTimestamp = new Date().getTime();
		uid = _uid;
		restrictionName = _restrictioName;
		methodName = _methodName;
	}

	public CRestriction(PRestriction restriction) {
		mTimestamp = new Date().getTime();
		uid = restriction.uid;
		restrictionName = restriction.restrictionName;
		methodName = restriction.methodName;
		restricted = restriction.restricted;
		asked = restriction.asked;
	}

	public void setExpiry(long time) {
		mTimestamp = time;
	}

	public boolean isExpired() {
		return (mTimestamp + PrivacyManager.cRestrictionCacheTimeoutMs < new Date().getTime());
	}

	@Override
	public boolean equals(Object obj) {
		CRestriction other = (CRestriction) obj;
		return (uid == other.uid
				&& (restrictionName == null ? other.restrictionName == null : restrictionName
						.equals(other.restrictionName)) && (methodName == null ? other.methodName == null : methodName
				.equals(other.methodName)));
	}

	@Override
	public int hashCode() {
		int hash = uid;
		if (restrictionName != null)
			hash = hash ^ restrictionName.hashCode();
		if (methodName != null)
			hash = hash ^ methodName.hashCode();
		return hash;
	}
}
