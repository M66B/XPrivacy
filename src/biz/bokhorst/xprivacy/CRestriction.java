package biz.bokhorst.xprivacy;

import java.util.Date;

public class CRestriction {
	private long mExpiry;
	public int uid;
	public String restrictionName;
	public String methodName;
	public String extra;
	public boolean restricted;
	public boolean asked;

	public CRestriction(int _uid, String _restrictioName, String _methodName, String _extra) {
		mExpiry = new Date().getTime() + PrivacyManager.cRestrictionCacheTimeoutMs;
		uid = _uid;
		restrictionName = _restrictioName;
		methodName = _methodName;
		extra = _extra;
	}

	public CRestriction(PRestriction restriction) {
		mExpiry = new Date().getTime() + PrivacyManager.cRestrictionCacheTimeoutMs;
		uid = restriction.uid;
		restrictionName = restriction.restrictionName;
		methodName = restriction.methodName;
		extra = restriction.extra;
		restricted = restriction.restricted;
		asked = restriction.asked;
	}

	public void setExpiry(long time) {
		mExpiry = time;
	}

	public boolean isExpired() {
		return (new Date().getTime() > mExpiry);
	}

	@Override
	public boolean equals(Object obj) {
		CRestriction other = (CRestriction) obj;
		// @formatter:off
		return (uid == other.uid
				&& (restrictionName == null ? other.restrictionName == null : restrictionName.equals(other.restrictionName))
				&& (methodName == null ? other.methodName == null : methodName.equals(other.methodName))
				&& (extra == null ? other.extra == null : extra.equals(other.extra)));
		// @formatter:on
	}

	@Override
	public int hashCode() {
		int hash = uid;
		if (restrictionName != null)
			hash = hash ^ restrictionName.hashCode();
		if (methodName != null)
			hash = hash ^ methodName.hashCode();
		if (extra != null)
			hash = hash ^ extra.hashCode();
		return hash;
	}
}
