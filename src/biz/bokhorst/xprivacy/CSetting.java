package biz.bokhorst.xprivacy;

import java.util.Date;

public class CSetting {
	private long mTimestamp;
	private int mUid;
	private String mName;
	private String mValue;

	public CSetting(int uid, String name) {
		mTimestamp = new Date().getTime();
		mUid = uid;
		mName = name;
		mValue = null;
	}

	public void setValue(String value) {
		mValue = value;
	}

	public boolean willExpire() {
		if (mUid == 0) {
			if (mName.equals(PrivacyManager.cSettingVersion))
				return false;
			if (mName.equals(PrivacyManager.cSettingLog))
				return false;
			if (mName.equals(PrivacyManager.cSettingExperimental))
				return false;
		}
		return true;
	}

	public boolean isExpired() {
		return (willExpire() ? (mTimestamp + PrivacyManager.cSettingCacheTimeoutMs < new Date().getTime()) : false);
	}

	public String getValue() {
		return mValue;
	}

	@Override
	public boolean equals(Object obj) {
		CSetting other = (CSetting) obj;
		return (this.mUid == other.mUid && this.mName.equals(other.mName));
	}

	@Override
	public int hashCode() {
		return mUid ^ mName.hashCode();
	}
}
