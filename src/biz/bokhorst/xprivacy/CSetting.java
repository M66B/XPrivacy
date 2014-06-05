package biz.bokhorst.xprivacy;

import java.util.Date;

import android.util.Log;

public class CSetting {
	private long mTimestamp;
	private int mUid;
	private String mType;
	private String mName;
	private String mValue;

	public CSetting(int uid, String type, String name) {
		mTimestamp = new Date().getTime();
		mUid = Math.abs(uid);
		mType = type;
		mName = name;
		mValue = null;
		if (type == null) {
			Util.log(null, Log.WARN, "CSetting null");
			Util.logStack(null, Log.WARN);
		}
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

	public int getUid() {
		return mUid;
	}

	public String getValue() {
		return mValue;
	}

	@Override
	public boolean equals(Object obj) {
		CSetting other = (CSetting) obj;
		return (this.mUid == other.mUid && this.mType.equals(other.mType) && this.mName.equals(other.mName));
	}

	@Override
	public int hashCode() {
		return mUid ^ mType.hashCode() ^ mName.hashCode();
	}

	@Override
	public String toString() {
		return mUid + ":" + mType + "/" + mName + "=" + mValue;
	}
}
