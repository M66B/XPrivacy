package biz.bokhorst.xprivacy;

import java.util.Date;

import android.annotation.SuppressLint;

public class UsageData implements Comparable<UsageData> {
	private Integer mUid;
	private String mRestriction;
	private String mMethodName;
	private boolean mRestricted;
	private long mTimeStamp;
	private int mHash;

	public UsageData(int uid, String restrictionName, String methodName, boolean restricted) {
		initialize(uid, restrictionName, methodName, restricted, new Date().getTime());
	}

	public UsageData(int uid, String restrictionName, String methodName, boolean restricted, long used) {
		initialize(uid, restrictionName, methodName, restricted, used);
	}

	private void initialize(int uid, String restrictionName, String methodName, boolean restricted, long used) {
		mUid = uid;
		mRestriction = restrictionName;
		mMethodName = methodName;
		mRestricted = restricted;
		mTimeStamp = used;

		mHash = mUid.hashCode();
		if (mRestriction != null)
			mHash = mHash ^ mRestriction.hashCode();
		if (mMethodName != null)
			mHash = mHash ^ mMethodName.hashCode();
	}

	public int getUid() {
		return mUid;
	}

	public String getRestrictionName() {
		return mRestriction;
	}

	public String getMethodName() {
		return mMethodName;
	}

	public boolean getRestricted() {
		return mRestricted;
	}

	public long getTimeStamp() {
		return mTimeStamp;
	}

	@Override
	public int hashCode() {
		return mHash;
	}

	@Override
	public boolean equals(Object obj) {
		UsageData other = (UsageData) obj;
		// @formatter:off
		return
			(mUid.equals(other.mUid) &&
			(mRestriction == null ? other.mRestriction == null : mRestriction.equals(other.mRestriction)) &&
			(mMethodName == null ? other.mMethodName == null : mMethodName.equals(other.mMethodName)));
		// @formatter:on
	}

	@Override
	@SuppressLint("DefaultLocale")
	public String toString() {
		return String.format("%d/%s/%s=%b", mUid, mRestriction, mMethodName, mRestricted);
	}

	@Override
	public int compareTo(UsageData another) {
		if (mTimeStamp < another.mTimeStamp)
			return 1;
		else if (mTimeStamp > another.mTimeStamp)
			return -1;
		else
			return 0;
	}
}
