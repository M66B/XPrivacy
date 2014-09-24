package biz.bokhorst.xprivacy;

import android.annotation.SuppressLint;
import android.os.Binder;

public abstract class XHook {
	private String mRestrictionName;
	private String mMethodName;
	private String mSpecifier;
	private String mSecret;

	protected XHook(String restrictionName, String methodName, String specifier) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mSpecifier = specifier;
	}

	abstract public String getClassName();

	public boolean isVisible() {
		return true;
	}

	public String getRestrictionName() {
		return mRestrictionName;
	}

	public String getMethodName() {
		return mMethodName;
	}

	public String getSpecifier() {
		return (mSpecifier == null ? mMethodName : mSpecifier);
	}

	public void setSecret(String secret) {
		mSecret = secret;
	}

	protected String getSecret() {
		return mSecret;
	}

	abstract protected void before(XParam param) throws Throwable;

	abstract protected void after(XParam param) throws Throwable;

	protected boolean isRestricted(XParam param) throws Throwable {
		return isRestricted(param, getSpecifier());
	}

	protected boolean isRestrictedExtra(XParam param, String extra) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestrictionExtra(this, uid, mRestrictionName, getSpecifier(), extra, mSecret);
	}

	protected boolean isRestrictedExtra(XParam param, String methodName, String extra) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestrictionExtra(this, uid, mRestrictionName, methodName, extra, mSecret);
	}

	protected boolean isRestrictedExtra(XParam param, String restrictionName, String methodName, String extra)
			throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestrictionExtra(this, uid, restrictionName, methodName, extra, mSecret);
	}

	protected boolean isRestrictedExtra(int uid, String restrictionName, String methodName, String extra)
			throws Throwable {
		return PrivacyManager.getRestrictionExtra(this, uid, restrictionName, methodName, extra, mSecret);
	}

	protected boolean isRestrictedValue(int uid, String value) throws Throwable {
		return PrivacyManager.getRestrictionExtra(this, uid, mRestrictionName, getSpecifier(), null, value, mSecret);
	}

	protected boolean isRestrictedValue(XParam param, String value) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestrictionExtra(this, uid, mRestrictionName, getSpecifier(), null, value, mSecret);
	}

	protected boolean isRestrictedValue(int uid, String methodName, String value) throws Throwable {
		return PrivacyManager.getRestrictionExtra(this, uid, mRestrictionName, methodName, null, value, mSecret);
	}

	protected boolean isRestrictedValue(int uid, String restrictionName, String methodName, String value)
			throws Throwable {
		return PrivacyManager.getRestrictionExtra(this, uid, restrictionName, methodName, null, value, mSecret);
	}

	protected boolean isRestrictedExtraValue(int uid, String restrictionName, String methodName, String extra,
			String value) throws Throwable {
		return PrivacyManager.getRestrictionExtra(this, uid, restrictionName, methodName, extra, value, mSecret);
	}

	protected boolean isRestricted(XParam param, String methodName) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestriction(this, uid, mRestrictionName, methodName, mSecret);
	}

	protected boolean isRestricted(XParam param, String restrictionName, String methodName) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestriction(this, uid, restrictionName, methodName, mSecret);
	}

	protected boolean getRestricted(int uid) throws Throwable {
		return PrivacyManager.getRestriction(this, uid, mRestrictionName, getSpecifier(), mSecret);
	}

	protected boolean getRestricted(int uid, String methodName) throws Throwable {
		return PrivacyManager.getRestriction(this, uid, mRestrictionName, methodName, mSecret);
	}

	protected boolean getRestricted(int uid, String restrictionName, String methodName) throws Throwable {
		return PrivacyManager.getRestriction(this, uid, restrictionName, methodName, mSecret);
	}

	@Override
	@SuppressLint("FieldGetter")
	public String toString() {
		return getRestrictionName() + "/" + getSpecifier() + " (" + getClassName() + ")";
	}
}
