package biz.bokhorst.xprivacy;

import android.os.Binder;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {
	private String mRestrictionName;
	private String mMethodName;
	private String mSpecifier;
	private int mSdk;
	private boolean mOptional = false;
	private String mSecret;

	protected XHook(String restrictionName, String methodName, String specifier) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mSpecifier = specifier;
		mSdk = 0;
	}

	protected XHook(String restrictionName, String methodName, String specifier, int sdk) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mSpecifier = specifier;
		mSdk = sdk;
	}

	protected XHook optional() {
		mOptional = true;
		return this;
	}

	public boolean isVisible() {
		return true;
	}

	public boolean isOptional() {
		return mOptional;
	}

	abstract public String getClassName();

	public String getRestrictionName() {
		return mRestrictionName;
	}

	public String getMethodName() {
		return mMethodName;
	}

	public String getSpecifier() {
		return (mSpecifier == null ? mMethodName : mSpecifier);
	}

	public int getSdk() {
		return mSdk;
	}

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	public void setSecret(String secret) {
		mSecret = secret;
	}

	protected String getSecret() {
		return mSecret;
	}

	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		return isRestricted(param, getSpecifier());
	}

	protected boolean isRestrictedExtra(MethodHookParam param, String extra) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestrictionExtra(this, uid, mRestrictionName, getSpecifier(), extra, mSecret);
	}

	protected boolean isRestrictedExtra(MethodHookParam param, String methodName, String extra) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestrictionExtra(this, uid, mRestrictionName, methodName, extra, mSecret);
	}

	protected boolean isRestrictedExtra(MethodHookParam param, String restrictionName, String methodName, String extra)
			throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestrictionExtra(this, uid, restrictionName, methodName, extra, mSecret);
	}

	protected boolean isRestricted(MethodHookParam param, String methodName) throws Throwable {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getRestriction(this, uid, mRestrictionName, methodName, mSecret);
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
}
