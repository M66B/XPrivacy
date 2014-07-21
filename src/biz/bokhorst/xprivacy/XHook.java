package biz.bokhorst.xprivacy;

import android.os.Binder;
import android.os.Build;

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

	public static boolean isAOSP(int sdk) {
		if (sdk == Build.VERSION_CODES.KITKAT && sdk == Build.VERSION.SDK_INT) {
			if (Build.DISPLAY == null || Build.HOST == null)
				return false;
			return (Build.HOST.endsWith(".google.com") || Build.DISPLAY.startsWith("omni"));
		} else
			return false;
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
	public String toString() {
		return getRestrictionName() + "/" + getSpecifier() + " (" + getClassName() + ")";
	}
}
