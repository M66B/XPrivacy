package biz.bokhorst.xprivacy;

import android.annotation.SuppressLint;
import android.app.AndroidAppHelper;
import android.content.Context;
import android.os.Binder;
import android.os.Build;
import android.os.Process;
import android.util.Log;
import android.widget.Toast;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {

	private String mRestrictionName;
	private String mMethodName;
	private String mSpecifier;
	private int mSdk;

	protected XHook(String restrictionName, String methodName, String specifier) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mSpecifier = specifier;
		mSdk = Build.VERSION_CODES.ICE_CREAM_SANDWICH;

		if (restrictionName != null)
			PrivacyManager.registerMethod(restrictionName, getSpecifier(), mSdk);
	}

	protected XHook(String restrictionName, String methodName, String specifier, int sdk) {
		mRestrictionName = restrictionName;
		mMethodName = methodName;
		mSpecifier = specifier;
		mSdk = sdk;

		if (restrictionName != null)
			PrivacyManager.registerMethod(restrictionName, getSpecifier(), sdk);
	}

	public boolean isVisible() {
		return true;
	}

	abstract public String getClassName();

	public String getRestrictionName() {
		return mRestrictionName;
	}

	public String getMethodName() {
		return mMethodName;
	}

	private String getSpecifier() {
		return (mSpecifier == null ? mMethodName : mSpecifier);
	}

	public int getSdk() {
		return mSdk;
	}

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		return isRestricted(param, getSpecifier());
	}

	protected boolean isRestricted(MethodHookParam param, String methodName) throws Throwable {
		int uid = Binder.getCallingUid();
		Context context = AndroidAppHelper.currentApplication();
		return PrivacyManager.getRestricted(this, context, uid, mRestrictionName, methodName, true, true);
	}

	protected boolean getRestricted(Context context, int uid, boolean usage) throws Throwable {
		return PrivacyManager.getRestricted(this, context, uid, mRestrictionName, getSpecifier(), usage, true);
	}

	protected boolean getRestricted(Context context, int uid, String methodName, boolean usage) throws Throwable {
		return PrivacyManager.getRestricted(this, context, uid, mRestrictionName, methodName, usage, true);
	}

	protected static void notifyUser(String message) throws Throwable {
		notifyUser(AndroidAppHelper.currentApplication(), message);
	}

	@SuppressLint("DefaultLocale")
	protected static void notifyUser(Context context, String message) throws Throwable {
		if (context != null) {
			String format = Util.getXString(context, R.string.msg_restricted);
			String text = String.format(format, message);
			text = String.format("%s uid=%d", text, Binder.getCallingUid());
			Toast toast = Toast.makeText(context, text, Toast.LENGTH_LONG);
			toast.show();
		}
	}

	protected void info(String message) {
		Util.log(this, Log.INFO, message);
	}

	protected void warning(String message) {
		Util.log(this, Log.WARN, message);
	}

	protected void error(String message) {
		Util.log(this, Log.ERROR, message);
	}
}
