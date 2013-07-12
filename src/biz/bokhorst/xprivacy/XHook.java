package biz.bokhorst.xprivacy;

import android.app.AndroidAppHelper;
import android.content.Context;
import android.os.Binder;
import android.util.Log;
import android.widget.Toast;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {

	private String mMethodName;
	private String mRestrictionName;
	private String mSpecifier;

	public XHook(String methodName, String restrictionName, String[] permissions, String specifier) {
		// Sanity check
		if (methodName == null)
			throw new IllegalArgumentException();

		mMethodName = methodName;
		mRestrictionName = restrictionName;
		mSpecifier = specifier;

		if (restrictionName != null)
			PrivacyManager.registerMethod(getSpecifier(), restrictionName, permissions);
	}

	public String getMethodName() {
		return mMethodName;
	}

	public String getRestrictionName() {
		return mRestrictionName;
	}

	private String getSpecifier() {
		return (mSpecifier == null ? mMethodName : mSpecifier);
	}

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		return isRestricted(param, mMethodName);
	}

	protected boolean isRestricted(MethodHookParam param, String methodName) throws Throwable {
		int uid = Binder.getCallingUid();
		Context context = AndroidAppHelper.currentApplication();
		return PrivacyManager.getRestricted(this, context, uid, mRestrictionName, methodName, true, true);
	}

	protected boolean getRestricted(Context context, int uid, boolean usage) throws Throwable {
		return PrivacyManager.getRestricted(this, context, uid, mRestrictionName, mMethodName, usage, true);
	}

	protected boolean getRestricted(Context context, int uid, String methodName, boolean usage) throws Throwable {
		return PrivacyManager.getRestricted(this, context, uid, mRestrictionName, methodName, usage, true);
	}

	protected void notifyUser(String message) throws Throwable {
		notifyUser(AndroidAppHelper.currentApplication(), message);
	}

	protected void notifyUser(Context context, String message) throws Throwable {
		if (context != null) {
			String format = Util.getXString(context, R.string.msg_restricted);
			String text = String.format(format, message);
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
