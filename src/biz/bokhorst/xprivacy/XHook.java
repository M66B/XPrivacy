package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findClass;
import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.app.AndroidAppHelper;
import android.content.Context;
import android.os.Binder;
import android.util.Log;
import android.widget.Toast;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {

	private String mMethodName;
	private String mRestrictionName;

	public XHook(String methodName, String restrictionName, String[] permissions) {
		// Sanity check
		if (methodName == null)
			throw new IllegalArgumentException();

		mMethodName = methodName;
		mRestrictionName = restrictionName;
		if (restrictionName != null)
			XRestriction.registerMethod(methodName, restrictionName, permissions);
	}

	public String getMethodName() {
		return mMethodName;
	}

	public String getRestrictionName() {
		return mRestrictionName;
	}

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Context context = AndroidAppHelper.currentApplication();
		if (context == null)
			try {
				Class<?> atClass = findClass("android.app.ActivityThread", param.thisObject.getClass().getClassLoader());
				Field fieldSystemContext = findField(atClass, "mSystemContext");
				context = (Context) fieldSystemContext.get(null);
				if (context == null) {
					XUtil.log(this, Log.INFO, "No context, not restricting");
					XUtil.logStack(this);
					return false;
				} else
					XUtil.log(this, Log.INFO, "Using system context");
			} catch (Throwable ex) {
				XUtil.bug(this, ex);
				return false;
			}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

	protected boolean getRestricted(Context context, int uid, boolean usage) {
		return XRestriction.getRestrictedCached(this, context, uid, mRestrictionName, usage);
	}

	protected void setRestricted(Context context, int uid, boolean restricted) {
		XRestriction.setRestricted(this, context, uid, mRestrictionName, restricted);
	}

	protected void notifyUser(String message) throws Throwable {
		notifyUser(AndroidAppHelper.currentApplication(), message);
	}

	protected void notifyUser(Context context, String message) throws Throwable {
		String format = XUtil.getXString(context, R.string.msg_restricted);
		String text = String.format(format, message);
		Toast toast = Toast.makeText(context, text, Toast.LENGTH_LONG);
		toast.show();
	}

	protected void info(String message) {
		XUtil.log(this, Log.INFO, message);
	}

	protected void warning(String message) {
		XUtil.log(this, Log.WARN, message);
	}

	protected void error(String message) {
		XUtil.log(this, Log.ERROR, message);
	}
}
