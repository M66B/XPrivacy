package biz.bokhorst.xprivacy;

import android.content.Context;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {

	private String mMethodName;
	private String mPermissionName;

	public XHook(String methodName, String permissionName) {
		mMethodName = methodName;
		mPermissionName = permissionName;
	}

	public String getMethodName() {
		return mMethodName;
	}

	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	abstract protected boolean isAllowed(MethodHookParam param) throws Throwable;

	protected boolean getAllowed(Context context, int uid, boolean usage) {
		return XPermissions.getAllowed(this, context, uid, mPermissionName, usage);
	}

	protected void setAllowed(Context context, int uid, boolean allowed) {
		XPermissions.setAllowed(this, context, uid, mPermissionName, allowed);
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
