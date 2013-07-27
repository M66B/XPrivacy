package biz.bokhorst.xprivacy;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

import android.os.Environment;
import android.util.Log;

import biz.bokhorst.xprivacy.XHook;

public class XEnvironment extends XHook {

	public XEnvironment(String methodName, String restrictionName, String[] permissions) {
		super(restrictionName, methodName, permissions, null);
	}

	// public static String getExternalStorageState()
	// frameworks/base/core/java/android/os/Environment.java
	// http://developer.android.com/reference/android/os/Environment.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("getExternalStorageState")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(Environment.MEDIA_UNMOUNTED);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}
}
