package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

import android.os.Environment;
import android.util.Log;

import biz.bokhorst.xprivacy.XHook;

public class XEnvironment extends XHook {

	private XEnvironment(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "android.os.Environment";
	}

	// public static String getExternalStorageState()
	// frameworks/base/core/java/android/os/Environment.java
	// http://developer.android.com/reference/android/os/Environment.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XEnvironment("getExternalStorageState", PrivacyManager.cStorage));
		return listHook;
	}

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
