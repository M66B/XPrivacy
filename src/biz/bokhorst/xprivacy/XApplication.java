package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.Application;
import android.content.Context;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XApplication extends XHook {
	private Methods mMethod;
	private static XUncaughtExceptionHandler mUncaughtExceptionHandler = null;

	public XApplication(Methods method, String restrictionName, String actionName) {
		super(restrictionName, method.name(), actionName);
		mMethod = method;
	}

	@Override
	public String getClassName() {
		return "android.app.Application";
	}

	// public void onCreate ()
	// frameworks/base/core/java/android/app/Application.java
	// http://developer.android.com/reference/android/app/Application.html

	private enum Methods {
		onCreate
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XApplication(Methods.onCreate, null, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.onCreate) {
			if (mUncaughtExceptionHandler == null)
				mUncaughtExceptionHandler = new XUncaughtExceptionHandler((Application) param.thisObject);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
	
	private static class XUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {

		private static Context mContext;

		public XUncaughtExceptionHandler(Application app) {
			mContext = app;
		}

		@Override
		public void uncaughtException(Thread arg0, Throwable arg1) {
			try {
				PrivacyManager.sendUsageData(null, mContext);
			} finally {
				// do nothing
			}
		}
	}
}
