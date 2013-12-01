package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.Application;
import android.content.Context;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XApplication extends XHook {
	private Methods mMethod;

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
			Thread.UncaughtExceptionHandler defaultHandler = Thread.getDefaultUncaughtExceptionHandler();
			if (!(defaultHandler instanceof XUncaughtExceptionHandler)) {
				Util.log(this, Log.INFO, "Installing XUncaughtExceptionHandler uid=" + Binder.getCallingUid());
				Application app = (Application) param.thisObject;
				Thread.setDefaultUncaughtExceptionHandler(new XUncaughtExceptionHandler(this, app, defaultHandler));
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	private static class XUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {
		private XHook mHook;
		private Context mContext;
		private Thread.UncaughtExceptionHandler mDefaultHandler;

		public XUncaughtExceptionHandler(XHook hook, Context context, Thread.UncaughtExceptionHandler defaultHandler) {
			mHook = hook;
			mContext = context;
			mDefaultHandler = defaultHandler;
		}

		@Override
		public void uncaughtException(Thread thread, Throwable ex) {
			try {
				Util.log(mHook, Log.WARN, "Uncaught exception uid=" + Binder.getCallingUid() + ": " + ex.getMessage());
				PrivacyManager.sendUsageData(null, mContext);
			} catch (Throwable exex) {
				Util.bug(mHook, exex);
			}
			mDefaultHandler.uncaughtException(thread, ex);
		}
	}
}
