package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.Application;
import android.content.Context;
import android.content.Intent;
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

	// public void onCreate()
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

	public static class XUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {
		private XHook mHook;
		private Context mContext;
		private Thread.UncaughtExceptionHandler mDefaultHandler;

		public XUncaughtExceptionHandler(XHook hook, Context context, Thread.UncaughtExceptionHandler defaultHandler) {
			mHook = hook;
			mContext = context;
			mDefaultHandler = defaultHandler;
		}

		public Thread.UncaughtExceptionHandler getDefaultHandler() {
			return mDefaultHandler;
		}

		public void setDefaultHandler(Thread.UncaughtExceptionHandler handler) {
			Util.log(mHook, Log.WARN, "Setting new default handler uid=" + Binder.getCallingUid());
			mDefaultHandler = handler;
		}

		@Override
		public void uncaughtException(Thread thread, Throwable ex) {
			try {
				int uid = Binder.getCallingUid();
				Util.log(mHook, Log.WARN, "Uncaught exception uid=" + uid + ": " + ex);

				// Update usage data
				if (PrivacyManager.isUsageDataEnabled(uid))
					PrivacyManager.sendUsageData(null, mContext);

				// Open app settings
				String self = XApplication.class.getPackage().getName();
				if (!mContext.getPackageName().equals(self)) {
					Intent intentCrashed = new Intent(Intent.ACTION_MAIN);
					intentCrashed.putExtra(ActivityApp.cPackageName, mContext.getPackageName());
					intentCrashed.putExtra(ActivityApp.cActionCrash, ex.toString());
					intentCrashed.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
					intentCrashed.setClass(mContext.getApplicationContext(), ActivityApp.class);
					mContext.startActivity(intentCrashed);
				}
			} catch (Throwable exex) {
				Util.bug(mHook, exex);
			}
			mDefaultHandler.uncaughtException(thread, ex);
		}
	}
}
