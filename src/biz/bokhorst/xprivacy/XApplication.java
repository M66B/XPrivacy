package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.Application;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XApplication extends XHook {
	private Methods mMethod;

	private static boolean mReceiverInstalled = false;

	public static String cPackageName = "PackageName";
	public static String cAction = "Action";
	public static String cActionKillProcess = "Kill";
	public static String cActionFlushCache = "Flush";

	public static String ACTION_MANAGE_PACKAGE = "biz.bokhorst.xprivacy.ACTION_MANAGE_PACKAGE";
	public static String PERMISSION_MANAGE_PACKAGES = "biz.bokhorst.xprivacy.MANAGE_PACKAGES";

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
			Application app = (Application) param.thisObject;

			// Install uncaught exception handler
			Thread.UncaughtExceptionHandler defaultHandler = Thread.getDefaultUncaughtExceptionHandler();
			if (!(defaultHandler instanceof XUncaughtExceptionHandler)) {
				Util.log(this, Log.INFO, "Installing XUncaughtExceptionHandler uid=" + Binder.getCallingUid());
				Thread.setDefaultUncaughtExceptionHandler(new XUncaughtExceptionHandler(this, app, defaultHandler));
			}

			// Install receiver for package management
			if (Binder.getCallingUid() != PrivacyManager.cAndroidUid) {
				boolean experimental = PrivacyManager.getSettingBool(null, app, 0, PrivacyManager.cSettingExperimental,
						false, true);
				if (experimental && !mReceiverInstalled)
					try {
						mReceiverInstalled = true;
						Util.log(this, Log.INFO, "Installing receiver uid=" + Binder.getCallingUid());
						app.registerReceiver(new Receiver(app), new IntentFilter(ACTION_MANAGE_PACKAGE),
								PERMISSION_MANAGE_PACKAGES, null);
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	public static void manage(Context context, int uid, String action) {
		String[] packageName = context.getPackageManager().getPackagesForUid(uid);
		manage(context, packageName[0], action);
	}

	public static void manage(Context context, String packageName, String action) {
		Util.log(null, Log.WARN, "Manage package=" + packageName + " action=" + action);
		Intent manageIntent = new Intent(XApplication.ACTION_MANAGE_PACKAGE);
		manageIntent.putExtra(XApplication.cPackageName, packageName);
		manageIntent.putExtra(XApplication.cAction, action);
		context.sendBroadcast(manageIntent); // XApplication.PERMISSION_MANAGE_PACKAGES);
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
			} catch (Throwable exex) {
				Util.bug(mHook, exex);
			}
			mDefaultHandler.uncaughtException(thread, ex);
		}
	}

	private class Receiver extends BroadcastReceiver {
		private Application mApplication;

		public Receiver(Application app) {
		}

		@Override
		public void onReceive(Context context, Intent intent) {
			try {
				String packageName = intent.getExtras().getString(cPackageName);
				if (context.getPackageName().equals(packageName)) {
					String action = intent.getExtras().getString(cAction);
					Util.log(null, Log.WARN, "Managing package=" + packageName + " action=" + action);
					if (action.equals(cActionKillProcess))
						android.os.Process.killProcess(android.os.Process.myPid());
					else if (action.equals(cActionFlushCache))
						PrivacyManager.flush(mApplication, Binder.getCallingUid());
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}
	}
}
