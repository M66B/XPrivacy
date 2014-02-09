package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.app.Application;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Process;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XApplication extends XHook {
	private Methods mMethod;

	private static boolean mReceiverInstalled = false;

	public static String cAction = "Action";
	public static String cActionKillProcess = "Kill";

	public static String ACTION_MANAGE_PACKAGE = "biz.bokhorst.xprivacy.ACTION_MANAGE_PACKAGE";
	public static String PERMISSION_MANAGE_PACKAGES = "biz.bokhorst.xprivacy.MANAGE_PACKAGES";

	public XApplication(Methods method, String restrictionName, String actionName, int sdk) {
		super(restrictionName, method.name(), actionName, sdk);
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
		listHook.add(new XApplication(Methods.onCreate, null, null, 1));
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

			// Install receiver for package management
			if (PrivacyManager.isApplication(Process.myUid()) && !mReceiverInstalled)
				try {
					mReceiverInstalled = true;
					Util.log(this, Log.INFO, "Installing receiver uid=" + Process.myUid());
					app.registerReceiver(new Receiver(app), new IntentFilter(ACTION_MANAGE_PACKAGE),
							PERMISSION_MANAGE_PACKAGES, null);
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	public static void manage(Context context, int uid, String action) {
		if (uid == 0)
			manage(context, null, action);
		else {
			String[] packageName = context.getPackageManager().getPackagesForUid(uid);
			if (packageName != null && packageName.length > 0)
				manage(context, packageName[0], action);
			else
				Util.log(null, Log.WARN, "No packages uid=" + uid + " action=" + action);
		}
	}

	public static void manage(Context context, String packageName, String action) {
		Util.log(null, Log.INFO, "Manage package=" + packageName + " action=" + action);
		if (packageName == null && XApplication.cActionKillProcess.equals(action)) {
			Util.log(null, Log.WARN, "Kill all");
			return;
		}
		Intent manageIntent = new Intent(XApplication.ACTION_MANAGE_PACKAGE);
		manageIntent.putExtra(XApplication.cAction, action);
		if (packageName != null)
			manageIntent.setPackage(packageName);
		context.sendBroadcast(manageIntent);
	}

	private class Receiver extends BroadcastReceiver {
		public Receiver(Application app) {
		}

		@Override
		public void onReceive(Context context, Intent intent) {
			try {
				String action = intent.getExtras().getString(cAction);
				Util.log(null, Log.INFO, "Managing uid=" + Process.myUid() + " action=" + action);
				if (cActionKillProcess.equals(action))
					android.os.Process.killProcess(Process.myPid());
				else
					Util.log(null, Log.WARN, "Unknown management action=" + action);
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}
	}
}
