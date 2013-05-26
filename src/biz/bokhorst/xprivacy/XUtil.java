package biz.bokhorst.xprivacy;

import java.util.List;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.content.ContentProvider;
import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Binder;

import de.robv.android.xposed.XposedBridge;

public class XUtil {

	public static Boolean isCallingPackage(XHook hook, ContentProvider contentProvider, String propertyName) {
		// Get content provider
		if (contentProvider == null) {
			XUtil.warning(hook, "ContentProvider is null");
			return false;
		}

		// Get context
		Context context = contentProvider.getContext();
		if (context == null) {
			XUtil.warning(hook, "Context is null");
			return false;
		}

		// Get package manager
		PackageManager packageManager = context.getPackageManager();
		if (packageManager == null) {
			XUtil.warning(hook, "PackageManager is null");
			return false;
		}

		// Get process ID
		int uid = Binder.getCallingUid();
		XUtil.info(hook, "search uid=" + uid + " name=" + XUtil.getProcessNameByUid(context, uid));

		// Get package names
		String[] packageNames = packageManager.getPackagesForUid(uid);
		if (packageNames == null) {
			XUtil.warning(hook, "packageNames is null");
			return false;
		}

		// Get search package names
		String[] searchPackageNames = System.getProperty(propertyName, "").split(",");

		// Scan package names
		Boolean found = false;
		for (String packageName : packageNames)
			for (String searchPackageName : searchPackageNames) {
				Boolean equal = packageName.equals(searchPackageName);
				if (equal)
					found = true;
				XUtil.info(hook, "search package=" + packageName + (equal ? " *" : ""));
			}
		if (!found)
			for (String searchPackageName : searchPackageNames)
				XUtil.info(hook, "search package=" + searchPackageName + " ?");

		return found;
	}

	public static String getProcessNameByUid(Context context, int uid) {
		ActivityManager manager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
		if (manager != null) {
			List<RunningAppProcessInfo> lstProcess = manager.getRunningAppProcesses();
			if (lstProcess != null)
				for (RunningAppProcessInfo processInfo : lstProcess)
					if (processInfo.uid == uid)
						return processInfo.processName;
		}
		return "";
	}

	private static void log(XHook hook, String prefix, String message) {
		XposedBridge.log(String.format("XPrivacy(%s) %s %s", (hook == null ? "" : hook.getClass().getSimpleName()),
				prefix, message));
	}

	public static void info(XHook hook, String message) {
		log(hook, "Info", message);
	}

	public static void warning(XHook hook, String message) {
		log(hook, "Warning", message);
	}

	public static void error(XHook hook, String message) {
		log(hook, "Error", message);
	}
}
