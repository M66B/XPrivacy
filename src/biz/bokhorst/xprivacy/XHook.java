package biz.bokhorst.xprivacy;

import android.content.ContentProvider;
import android.content.Context;
import android.content.pm.PackageManager;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {
	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	public Context getContext(ContentProvider contentProvider) {
		// Check content provider
		if (contentProvider == null) {
			warning("ContentProvider is null");
			return null;
		}

		// Get context
		return contentProvider.getContext();
	}

	public Boolean isCallingPackage(Context context, int uid, String propertyName) {
		// Check context
		if (context == null) {
			warning("Context is null");
			return null;
		}

		// Get package manager
		PackageManager packageManager = context.getPackageManager();
		if (packageManager == null) {
			warning("PackageManager is null");
			return false;
		}

		// Log process
		info("search uid=" + uid + " name=" + XUtil.getProcessNameByUid(context, uid));

		// Get package names
		String[] packageNames = packageManager.getPackagesForUid(uid);
		if (packageNames == null) {
			warning("packageNames is null");
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
				if (equal)
					info("search package=" + packageName + " *");
				else
					verbose("search package=" + packageName);
			}
		if (!found)
			for (String searchPackageName : searchPackageNames)
				verbose("search package=" + searchPackageName + " ?");

		return found;
	}

	public void verbose(String message) {
		XUtil.log(this, XUtil.LOG_VERBOSE, message);
	}

	public void info(String message) {
		XUtil.log(this, XUtil.LOG_INFO, message);
	}

	public void warning(String message) {
		XUtil.log(this, XUtil.LOG_WARNING, message);
	}

	public void error(String message) {
		XUtil.log(this, XUtil.LOG_ERROR, message);
	}
}
