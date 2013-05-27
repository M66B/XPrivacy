package biz.bokhorst.xprivacy;

import android.content.Context;
import android.content.pm.PackageManager;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public abstract class XHook {
	abstract protected void before(MethodHookParam param) throws Throwable;

	abstract protected void after(MethodHookParam param) throws Throwable;

	public Boolean checkPackage(Context context, int uid, String propertyName) {
		// Get search package names
		String[] searchPackageNames = System.getProperty(propertyName, "").split(",");
		if (searchPackageNames.length == 0)
			return false;

		// Check context
		if (context == null) {
			warning("Context is null");
			return false;
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

		// Scan package names
		Boolean found = false;
		for (String packageName : packageNames)
			for (String searchPackageName : searchPackageNames) {
				Boolean equal = (searchPackageName == "*" || packageName.equals(searchPackageName));
				if (equal) {
					found = true;
					info("found package=" + packageName);
				} else
					debug("search package=" + packageName);
			}
		return found;
	}

	public void debug(String message) {
		XUtil.log(this, XUtil.LOG_DEBUG, message);
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
