package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.os.Build;
import android.util.Log;
import android.content.ComponentName;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ProviderInfo;
import android.content.pm.ResolveInfo;

public class XPackageManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.app.ApplicationPackageManager";

	private XPackageManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public List<ApplicationInfo> getInstalledApplications(int flags)
	// public List<PackageInfo> getInstalledPackages(int flags)
	// public String[] getPackagesForUid(int uid)
	// public List<PackageInfo> getPackagesHoldingPermissions(String[] permissions, int flags)
	// abstract int getPreferredActivities(List<IntentFilter> outFilters, List<ComponentName> outActivities, String packageName)
	// public List<PackageInfo> getPreferredPackages(int flags)
	// public List<ResolveInfo> queryBroadcastReceivers(Intent intent, int flags)
	// public List<ProviderInfo> queryContentProviders(String processName, int uid, int flags)
	// public List<ResolveInfo> queryIntentActivities(Intent intent, int flags)
	// public List<ResolveInfo> queryIntentActivityOptions(ComponentName caller, Intent[] specifics, Intent intent, int flags)
	// public List<ResolveInfo> queryIntentContentProviders(Intent intent, int flags)
	// public List<ResolveInfo> queryIntentServices(Intent intent, int flags)
	// frameworks/base/core/java/android/app/ApplicationPackageManager.java
	// http://developer.android.com/reference/android/content/pm/PackageManager.html

	// public int checkPermission(String permName, String pkgName)
	// public int checkUidPermission(String permName, int uid)

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getInstalledApplications, getInstalledPackages,
		getPackagesForUid,
		getPackagesHoldingPermissions,
		getPreferredActivities, getPreferredPackages,
		queryBroadcastReceivers, queryContentProviders,
		queryIntentActivities, queryIntentActivityOptions,
		queryIntentContentProviders, queryIntentServices,

		checkPermission, checkUidPermission
	};
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			for (Methods am : Methods.values())
				if (am == Methods.checkPermission || am == Methods.checkUidPermission)
					listHook.add(new XPackageManager(am, PrivacyManager.cSystem,
							"com.android.server.pm.PackageManagerService"));
				else
					listHook.add(new XPackageManager(am, PrivacyManager.cSystem, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getInstalledApplications) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterApplicationInfo((List<ApplicationInfo>) param.getResult()));

		} else if (mMethod == Methods.getPackagesForUid) {
			if (isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.getPreferredActivities) {
			if (param.args.length > 1 && isRestricted(param)) {
				param.args[0] = new ArrayList<IntentFilter>();
				param.args[1] = new ArrayList<ComponentName>();
				param.setResult(0);
			}

		} else if (mMethod == Methods.getInstalledPackages || mMethod == Methods.getPackagesHoldingPermissions
				|| mMethod == Methods.getPreferredPackages) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterPackageInfo((List<PackageInfo>) param.getResult()));

		} else if (mMethod == Methods.queryBroadcastReceivers || mMethod == Methods.queryIntentActivities
				|| mMethod == Methods.queryIntentActivityOptions || mMethod == Methods.queryIntentContentProviders
				|| mMethod == Methods.queryIntentServices) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterResolveInfo((List<ResolveInfo>) param.getResult()));

		} else if (mMethod == Methods.queryContentProviders) {
			if (param.args.length > 0 && param.args[0] instanceof String)
				if (param.getResult() != null && isRestrictedExtra(param, (String) param.args[0]))
					param.setResult(filterProviderInfo((List<ProviderInfo>) param.getResult()));

		} else if (mMethod == Methods.checkPermission) {
			if (!PrivacyManager.getSettingBool(0, PrivacyManager.cSettingPermMan, false))
				return;

			if (param.args.length > 1 && param.args[0] instanceof String && param.args[1] instanceof String) {
				String permName = (String) param.args[0];
				String pkgName = (String) param.args[1];
				int resultOfCheck = (Integer) param.getResult();

				int uid;
				if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN) {
					// PackageInfo getPackageInfo(String packageName, int flags)
					Method mGetPackageInfo = param.thisObject.getClass().getDeclaredMethod("getPackageInfo",
							String.class, int.class);
					mGetPackageInfo.setAccessible(true);
					PackageInfo pInfo = (PackageInfo) mGetPackageInfo.invoke(param.thisObject, pkgName, 0);
					uid = pInfo.applicationInfo.uid;

				} else {
					// public int getPackageUid(String packageName, int userId)
					Method mGetPackageUid = param.thisObject.getClass().getDeclaredMethod("getPackageUid",
							String.class, int.class);
					mGetPackageUid.setAccessible(true);
					int userId = Util.getUserId(Binder.getCallingUid());
					uid = (Integer) mGetPackageUid.invoke(param.thisObject, pkgName, userId);
				}

				if (resultOfCheck == PackageManager.PERMISSION_GRANTED) {
					permName = permName.replace("android.permission.", "");
					if (isRestrictedExtra(uid, getRestrictionName(), getMethodName(), permName))
						param.setResult(PackageManager.PERMISSION_DENIED);
				}
			}

		} else if (mMethod == Methods.checkUidPermission) {
			if (!PrivacyManager.getSettingBool(0, PrivacyManager.cSettingPermMan, false))
				return;

			if (param.args.length > 1 && param.args[0] instanceof String && param.args[1] instanceof Integer) {
				String permName = (String) param.args[0];
				int uid = (Integer) param.args[1];
				int resultOfCheck = (Integer) param.getResult();

				if (resultOfCheck == PackageManager.PERMISSION_GRANTED) {
					permName = permName.replace("android.permission.", "");
					if (isRestrictedExtra(uid, getRestrictionName(), getMethodName(), permName))
						param.setResult(PackageManager.PERMISSION_DENIED);
				}
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	private List<ApplicationInfo> filterApplicationInfo(List<ApplicationInfo> original) {
		ArrayList<ApplicationInfo> result = new ArrayList<ApplicationInfo>();
		for (ApplicationInfo appInfo : original)
			if (isPackageAllowed(appInfo.packageName))
				result.add(appInfo);
		return result;
	}

	private List<PackageInfo> filterPackageInfo(List<PackageInfo> original) {
		ArrayList<PackageInfo> result = new ArrayList<PackageInfo>();
		for (PackageInfo pkgInfo : original)
			if (isPackageAllowed(pkgInfo.packageName))
				result.add(pkgInfo);
		return result;
	}

	private List<ProviderInfo> filterProviderInfo(List<ProviderInfo> original) {
		ArrayList<ProviderInfo> result = new ArrayList<ProviderInfo>();
		for (ProviderInfo provInfo : original)
			if (isPackageAllowed(provInfo.packageName))
				result.add(provInfo);
		return result;
	}

	private List<ResolveInfo> filterResolveInfo(List<ResolveInfo> original) {
		ArrayList<ResolveInfo> result = new ArrayList<ResolveInfo>();
		for (ResolveInfo resInfo : original)
			if (resInfo.activityInfo != null && resInfo.activityInfo.applicationInfo != null)
				if (isPackageAllowed(resInfo.activityInfo.applicationInfo.packageName))
					result.add(resInfo);
		return result;
	}

	public static boolean isPackageAllowed(String packageName) {
		int uid = Binder.getCallingUid();

		if (packageName == null) {
			Util.log(null, Log.WARN, "isPackageAllowed uid=" + uid + " package=" + packageName);
			Util.logStack(null, Log.WARN);
			return false;
		}

		boolean allowed = PrivacyManager.getSettingBool(-uid, Meta.cTypeApplication, packageName, false);
		boolean blacklist = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingBlacklist, false);
		if (blacklist)
			allowed = !allowed;
		return allowed;
	}
}
