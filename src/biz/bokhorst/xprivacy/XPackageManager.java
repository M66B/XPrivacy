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

	private XPackageManager(Methods method, String restrictionName) {
		super(restrictionName, method.name().replace("Srv_", ""), method.name());
		mMethod = method;
		mClassName = "com.android.server.pm.PackageManagerService";
	}

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

	// public java.lang.String[] getPackagesForUid(int uid)
	// public java.util.List<android.content.pm.ResolveInfo> queryIntentActivities(android.content.Intent intent, java.lang.String resolvedType, int flags, int userId)
	// public java.util.List<android.content.pm.ResolveInfo> queryIntentActivityOptions(android.content.ComponentName caller, android.content.Intent[] specifics, java.lang.String[] specificTypes, android.content.Intent intent, java.lang.String resolvedType, int flags, int userId)
	// public java.util.List<android.content.pm.ResolveInfo> queryIntentReceivers(android.content.Intent intent, java.lang.String resolvedType, int flags, int userId)
	// public java.util.List<android.content.pm.ResolveInfo> queryIntentServices(android.content.Intent intent, java.lang.String resolvedType, int flags, int userId)
	// public java.util.List<android.content.pm.ResolveInfo> queryIntentContentProviders(android.content.Intent intent, java.lang.String resolvedType, int flags, int userId)
	// public java.util.List<android.content.pm.ApplicationInfo> getPersistentApplications(int flags)
	// public java.util.List<android.content.pm.ProviderInfo> queryContentProviders(java.lang.String processName, int uid, int flags)
	// public java.util.List<android.content.pm.PackageInfo> getPreferredPackages(int flags)
	// public android.content.pm.ParceledListSlice getInstalledPackages(int flags, int userId)
	// public android.content.pm.ParceledListSlice getPackagesHoldingPermissions(java.lang.String[] permissions, int flags, int userId)
	// public android.content.pm.ParceledListSlice getInstalledApplications(int flags, int userId)
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.2_r1/com/android/server/pm/PackageManagerService.java/

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

		checkPermission, checkUidPermission,

		Srv_getInstalledApplications, Srv_getInstalledPackages,
		Srv_getPackagesForUid,
		Srv_getPackagesHoldingPermissions,
		Srv_getPersistentApplications,
		Srv_getPreferredPackages,
		Srv_queryContentProviders,
		Srv_queryIntentActivities, Srv_queryIntentActivityOptions,
		Srv_queryIntentContentProviders,
		Srv_queryIntentReceivers,
		Srv_queryIntentServices
	};
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			if (isAOSP(Build.VERSION_CODES.KITKAT)) {
				listHook.add(new XPackageManager(Methods.Srv_getInstalledApplications, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_getInstalledPackages, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_getPackagesForUid, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_getPackagesHoldingPermissions, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_getPersistentApplications, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_getPreferredPackages, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_queryContentProviders, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_queryIntentActivities, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_queryIntentActivityOptions, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_queryIntentContentProviders, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_queryIntentReceivers, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_queryIntentServices, PrivacyManager.cSystem));
			}

			listHook.add(new XPackageManager(Methods.getInstalledApplications, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.getInstalledPackages, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.getPackagesForUid, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.getPackagesHoldingPermissions, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.getPreferredActivities, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.getPreferredPackages, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.queryBroadcastReceivers, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.queryContentProviders, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.queryIntentActivities, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.queryIntentActivityOptions, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.queryIntentContentProviders, PrivacyManager.cSystem, className));
			listHook.add(new XPackageManager(Methods.queryIntentServices, PrivacyManager.cSystem, className));

			listHook.add(new XPackageManager(Methods.checkPermission, PrivacyManager.cSystem));
			listHook.add(new XPackageManager(Methods.checkUidPermission, PrivacyManager.cSystem));
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
		switch (mMethod) {
		case Srv_getInstalledApplications:
		case Srv_getInstalledPackages:
		case Srv_getPackagesForUid:
		case Srv_getPackagesHoldingPermissions:
		case Srv_getPersistentApplications:
		case Srv_getPreferredPackages:
		case Srv_queryContentProviders:
		case Srv_queryIntentActivities:
		case Srv_queryIntentActivityOptions:
		case Srv_queryIntentContentProviders:
		case Srv_queryIntentReceivers:
		case Srv_queryIntentServices:
			break;

		case getInstalledApplications:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterApplicationInfo((List<ApplicationInfo>) param.getResult()));
			break;

		case getPackagesForUid:
			if (isRestricted(param))
				param.setResult(null);
			break;

		case getPreferredActivities:
			if (param.args.length > 1 && isRestricted(param)) {
				param.args[0] = new ArrayList<IntentFilter>();
				param.args[1] = new ArrayList<ComponentName>();
				param.setResult(0);
			}
			break;

		case getInstalledPackages:
		case getPackagesHoldingPermissions:
		case getPreferredPackages:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterPackageInfo((List<PackageInfo>) param.getResult()));
			break;

		case queryBroadcastReceivers:
		case queryIntentActivities:
		case queryIntentActivityOptions:
		case queryIntentContentProviders:
		case queryIntentServices:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterResolveInfo((List<ResolveInfo>) param.getResult()));
			break;

		case queryContentProviders:
			if (param.args.length > 0 && param.args[0] instanceof String)
				if (param.getResult() != null && isRestrictedExtra(param, (String) param.args[0]))
					param.setResult(filterProviderInfo((List<ProviderInfo>) param.getResult()));
			break;

		case checkPermission:
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
			break;

		case checkUidPermission:
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
			break;

		}
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
