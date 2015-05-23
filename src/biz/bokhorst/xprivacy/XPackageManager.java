package biz.bokhorst.xprivacy;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
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

	// public PackageInfo getPackageInfo(String packageName, int flags, int userId)
	// public ApplicationInfo getApplicationInfo(String packageName, int flags, int userId)

	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/com/android/server/pm/PackageManagerService.java

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

		Srv_getPackageInfo, Srv_getApplicationInfo,
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

	public static List<XHook> getInstances(String className, boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			if (server) {
				listHook.add(new XPackageManager(Methods.Srv_getPackageInfo, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.Srv_getApplicationInfo, PrivacyManager.cSystem));
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

				listHook.add(new XPackageManager(Methods.checkPermission, PrivacyManager.cSystem));
				listHook.add(new XPackageManager(Methods.checkUidPermission, PrivacyManager.cSystem));
			} else {
				listHook.add(new XPackageManager(Methods.getInstalledApplications, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.getInstalledPackages, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.getPackagesForUid, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.getPackagesHoldingPermissions, PrivacyManager.cSystem,
						className));
				listHook.add(new XPackageManager(Methods.getPreferredActivities, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.getPreferredPackages, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.queryBroadcastReceivers, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.queryContentProviders, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.queryIntentActivities, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.queryIntentActivityOptions, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.queryIntentContentProviders, PrivacyManager.cSystem, className));
				listHook.add(new XPackageManager(Methods.queryIntentServices, PrivacyManager.cSystem, className));
			}
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.getPreferredActivities)
			if (param.args.length > 1)
				if (isRestricted(param)) {
					param.args[0] = new ArrayList<IntentFilter>();
					param.args[1] = new ArrayList<ComponentName>();
					param.setResult(0);
				}
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case Srv_getPackageInfo:
		case Srv_getApplicationInfo:
			if (param.args.length > 0 && param.args[0] instanceof String && param.getResult() != null) {
				// Allow own package name
				int uid = -1;
				if (mMethod == Methods.Srv_getPackageInfo) {
					PackageInfo pInfo = (PackageInfo) param.getResult();
					if (pInfo.applicationInfo != null)
						uid = pInfo.applicationInfo.uid;
				} else if (mMethod == Methods.Srv_getApplicationInfo) {
					ApplicationInfo aInfo = (ApplicationInfo) param.getResult();
					uid = aInfo.uid;
				}
				if (uid == Binder.getCallingUid())
					return;

				// Prevent recursion
				String packageName = (String) param.args[0];
				if (!XPackageManager.class.getPackage().getName().equals(packageName))
					if (isRestrictedExtra(param, packageName))
						if (!isPackageAllowed(uid, packageName))
							param.setResult(null);
			}
			break;

		case Srv_getInstalledApplications:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					Method mGetList = param.getResult().getClass().getDeclaredMethod("getList");
					List<ApplicationInfo> listAppInfo = (List<ApplicationInfo>) mGetList.invoke(param.getResult());
					Constructor<?> constructor = param.getResult().getClass().getConstructor(List.class);
					param.setResult(constructor.newInstance(filterApplicationInfo(listAppInfo)));
				}
			break;

		case Srv_getInstalledPackages:
		case Srv_getPackagesHoldingPermissions:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					Method mGetList = param.getResult().getClass().getDeclaredMethod("getList");
					List<PackageInfo> listPkgInfo = (List<PackageInfo>) mGetList.invoke(param.getResult());
					Constructor<?> constructor = param.getResult().getClass().getConstructor(List.class);
					param.setResult(constructor.newInstance(filterPackageInfo(listPkgInfo)));
				}
			break;

		case getPackagesForUid:
		case Srv_getPackagesForUid:
			if (param.args.length > 0 && param.args[0] instanceof Integer && param.getResult() != null) {
				int uid = (Integer) param.args[0];
				if (uid != Binder.getCallingUid())
					if (isRestrictedExtra(param, Integer.toString(uid))) {
						List<String> lstResult = new ArrayList<String>();
						if (param.getResult() instanceof String[])
							for (String packageName : (String[]) param.getResult())
								if (isPackageAllowed(Binder.getCallingUid(), packageName))
									lstResult.add(packageName);
						if (lstResult.size() == 0)
							param.setResult(null);
						else
							param.setResult(lstResult.toArray(new String[0]));
					}
			}
			break;

		case Srv_getPersistentApplications:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(filterApplicationInfo((List<ApplicationInfo>) param.getResult()));
			break;

		case Srv_getPreferredPackages:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(filterPackageInfo((List<PackageInfo>) param.getResult()));
			break;

		case Srv_queryIntentActivities:
		case Srv_queryIntentActivityOptions:
		case Srv_queryIntentContentProviders:
		case Srv_queryIntentReceivers:
		case Srv_queryIntentServices:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(filterResolveInfo((List<ResolveInfo>) param.getResult()));
			break;

		case getInstalledApplications:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(filterApplicationInfo((List<ApplicationInfo>) param.getResult()));
			break;

		case getPreferredActivities:
			break;

		case getInstalledPackages:
		case getPackagesHoldingPermissions:
		case getPreferredPackages:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(filterPackageInfo((List<PackageInfo>) param.getResult()));
			break;

		case queryBroadcastReceivers:
		case queryIntentActivities:
		case queryIntentActivityOptions:
		case queryIntentContentProviders:
		case queryIntentServices:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(filterResolveInfo((List<ResolveInfo>) param.getResult()));
			break;

		case queryContentProviders:
		case Srv_queryContentProviders:
			if (param.args.length > 1 && param.args[1] instanceof Integer && param.getResult() != null) {
				int uid = (Integer) param.args[1];
				if (uid != Binder.getCallingUid()) {
					String processName = (String) param.args[0];
					if (isRestrictedExtra(param, processName))
						param.setResult(filterProviderInfo((List<ProviderInfo>) param.getResult()));
				}
			}
			break;

		case checkPermission:
			if (param.args.length > 1 && param.args[0] instanceof String && param.args[1] instanceof String) {
				String permName = (String) param.args[0];
				String pkgName = (String) param.args[1];
				int resultOfCheck = (Integer) param.getResult();

				if (resultOfCheck != PackageManager.PERMISSION_GRANTED)
					return;

				// Get uid
				int uid;
				Class<?> clazz = param.thisObject.getClass();
				try {
					// public int getPackageUid(String packageName, int userId)
					Method mGetPackageUid = clazz.getDeclaredMethod("getPackageUid", String.class, int.class);
					mGetPackageUid.setAccessible(true);
					int userId = Util.getUserId(Binder.getCallingUid());
					uid = (Integer) mGetPackageUid.invoke(param.thisObject, pkgName, userId);
				} catch (NoSuchMethodException ignored) {
					// public int getPackageUid(String packageName)
					Method mGetPackageUid = clazz.getDeclaredMethod("getPackageUid", String.class);
					mGetPackageUid.setAccessible(true);
					uid = (Integer) mGetPackageUid.invoke(param.thisObject, pkgName);
				}

				checkPermission(param, uid, permName);
			}
			break;

		case checkUidPermission:
			if (param.args.length > 1 && param.args[0] instanceof String && param.args[1] instanceof Integer) {
				String permName = (String) param.args[0];
				int uid = (Integer) param.args[1];
				int resultOfCheck = (Integer) param.getResult();

				if (resultOfCheck == PackageManager.PERMISSION_GRANTED)
					checkPermission(param, uid, permName);
			}
			break;

		}
	}

	private void checkPermission(XParam param, int uid, String permName) throws Throwable {
		if ("android.permission.CAMERA".endsWith(permName))
			if (getRestricted(uid, PrivacyManager.cMedia, "Camera.permission"))
				param.setResult(PackageManager.PERMISSION_DENIED);

		if ("android.permission.RECORD_AUDIO".endsWith(permName))
			if (getRestricted(uid, PrivacyManager.cMedia, "Record.Audio.permission"))
				param.setResult(PackageManager.PERMISSION_DENIED);

		if ("android.permission.RECORD_VIDEO".endsWith(permName))
			if (getRestricted(uid, PrivacyManager.cMedia, "Record.Video.permission"))
				param.setResult(PackageManager.PERMISSION_DENIED);

		if (PrivacyManager.getSettingBool(0, PrivacyManager.cSettingPermMan, false)) {
			permName = permName.replace("android.permission.", "");
			if (isRestrictedExtra(uid, getRestrictionName(), getMethodName(), permName))
				param.setResult(PackageManager.PERMISSION_DENIED);
		}
	}

	private List<ApplicationInfo> filterApplicationInfo(List<ApplicationInfo> original) {
		ArrayList<ApplicationInfo> result = new ArrayList<ApplicationInfo>();
		for (ApplicationInfo appInfo : original)
			if (isPackageAllowed(appInfo.uid, appInfo.packageName))
				result.add(appInfo);
		return result;
	}

	private List<PackageInfo> filterPackageInfo(List<PackageInfo> original) {
		ArrayList<PackageInfo> result = new ArrayList<PackageInfo>();
		for (PackageInfo pkgInfo : original)
			if (isPackageAllowed(pkgInfo.applicationInfo == null ? 0 : pkgInfo.applicationInfo.uid, pkgInfo.packageName))
				result.add(pkgInfo);
		return result;
	}

	private List<ProviderInfo> filterProviderInfo(List<ProviderInfo> original) {
		ArrayList<ProviderInfo> result = new ArrayList<ProviderInfo>();
		for (ProviderInfo provInfo : original)
			if (isPackageAllowed(provInfo.applicationInfo == null ? 0 : provInfo.applicationInfo.uid,
					provInfo.packageName))
				result.add(provInfo);
		return result;
	}

	private List<ResolveInfo> filterResolveInfo(List<ResolveInfo> original) {
		ArrayList<ResolveInfo> result = new ArrayList<ResolveInfo>();
		for (ResolveInfo resInfo : original)
			if (resInfo.activityInfo != null && resInfo.activityInfo.applicationInfo != null)
				if (isPackageAllowed(resInfo.activityInfo.applicationInfo.uid,
						resInfo.activityInfo.applicationInfo.packageName))
					result.add(resInfo);
		return result;
	}

	public static boolean isPackageAllowed(int puid, String packageName) {
		int uid = Binder.getCallingUid();
		if (puid == uid)
			return true;

		if (packageName == null) {
			Util.log(null, Log.WARN, "isPackageAllowed uid=" + uid + " package=" + packageName);
			Util.logStack(null, Log.WARN);
			return false;
		}

		boolean allowed = PrivacyManager.getSettingBool(-uid, Meta.cTypeApplication, packageName, false);
		boolean blacklist = PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingBlacklist, false);
		if (blacklist)
			allowed = !allowed;
		if (allowed)
			Util.log(null, Log.INFO, "Allowing package=" + packageName);
		return allowed;
	}
}
