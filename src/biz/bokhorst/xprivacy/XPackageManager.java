package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;
import android.content.ComponentName;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.ProviderInfo;
import android.content.pm.ResolveInfo;

public class XPackageManager extends XHook {
	private Methods mMethod;
	private String mClassName;

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
	
	// @formatter:on

	// @formatter:off
	private enum Methods {
		getInstalledApplications, getInstalledPackages,
		getPackagesHoldingPermissions,
		getPreferredActivities, getPreferredPackages,
		queryBroadcastReceivers, queryContentProviders,
		queryIntentActivities, queryIntentActivityOptions,
		queryIntentContentProviders, queryIntentServices
	};
	// @formatter:on

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods am : Methods.values())
			listHook.add(new XPackageManager(am, PrivacyManager.cSystem, className));
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
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterProviderInfo((List<ProviderInfo>) param.getResult()));

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

		return PrivacyManager.getSettingBool(-uid, Meta.cTypeApplication, packageName, false);
	}
}
