package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.os.Build;
import android.util.Log;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.ProviderInfo;
import android.content.pm.ResolveInfo;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

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
	// public List<PackageInfo> getPreferredPackages(int flags)
	// public List<ResolveInfo> queryBroadcastReceivers(Intent intent, int flags)
	// public List<ProviderInfo> queryContentProviders(String processName, int uid, int flags)
	// public List<ResolveInfo> queryIntentActivities(Intent intent, int flags)
	// public List<ResolveInfo> queryIntentActivityOptions(ComponentName caller, Intent[] specifics, Intent intent, int flags)
	// public List<ResolveInfo> queryIntentContentProviders(Intent intent, int flags)
	// public List<ResolveInfo> queryIntentServices(Intent intent, int flags)
	// frameworks/base/core/java/android/app/ApplicationPackageManager.java
	
	// @formatter:on

	private enum Methods {
		getInstalledApplications, getInstalledPackages, getPackagesHoldingPermissions, getPreferredPackages, queryBroadcastReceivers, queryContentProviders, queryIntentActivities, queryIntentActivityOptions, queryIntentContentProviders, queryIntentServices
	};

	public static List<XHook> getInstances(Object instance) {
		String className = instance.getClass().getName();
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods am : Methods.values())
			if (am == Methods.getPackagesHoldingPermissions) {
				if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2)
					listHook.add(new XPackageManager(am, PrivacyManager.cSystem, className));
			} else if (am == Methods.queryIntentContentProviders) {
				if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
					listHook.add(new XPackageManager(am, PrivacyManager.cSystem, className));
			} else
				listHook.add(new XPackageManager(am, PrivacyManager.cSystem, className));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getInstalledApplications) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(filterApplicationInfo((List<ApplicationInfo>) param.getResult()));
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
		for (PackageInfo appInfo : original)
			if (isPackageAllowed(appInfo.packageName))
				result.add(appInfo);
		return result;
	}

	private List<ProviderInfo> filterProviderInfo(List<ProviderInfo> original) {
		ArrayList<ProviderInfo> result = new ArrayList<ProviderInfo>();
		for (ProviderInfo appInfo : original)
			if (isPackageAllowed(appInfo.packageName))
				result.add(appInfo);
		return result;
	}

	private List<ResolveInfo> filterResolveInfo(List<ResolveInfo> original) {
		ArrayList<ResolveInfo> result = new ArrayList<ResolveInfo>();
		for (ResolveInfo appInfo : original)
			if (isPackageAllowed(appInfo.resolvePackageName))
				result.add(appInfo);
		return result;
	}

	public static boolean isPackageAllowed(String packageName) {
		int uid = Binder.getCallingUid();
		return PrivacyManager.getSettingBool(null, uid, PrivacyManager.cSettingApplication + packageName, false, true);
	}
}
