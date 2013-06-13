package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;

import android.os.Binder;
import android.util.Log;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.ResolveInfo;
import android.content.pm.ProviderInfo;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XApplicationPackageManager extends XHook {

	public XApplicationPackageManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// @formatter:off

	// public List<ApplicationInfo> getInstalledApplications(int flags)
	// public List<PackageInfo> getInstalledPackages(int flags)
	// public List<PackageInfo> getInstalledThemePackages()
	// public List<PackageInfo> getPreferredPackages(int flags)
	// public List<ResolveInfo> queryBroadcastReceivers(Intent intent, int flags)
	// public List<ProviderInfo> queryContentProviders(String processName, int uid, int flags)
	// public List<ResolveInfo> queryIntentActivities(Intent intent, int flags)
	// public List<ResolveInfo> queryIntentActivityOptions(ComponentName caller, Intent[] specifics, Intent intent, int flags)
	// public List<ResolveInfo> queryIntentServices(Intent intent, int flags)
	// frameworks/base/core/java/android/app/ApplicationPackageManager.java
	
	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			String methodName = param.method.getName();
			if (methodName.equals("getInstalledApplications"))
				param.setResult(new ArrayList<ApplicationInfo>());
			else if (methodName.equals("getInstalledPackages") || methodName.equals("getInstalledThemePackages")
					|| methodName.equals("getPreferredPackages"))
				param.setResult(new ArrayList<PackageInfo>());
			else if (methodName.equals("queryBroadcastReceivers") || methodName.equals("queryIntentActivities")
					|| methodName.equals("queryIntentActivityOptions") || methodName.equals("queryIntentServices"))
				param.setResult(new ArrayList<ResolveInfo>());
			else if (methodName.equals("queryContentProviders"))
				param.setResult(new ArrayList<ProviderInfo>());
			else
				XUtil.log(this, Log.WARN, "Unknown method=" + methodName);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		// CM10/CM10.1
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}

}
