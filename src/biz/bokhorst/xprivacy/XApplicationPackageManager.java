package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;

import android.os.Binder;
import android.util.Log;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XApplicationPackageManager extends XHook {

	public XApplicationPackageManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// @formatter:off

	// public List<ApplicationInfo> getInstalledApplications(int flags)
	// public List<PackageInfo> getInstalledPackages(int flags)
	// public List<PackageInfo> getInstalledThemePackages()
	// public List<PackageInfo> getPreferredPackages(int flags)
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
		Context context = null;
		try {
			Field fieldContext = findField(param.thisObject.getClass(), "mContext");
			context = (Context) fieldContext.get(param.thisObject);
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
		}
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}
