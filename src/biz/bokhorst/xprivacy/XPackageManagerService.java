package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XPackageManagerService extends XHook {

	public XPackageManagerService(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// public int[] getPackageGids(String packageName)
	// frameworks/base/services/java/com/android/server/pm/PackageManagerService.java

	// system/core/include/private/android_filesystem_config.h
	final static int sdcard_r = 1028;

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String packageName = (String) param.args[0];
		if (Arrays.asList(getPackageNames(param)).contains(packageName))
			if (isRestricted(param)) {
				int[] gids = (int[]) param.getResultOrThrowable();
				List<Integer> listGids = new ArrayList<Integer>();
				for (int i = 0; i < gids.length; i++)
					listGids.add(gids[i]);
				if (listGids.contains(sdcard_r)) {
					listGids.remove(sdcard_r);
					XUtil.log(this, Log.INFO, "Removed sdcard_r package=" + packageName);
				}
				gids = new int[listGids.size()];
				for (int i = 0; i < listGids.size(); i++)
					gids[i] = listGids.get(i);
				param.setResult(gids);
			}
	}

	private String[] getPackageNames(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		return context.getPackageManager().getPackagesForUid(uid);
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}
