package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Binder;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XPackageManagerService extends XHook {

	public XPackageManagerService(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// @formatter:off

	// public int[] getPackageGids(String packageName)
	// frameworks/base/services/java/com/android/server/pm/PackageManagerService.java

	// I/ActivityManager(  455): Start proc biz.bokhorst.xprivacy for content provider biz.bokhorst.xprivacy/.XPrivacyProvider: pid=548 uid=10122 gids={1006, 1028, 1015}
	// frameworks/base/services/java/com/android/server/am/ActivityManagerService.java

	// @formatter:on

	// system/core/include/private/android_filesystem_config.h
	final static int sdcard_r = 1028;

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		int uid = Binder.getCallingUid();
		String packageName = (String) param.args[0];
		XUtil.log(this, Log.INFO, "package=" + packageName + " uid=" + uid);
/*		
		if (!XPackageManagerService.class.getPackage().getName().equals(packageName))
			if (isRestricted(param))
				try {
					// Get gids
					int[] gids = (int[]) param.getResultOrThrowable();

					// Build list of gids
					List<Integer> listGids = new ArrayList<Integer>();
					for (int i = 0; i < gids.length; i++)
						if (gids[i] == sdcard_r)
							XUtil.log(this, Log.INFO, "Removed sdcard_r");
						else
							listGids.add(gids[i]);

					// return gids
					gids = new int[listGids.size()];
					for (int i = 0; i < listGids.size(); i++)
						gids[i] = listGids.get(i);
					param.setResult(gids);
				} catch (Throwable ex) {
					XUtil.bug(this, ex);
				}
*/
	}

	@Override
	protected boolean isRestricted(MethodHookParam param) throws Throwable {
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		Context context = (Context) fieldContext.get(param.thisObject);
		int uid = Binder.getCallingUid();
		return getRestricted(context, uid, true);
	}
}
