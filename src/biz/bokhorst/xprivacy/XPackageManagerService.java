package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XPackageManagerService extends XHook {

	private String mRestrictionName;
	private String mAction;

	public XPackageManagerService(String methodName, String restrictionName, String[] permissions, String action) {
		super(methodName, restrictionName, permissions, action);
		mRestrictionName = restrictionName;
		mAction = action;
	}

	// public int[] getPackageGids(String packageName)
	// frameworks/base/services/java/com/android/server/pm/PackageManagerService.java

	final static int sdcard_r = 1028; // 4.1+
	final static int sdcard_rw = 1015; // 4.0+
	final static int media_rw = 1023; // 4.0+
	final static int inet = 3003; // 4.0+
	final static int inet_raw = 3004; // 4.0+

	// system/core/include/private/android_filesystem_config.h
	// frameworks/base/data/etc/platform.xml

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Get group IDs
		int[] gids = (int[]) param.getResult();
		if (gids != null) {
			// Build list of group IDs
			boolean modified = false;
			List<Integer> listGids = new ArrayList<Integer>();
			for (int i = 0; i < gids.length; i++)
				if (gids[i] == media_rw) {
					if (mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("media")) {
						modified = true;
						Util.log(this, Log.INFO, "Deny media_rw restriction=" + mRestrictionName + " action=" + mAction);
					}
				} else if (gids[i] == sdcard_r || gids[i] == sdcard_rw) {
					if (mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("sdcard")) {
						modified = true;
						Util.log(this, Log.INFO, "Deny sdcard_r/sdcard_rw restriction=" + mRestrictionName + " action="
								+ mAction);
					}
				} else if (gids[i] == inet || gids[i] == inet_raw) {
					if (mRestrictionName.equals(PrivacyManager.cInternet)) {
						modified = true;
						Util.log(this, Log.INFO, "Deny inet/inet_raw restriction=" + mRestrictionName + " action="
								+ mAction);
					}
				} else
					listGids.add(gids[i]);

			if (modified) {
				// Build list of modified gids
				gids = new int[listGids.size()];
				for (int i = 0; i < listGids.size(); i++)
					gids[i] = listGids.get(i);

				// Get uid
				int uid = -1;
				try {
					// ICS: public int getPackageUid(String packageName)
					// public int getPackageUid(String packageName, int userId)
					String packageName = (String) param.args[0];
					if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
						Method method = param.thisObject.getClass().getMethod("getPackageUid", String.class, int.class);
						uid = (Integer) method.invoke(param.thisObject, packageName, 0);
					} else {
						Method method = param.thisObject.getClass().getMethod("getPackageUid", String.class);
						uid = (Integer) method.invoke(param.thisObject, packageName);
					}
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}

				if (uid >= 0 && getRestricted(null, uid, mAction, false))
					param.setResult(gids);
			}
		}
	}
}
