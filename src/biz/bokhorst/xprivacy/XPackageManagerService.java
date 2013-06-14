package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XPackageManagerService extends XHook {

	private String mRestrictionName;

	public XPackageManagerService(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
		mRestrictionName = restrictionName;
	}

	// public int[] getPackageGids(String packageName)
	// frameworks/base/services/java/com/android/server/pm/PackageManagerService.java

	final static int sdcard_r = 1028;
	final static int sdcard_rw = 1015;
	final static int inet = 3003;
	final static int inet_raw = 3004;

	// system/core/include/private/android_filesystem_config.h
	// frameworks/base/data/etc/platform.xml

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Get argument
		String packageName = (String) param.args[0];

		// public int getPackageUid(String packageName, int userId)
		Method method = param.thisObject.getClass().getMethod("getPackageUid", String.class, int.class);
		int uid = (Integer) method.invoke(param.thisObject, packageName, 0);

		if (getRestricted(null, uid, true)) {
			// Get gids
			int[] gids = (int[]) param.getResult();
			if (gids != null) {
				// Build list of gids
				List<Integer> listGids = new ArrayList<Integer>();
				for (int i = 0; i < gids.length; i++)
					if ((gids[i] == sdcard_r || gids[i] == sdcard_rw) && mRestrictionName.equals(XRestriction.cStorage))
						;
					else if ((gids[i] == inet || gids[i] == inet_raw)
							&& mRestrictionName.equals(XRestriction.cInternet))
						;
					else
						listGids.add(gids[i]);

				// return gids
				gids = new int[listGids.size()];
				for (int i = 0; i < listGids.size(); i++)
					gids[i] = listGids.get(i);
				param.setResult(gids);
			}
		}
	}
}
