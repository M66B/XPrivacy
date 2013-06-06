package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

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

	// system/core/include/private/android_filesystem_config.h

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		Context context = getContext(param);
		String packageName = (String) param.args[0];
		String methodName = this.getMethodName();
		if (XPrivacyProvider.getRestricted(this, context, packageName, mRestrictionName, methodName, true)) {
			// Get gids
			int[] gids = (int[]) param.getResultOrThrowable();

			// Build list of gids
			List<Integer> listGids = new ArrayList<Integer>();
			for (int i = 0; i < gids.length; i++)
				if (gids[i] != sdcard_r && gids[i] != sdcard_rw)
					listGids.add(gids[i]);

			// return gids
			gids = new int[listGids.size()];
			for (int i = 0; i < listGids.size(); i++)
				gids[i] = listGids.get(i);
			param.setResult(gids);
		}
	}

	private Context getContext(MethodHookParam param) throws Throwable {
		// CM10/CM10.1
		Field fieldContext = findField(param.thisObject.getClass(), "mContext");
		return (Context) fieldContext.get(param.thisObject);
	}
}
