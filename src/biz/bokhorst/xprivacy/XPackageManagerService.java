package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.content.pm.PackageManager;
import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XPackageManagerService extends XHook {
	private Methods mMethod;
	private static String INTERACT_ACROSS_USERS = "android.permission.INTERACT_ACROSS_USERS";

	private XPackageManagerService(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "com.android.server.pm.PackageManagerService";
	}

	// public int checkUidPermission(String permName, int uid)
	// frameworks/base/services/java/com/android/server/pm/PackageManagerService.java

	private enum Methods {
		checkUidPermission
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1)
			listHook.add(new XPackageManagerService(Methods.checkUidPermission, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.checkUidPermission) {
			if (param.args.length >= 2) {
				String permissionName = (String) param.args[0];
				int uid = (Integer) param.args[1];
				if (INTERACT_ACROSS_USERS.equals(permissionName)) {
					// public String[] getPackagesForUid(int uid)
					Method method = param.thisObject.getClass().getDeclaredMethod("getPackagesForUid", int.class);
					String[] packageName = (String[]) method.invoke(param.thisObject, uid);
					if (packageName != null && packageName.length > 0) {
						String self = XPackageParser.class.getPackage().getName();
						if (self.equals(packageName[0])) {
							Util.log(this, Log.WARN, "Allowing single user uid=" + uid);
							param.setResult(PackageManager.PERMISSION_GRANTED);
						}
					}
				}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
