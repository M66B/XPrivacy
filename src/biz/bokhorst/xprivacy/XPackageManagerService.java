package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.content.pm.ApplicationInfo;
import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.getObjectField;
import static de.robv.android.xposed.XposedHelpers.setObjectField;

public class XPackageManagerService extends XHook {
	private Methods mMethod;
	private String mRestrictionName;
	private String mAction;

	private XPackageManagerService(Methods method, String restrictionName, String action) {
		super(restrictionName, method.name(), action);
		mMethod = method;
		mRestrictionName = restrictionName;
		mAction = action;
	}

	public String getClassName() {
		return "com.android.server.pm.PackageManagerService";
	}

	private enum Methods {
		getPackageGids, grantPermissionsLPw
	};

	public boolean isVisible() {
		return (mMethod == Methods.getPackageGids);
	}

	// @formatter:off

	// public int[] getPackageGids(String packageName)
	// private void grantPermissionsLPw(PackageParser.Package pkg, boolean replace)
	// frameworks/base/services/java/com/android/server/pm/PackageManagerService.java

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XPackageManagerService(Methods.getPackageGids, PrivacyManager.cInternet, "inet"));
		listHook.add(new XPackageManagerService(Methods.getPackageGids, PrivacyManager.cStorage, "media"));
		listHook.add(new XPackageManagerService(Methods.getPackageGids, PrivacyManager.cStorage, "sdcard"));
		listHook.add(new XPackageManagerService(Methods.grantPermissionsLPw, null, null));
		return listHook;
	}

	final static int sdcard_r = 1028; // 4.1+
	final static int sdcard_rw = 1015; // 4.0+
	final static int media_rw = 1023; // 4.0+
	final static int inet = 3003; // 4.0+
	final static int inet_raw = 3004; // 4.0+

	// system/core/include/private/android_filesystem_config.h
	// frameworks/base/data/etc/platform.xml

	@Override
	@SuppressWarnings("unchecked")
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getPackageGids) {
			// Do nothing
		} else if (mMethod == Methods.grantPermissionsLPw) {
			try {
				if (param.args.length > 0 && param.args[0] != null) {
					// Get application info
					ApplicationInfo appInfo = (ApplicationInfo) getObjectField(param.args[0], "applicationInfo");

					// Get requested permissions
					ArrayList<String> requestedPermissions = (ArrayList<String>) getObjectField(param.args[0],
							"requestedPermissions");

					// Save requested permissions
					param.setObjectExtra("save_permissions", requestedPermissions);

					// Modify permissions
					ArrayList<String> modifiedPermissions = new ArrayList<String>();
					for (String requestedPermission : requestedPermissions) {
						boolean revoke = false;
						for (String restrictionName : PrivacyManager.getRestrictions()) {
							for (PrivacyManager.MethodDescription md : PrivacyManager.getMethods(restrictionName)) {
								if (md.getPermissions() != null)
									for (String permission : md.getPermissions())
										if (!"".equals(permission) && requestedPermission.endsWith(permission)) {
											boolean restricted = PrivacyManager.getRestricted(this, null, appInfo.uid,
													restrictionName, md.getName(), false, true);
											if (restricted) {
												Util.log(this, Log.INFO, "Revoking uid=" + appInfo.uid + " permission="
														+ requestedPermission);
												revoke = true;
												break;
											}
										}
								if (revoke)
									break;
							}
							if (revoke)
								break;
						}

						if (!revoke)
							modifiedPermissions.add(requestedPermission);
					}

					// Set modified permissions
					setObjectField(param.args[0], "requestedPermissions", modifiedPermissions);
				}
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	@SuppressWarnings("unchecked")
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getPackageGids) {
			// Get uid
			int uid = -1;
			try {
				// ICS: public int getPackageUid(String packageName)
				// JB+: public int getPackageUid(String packageName, int userId)
				if (param.args.length > 0) {
					String packageName = (String) param.args[0];
					if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
						Method method = param.thisObject.getClass().getMethod("getPackageUid", String.class, int.class);
						uid = (Integer) method.invoke(param.thisObject, packageName, 0);
					} else {
						Method method = param.thisObject.getClass().getMethod("getPackageUid", String.class);
						uid = (Integer) method.invoke(param.thisObject, packageName);
					}
				}
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}

			// Check if restricted
			if (uid >= 0 && getRestricted(null, uid, mAction, false)) {
				// Get group IDs
				int[] gids = (int[]) param.getResult();
				if (gids == null)
					return;

				// Build list of modified gids
				List<Integer> listGids = new ArrayList<Integer>();
				for (int i = 0; i < gids.length; i++) {
					if (gids[i] == media_rw) {
						if (!(mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("media")))
							listGids.add(gids[i]);
					} else if (gids[i] == sdcard_r || gids[i] == sdcard_rw) {
						if (!(mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("sdcard")))
							listGids.add(gids[i]);
					} else if (gids[i] == inet || gids[i] == inet_raw) {
						if (!(mRestrictionName.equals(PrivacyManager.cInternet)))
							listGids.add(gids[i]);
					} else
						listGids.add(gids[i]);
				}

				// Proces list of modified gids
				int[] mGids = new int[listGids.size()];
				for (int i = 0; i < listGids.size(); i++)
					mGids[i] = listGids.get(i);

				if (mGids.length == 0)
					param.setResult(null);
				else
					param.setResult(mGids);
			}
		} else if (mMethod == Methods.grantPermissionsLPw) {
			try {
				// Restore permissions
				ArrayList<String> requestedPermissions = (ArrayList<String>) param.getObjectExtra("save_permissions");
				if (requestedPermissions != null)
					setObjectField(param.args[0], "requestedPermissions", requestedPermissions);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
