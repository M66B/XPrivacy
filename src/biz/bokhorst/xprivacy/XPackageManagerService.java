package biz.bokhorst.xprivacy;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.os.Build;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.getObjectField;
import static de.robv.android.xposed.XposedHelpers.setObjectField;
import static de.robv.android.xposed.XposedHelpers.callMethod;

public class XPackageManagerService extends XHook {
	private Methods mMethod;
	private String mRestrictionName;
	private String mAction;

	private static String cPackageName = "PackageName";
	private static String cKill = "Kill";

	private static String SAVED_PERMISIONS = "saved_permissions";
	private static String ACTION_MANAGE_PACKAGE = "biz.bokhorst.xprivacy.ACTION_MANAGE_PACKAGE";
	private static String PERMISSION_MANAGE_PACKAGES = "biz.bokhorst.xprivacy.MANAGE_PACKAGES";

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
		getPackageGids, grantPermissionsLPw, systemReady
	};

	public boolean isVisible() {
		return (mMethod != Methods.grantPermissionsLPw);
	}

	// @formatter:off

	// public int[] getPackageGids(String packageName)
	// private void grantPermissionsLPw(PackageParser.Package pkg, boolean replace)
	// public void systemReady()
	// frameworks/base/services/java/com/android/server/pm/PackageManagerService.java

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XPackageManagerService(Methods.getPackageGids, PrivacyManager.cInternet, "inet"));
		listHook.add(new XPackageManagerService(Methods.getPackageGids, PrivacyManager.cStorage, "media"));
		listHook.add(new XPackageManagerService(Methods.getPackageGids, PrivacyManager.cStorage, "sdcard"));
		listHook.add(new XPackageManagerService(Methods.grantPermissionsLPw, null, null));
		listHook.add(new XPackageManagerService(Methods.systemReady, null, null));
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
			// Revoke permissions
			try {
				boolean experimental = PrivacyManager.getSettingBool(this, null, 0,
						PrivacyManager.cSettingExperimental, false, true);
				if (experimental)
					if (param.args.length > 0 && param.args[0] != null) {
						// Get application info
						ApplicationInfo appInfo = (ApplicationInfo) getObjectField(param.args[0], "applicationInfo");

						// Get requested permissions
						ArrayList<String> listPermission = (ArrayList<String>) getObjectField(param.args[0],
								"requestedPermissions");

						// Save requested permissions
						param.setObjectExtra(SAVED_PERMISIONS, listPermission);

						// Modify permissions
						ArrayList<String> listModified = new ArrayList<String>();
						for (String permission : listPermission)
							if (PrivacyManager.getRestricted(this, appInfo.uid, permission))
								Util.log(this, Log.INFO, "Revoking uid=" + appInfo.uid + " permission=" + permission);
							else
								listModified.add(permission);

						// Set modified permissions
						setObjectField(param.args[0], "requestedPermissions", listModified);
					}
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		} else if (mMethod == Methods.systemReady) {
			// Do nothing
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
				ArrayList<String> listPermission = (ArrayList<String>) param.getObjectExtra(SAVED_PERMISIONS);
				if (listPermission != null)
					setObjectField(param.args[0], "requestedPermissions", listPermission);
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		} else if (mMethod == Methods.systemReady) {
			// Prepare package management
			Context context = (Context) getObjectField(param.thisObject, "mContext");
			if (context == null)
				Util.log(this, Log.ERROR, "Context is null");
			else
				context.registerReceiver(new Receiver(param.thisObject), new IntentFilter(ACTION_MANAGE_PACKAGE),
						PERMISSION_MANAGE_PACKAGES, null);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	public static void manage(Context context, String packageName, boolean kill) {
		Util.log(null, Log.WARN, "Manage package=" + packageName + " kill=" + kill);
		Intent applyIntent = new Intent(XPackageManagerService.ACTION_MANAGE_PACKAGE);
		applyIntent.putExtra(cPackageName, packageName);
		applyIntent.putExtra(cKill, true);
		context.sendBroadcast(applyIntent, XPackageManagerService.PERMISSION_MANAGE_PACKAGES);
	}

	private class Receiver extends BroadcastReceiver {
		private Object mPms;

		public Receiver(Object pms) {
			mPms = pms;
		}

		@Override
		@SuppressWarnings("unchecked")
		public void onReceive(Context context, Intent intent) {
			try {
				String packageName = intent.getExtras().getString(cPackageName);
				boolean kill = intent.getExtras().getBoolean(cKill, false);
				Util.log(null, Log.WARN, "Managing package=" + packageName + " kill=" + kill);

				// final HashMap<String, PackageParser.Package> mPackages
				// final Settings mSettings;

				// Update permissions
				Object pkgInfo;
				Map<String, Object> mapPackages = (Map<String, Object>) getObjectField(mPms, "mPackages");
				Object mSettings = getObjectField(mPms, "mSettings");
				synchronized (mapPackages) {
					pkgInfo = mapPackages.get(packageName);
					callMethod(mPms, "grantPermissionsLPw", pkgInfo, true);
					callMethod(mSettings, "writeLPr");
				}

				// Kill application
				if (kill) {
					try {
						ApplicationInfo appInfo = (ApplicationInfo) getObjectField(pkgInfo, "applicationInfo");
						if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.JELLY_BEAN_MR2)
							callMethod(mPms, "killApplication", packageName, appInfo.uid);
						else
							callMethod(mPms, "killApplication", packageName, appInfo.uid, "apply App Settings");
					} catch (Throwable ex) {
						Util.bug(null, ex);
					}
				}
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}
		}
	}
}
