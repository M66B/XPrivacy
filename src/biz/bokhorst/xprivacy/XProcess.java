package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XProcess extends XHook {
	private Methods mMethod;
	private String mRestrictionName;
	private String mAction;

	private XProcess(Methods method, String restrictionName, String action) {
		super(restrictionName, method.name(), action);
		mMethod = method;
		mRestrictionName = restrictionName;
		mAction = action;
	}

	public String getClassName() {
		return "android.os.Process";
	}

	public boolean isVisible() {
		return false;
	}

	private enum Methods {
		startViaZygote
	};

	// @formatter:off

	// private static ProcessStartResult startViaZygote(
	//     final String processClass, final String niceName,
	//     final int uid, final int gid, final int[] gids,
	// frameworks/base/core/java/android/os/Process.java

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cInternet, "inet"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cStorage, "media"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cStorage, "sdcard"));
		return listHook;
	}

	final static int sdcard_r = 1028; // 4.1+
	final static int sdcard_rw = 1015; // 4.0+
	final static int media_rw = 1023; // 4.0+

	final static int sdcard_pics = 1033; // 4.4+ photos
	final static int sdcard_av = 1034; // 4.4+ audio/video
	final static int sdcard_all = 1035; // 4.4+ all users

	final static int inet = 3003; // 4.0+
	final static int inet_raw = 3004; // 4.0+

	// system/core/include/private/android_filesystem_config.h
	// frameworks/base/data/etc/platform.xml

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.startViaZygote) {
			if (param.args.length >= 5) {
				// Check if restricted
				int uid = (Integer) param.args[2];
				if (getRestricted(uid, mAction)) {
					// Get group IDs
					int[] gids = (int[]) param.args[4];
					if (gids == null)
						return;

					// Build list of modified gids
					List<Integer> listGids = new ArrayList<Integer>();
					for (int i = 0; i < gids.length; i++) {
						if (gids[i] == media_rw)
							if (!(mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("media")))
								listGids.add(gids[i]);
							else
								Util.log(this, Log.INFO, "Revoking media_rw uid=" + uid);

						else if (gids[i] == sdcard_r || gids[i] == sdcard_rw || gids[i] == sdcard_pics
								|| gids[i] == sdcard_av || gids[i] == sdcard_all)
							if (!(mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("sdcard")))
								listGids.add(gids[i]);
							else
								Util.log(this, Log.INFO, "Revoking sdcard_rw uid=" + uid);

						else if (gids[i] == inet || gids[i] == inet_raw)
							if (!(mRestrictionName.equals(PrivacyManager.cInternet)))
								listGids.add(gids[i]);
							else
								Util.log(this, Log.INFO, "Revoking inet_raw uid=" + uid);

						else
							listGids.add(gids[i]);
					}

					// Proces list of modified gids
					int[] mGids = new int[listGids.size()];
					for (int i = 0; i < listGids.size(); i++)
						mGids[i] = listGids.get(i);

					param.args[4] = (mGids.length == 0 ? null : mGids);
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
