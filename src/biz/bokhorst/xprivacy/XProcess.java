package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

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
	//     final int uid, final int gid, final int[] gids, ...
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/android/os/Process.java

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cInternet, "inet"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cInternet, "inet_admin"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cInternet, "inet_bw"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cInternet, "inet_vpn"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cInternet, "inet_mesh"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cStorage, "media"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cStorage, "sdcard"));
		listHook.add(new XProcess(Methods.startViaZygote, PrivacyManager.cStorage, "mtp"));
		return listHook;
	}

	final static int sdcard_r = 1028; // 4.1+
	final static int sdcard_rw = 1015; // 4.0+
	final static int media_rw = 1023; // 4.0+
	final static int mtp = 1024;

	final static int sdcard_pics = 1033; // 4.4+ photos
	final static int sdcard_av = 1034; // 4.4+ audio/video
	final static int sdcard_all = 1035; // 4.4+ all users

	final static int inet = 3003; // 4.0+
	final static int inet_raw = 3004; // 4.0+
	final static int inet_admin = 3005;
	final static int inet_bw_stats = 3006;
	final static int inet_bw_acct = 3007;
	final static int inet_vpn = 1016;
	final static int inet_mesh = 1030;

	// frameworks/base/data/etc/platform.xml
	// https://android.googlesource.com/platform/system/core/+/master/include/private/android_filesystem_config.h

	// http://www.doubleencore.com/2014/03/android-external-storage/
	// http://www.chainfire.eu/articles/113/Is_Google_blocking_apps_writing_to_SD_cards_/
	// https://android.googlesource.com/platform/system/core/+/dfe0cba
	// https://android.googlesource.com/platform/system/core/+/master/sdcard/sdcard.c

	@Override
	protected void before(XParam param) throws Throwable {
		switch (mMethod) {
		case startViaZygote:
			if (param.args.length >= 5 && param.args[2] instanceof Integer && param.args[4] instanceof int[]) {
				// Get IDs
				int uid = (Integer) param.args[2];
				int[] gids = (int[]) param.args[4];

				// Build list of modified gids
				List<Integer> listGids = new ArrayList<Integer>();
				for (int i = 0; i < gids.length; i++) {
					if (gids[i] == media_rw)
						if (mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("media")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking media uid=" + uid);
						else
							listGids.add(gids[i]);

					else if (gids[i] == sdcard_r || gids[i] == sdcard_rw || gids[i] == sdcard_all
							|| gids[i] == sdcard_pics || gids[i] == sdcard_av)
						if (mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("sdcard")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking sdcard uid=" + uid);
						else
							listGids.add(gids[i]);

					else if (gids[i] == mtp)
						if (mRestrictionName.equals(PrivacyManager.cStorage) && mAction.equals("mtp")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking mtp uid=" + uid);
						else
							listGids.add(gids[i]);

					else if (gids[i] == inet || gids[i] == inet_raw)
						if (mRestrictionName.equals(PrivacyManager.cInternet) && mAction.equals("inet")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking inet uid=" + uid);
						else
							listGids.add(gids[i]);

					else if (gids[i] == inet_admin)
						if (mRestrictionName.equals(PrivacyManager.cInternet) && mAction.equals("inet_admin")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking inet_admin uid=" + uid);
						else
							listGids.add(gids[i]);

					else if (gids[i] == inet_bw_stats || gids[i] == inet_bw_acct)
						if (mRestrictionName.equals(PrivacyManager.cInternet) && mAction.equals("inet_bw")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking inet_bw uid=" + uid);
						else
							listGids.add(gids[i]);

					else if (gids[i] == inet_vpn)
						if (mRestrictionName.equals(PrivacyManager.cInternet) && mAction.equals("inet_vpn")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking inet_vpn uid=" + uid);
						else
							listGids.add(gids[i]);

					else if (gids[i] == inet_mesh)
						if (mRestrictionName.equals(PrivacyManager.cInternet) && mAction.equals("inet_mesh")
								&& getRestricted(uid, mAction))
							Util.log(this, Log.INFO, "Revoking inet_mesh uid=" + uid);
						else
							listGids.add(gids[i]);

					else
						listGids.add(gids[i]);
				}

				// Proces list of modified gids
				int[] mGids = new int[listGids.size()];
				for (int i = 0; i < listGids.size(); i++)
					mGids[i] = listGids.get(i);

				param.args[4] = (mGids.length == 0 ? null : mGids);
			}
			break;
		}
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}
