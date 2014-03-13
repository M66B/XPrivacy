package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.os.IBinder;
import android.os.RemoteException;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

public class UpdateService extends Service {
	public static String cAction = "Action";
	public static int cActionBoot = 1;
	public static int cActionUpdated = 2;

	private static Thread mWorkerThread;

	@Override
	public IBinder onBind(Intent intent) {
		return null;
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		// Check if work
		if (intent == null) {
			stopSelf();
			return 0;
		}

		// Check action
		Bundle extras = intent.getExtras();
		if (extras.containsKey(cAction)) {
			final int action = extras.getInt(cAction);
			Util.log(null, Log.WARN, "Service received action=" + action + " flags=" + flags);

			// Check service
			if (PrivacyService.getClient() == null) {
				Util.log(null, Log.ERROR, "Service not available");
				stopSelf();
				return 0;
			}

			// Start foreground service
			NotificationCompat.Builder builder = new NotificationCompat.Builder(UpdateService.this);
			builder.setSmallIcon(R.drawable.ic_launcher);
			builder.setContentTitle(getString(R.string.app_name));
			builder.setContentText(getString(R.string.msg_service));
			builder.setWhen(System.currentTimeMillis());
			builder.setAutoCancel(false);
			builder.setOngoing(true);
			Notification notification = builder.build();
			startForeground(Util.NOTIFY_SERVICE, notification);

			// Start worker
			mWorkerThread = new Thread(new Runnable() {
				@Override
				public void run() {
					try {
						// Check action
						if (action == cActionBoot) {
							// Boot received
							migrate(UpdateService.this);
							upgrade(UpdateService.this);
							randomize(UpdateService.this);

						} else if (action == cActionUpdated) {
							// Self updated
							upgrade(UpdateService.this);

						} else
							Util.log(null, Log.ERROR, "Unknown action=" + action);

						// Done
						stopForeground(true);
						stopSelf();
					} catch (Throwable ex) {
						Util.bug(null, ex);
						// Leave service running
					}
				}

			});
			mWorkerThread.start();
		} else
			Util.log(null, Log.ERROR, "Action missing");

		return START_STICKY;
	}

	private static void migrate(Context context) throws IOException, RemoteException {
		int first = 0;
		String format = context.getString(R.string.msg_migrating);
		List<ApplicationInfo> listApp = context.getPackageManager().getInstalledApplications(0);

		// Start migrate
		PrivacyProvider.migrateLegacy(context);

		// Migrate global settings
		PrivacyManager.setSettingList(PrivacyProvider.migrateSettings(context, 0));
		PrivacyProvider.finishMigrateSettings(0);

		// Migrate application settings/restrictions
		for (int i = 1; i <= listApp.size(); i++) {
			int uid = listApp.get(i - 1).uid;
			// Settings
			List<PSetting> listSetting = PrivacyProvider.migrateSettings(context, uid);
			PrivacyManager.setSettingList(listSetting);
			PrivacyProvider.finishMigrateSettings(uid);

			// Restrictions
			List<PRestriction> listRestriction = PrivacyProvider.migrateRestrictions(context, uid);
			PrivacyManager.setRestrictionList(listRestriction);
			PrivacyProvider.finishMigrateRestrictions(uid);

			if (first == 0)
				if (listSetting.size() > 0 || listRestriction.size() > 0)
					first = i;
			if (first > 0 && first < listApp.size())
				notifyProgress(context, Util.NOTIFY_MIGRATE, format, 100 * (i - first) / (listApp.size() - first));
		}
		if (first == 0)
			Util.log(null, Log.WARN, "Nothing to migrate");

		// Complete migration
		PrivacyService.getClient().setSetting(
				new PSetting(0, "", PrivacyManager.cSettingMigrated, Boolean.toString(true)));
	}

	private static void upgrade(Context context) throws NameNotFoundException {
		// Get previous version
		String currentVersion = Util.getSelfVersionName(context);
		Version sVersion = new Version(PrivacyManager.getSetting(0, PrivacyManager.cSettingVersion, "0.0", false));

		// Upgrade packages
		if (sVersion.compareTo(new Version("0.0")) != 0) {
			Util.log(null, Log.WARN, "Starting upgrade from version " + sVersion + " to version " + currentVersion);
			boolean dangerous = PrivacyManager.getSettingBool(0, PrivacyManager.cSettingDangerous, false, false);

			int first = 0;
			String format = context.getString(R.string.msg_upgrading);
			List<ApplicationInfo> listApp = context.getPackageManager().getInstalledApplications(0);

			for (int i = 1; i <= listApp.size(); i++) {
				int uid = listApp.get(i - 1).uid;
				List<PRestriction> listRestriction = getUpgradeWork(sVersion, dangerous, uid);
				PrivacyManager.setRestrictionList(listRestriction);

				if (first == 0)
					if (listRestriction.size() > 0)
						first = i;
				if (first > 0 && first < listApp.size())
					notifyProgress(context, Util.NOTIFY_UPGRADE, format, 100 * (i - first) / (listApp.size() - first));
			}
			if (first == 0)
				Util.log(null, Log.WARN, "Nothing to upgrade version=" + sVersion);
		} else
			Util.log(null, Log.WARN, "Noting to upgrade version=" + sVersion);

		PrivacyManager.setSetting(0, PrivacyManager.cSettingVersion, currentVersion);
	}

	private static void randomize(Context context) {
		int first = 0;
		String format = context.getString(R.string.msg_randomizing);
		List<ApplicationInfo> listApp = context.getPackageManager().getInstalledApplications(0);

		// Randomize global
		PrivacyManager.setSettingList(getRandomizeWork(context, 0));

		// Randomize applications
		for (int i = 1; i <= listApp.size(); i++) {
			int uid = listApp.get(i - 1).uid;
			List<PSetting> listSetting = getRandomizeWork(context, uid);
			PrivacyManager.setSettingList(listSetting);

			if (first == 0)
				if (listSetting.size() > 0)
					first = i;
			if (first > 0 && first < listApp.size())
				notifyProgress(context, Util.NOTIFY_RANDOMIZE, format, 100 * (i - first) / (listApp.size() - first));
		}
		if (first == 0)
			Util.log(null, Log.WARN, "Nothing to randomize");
	}

	private static List<PSetting> getRandomizeWork(Context context, int uid) {
		List<PSetting> listWork = new ArrayList<PSetting>();
		if (PrivacyManager.getSettingBool(-uid, PrivacyManager.cSettingRandom, false, true)) {
			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingLatitude))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingLatitude, PrivacyManager.getRandomProp("LAT")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingLongitude))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingLongitude, PrivacyManager
						.getRandomProp("LON")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingAltitude))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingAltitude, PrivacyManager.getRandomProp("ALT")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingSerial))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingSerial, PrivacyManager
						.getRandomProp("SERIAL")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingMac))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingMac, PrivacyManager.getRandomProp("MAC")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingPhone))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingPhone, PrivacyManager.getRandomProp("PHONE")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingImei))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingImei, PrivacyManager.getRandomProp("IMEI")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingId))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingId, PrivacyManager
						.getRandomProp("ANDROID_ID")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingGsfId))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingGsfId, PrivacyManager.getRandomProp("GSF_ID")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingAdId))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingAdId, PrivacyManager
						.getRandomProp("AdvertisingId")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingCountry))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingCountry, PrivacyManager
						.getRandomProp("ISO3166")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingSubscriber))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingSubscriber, PrivacyManager
						.getRandomProp("SubscriberId")));

			if (!hasRandomOnAccess(uid, PrivacyManager.cSettingSSID))
				listWork.add(new PSetting(uid, "", PrivacyManager.cSettingSSID, PrivacyManager.getRandomProp("SSID")));
		}
		return listWork;
	}

	private static boolean hasRandomOnAccess(int uid, String setting) {
		return PrivacyManager.cValueRandom.equals(PrivacyManager.getSetting(uid, setting, null, false));
	}

	private static List<PRestriction> getUpgradeWork(Version sVersion, boolean dangerous, int uid) {
		List<PRestriction> listWork = new ArrayList<PRestriction>();
		for (String restrictionName : PrivacyManager.getRestrictions())
			for (Hook md : PrivacyManager.getHooks(restrictionName))
				if (md.getFrom() != null)
					if (sVersion.compareTo(md.getFrom()) < 0) {
						// Disable new dangerous restrictions
						if (!dangerous && md.isDangerous()) {
							Util.log(null, Log.WARN, "Upgrading dangerous " + md + " from=" + md.getFrom() + " uid="
									+ uid);
							listWork.add(new PRestriction(uid, md.getRestrictionName(), md.getName(), false));
						}

						// Restrict replaced methods
						if (md.getReplaces() != null)
							if (PrivacyManager.getRestrictionEx(uid, md.getRestrictionName(), md.getReplaces()).restricted) {
								Util.log(null, Log.WARN,
										"Replaced " + md.getReplaces() + " by " + md + " from=" + md.getFrom()
												+ " uid=" + uid);
								listWork.add(new PRestriction(uid, md.getRestrictionName(), md.getName(), true));
							}
					}
		return listWork;
	}

	private static void notifyProgress(Context context, int id, String format, int percentage) {
		String message = String.format(format, String.format("%d %%", percentage));
		Util.log(null, Log.WARN, message);

		NotificationManager notificationManager = (NotificationManager) context
				.getSystemService(Context.NOTIFICATION_SERVICE);
		NotificationCompat.Builder builder = new NotificationCompat.Builder(context);
		builder.setSmallIcon(R.drawable.ic_launcher);
		builder.setContentTitle(context.getString(R.string.app_name));
		builder.setContentText(message);
		builder.setWhen(System.currentTimeMillis());
		builder.setAutoCancel(percentage == 100);
		builder.setOngoing(percentage < 100);
		Notification notification = builder.build();
		notificationManager.notify(id, notification);
	}
}
