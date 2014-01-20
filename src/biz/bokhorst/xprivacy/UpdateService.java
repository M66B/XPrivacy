package biz.bokhorst.xprivacy;

import android.app.IntentService;
import android.app.Notification;
import android.app.NotificationManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Process;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

public class UpdateService extends IntentService {
	private static Thread mChangeThread;
	private static Thread mBootThread;

	public static String cAction = "Action";
	public static String cActionBoot = "ActionBoot";
	public static String cActionChange = "ActionChange";

	public UpdateService() {
		super("xprivacy-update");
	}

	@Override
	protected void onHandleIntent(Intent intent) {
		Bundle extras = intent.getExtras();
		if (extras.containsKey(cAction))
			if (cActionBoot.equals(extras.getString(cAction))) {
				// Boot received
				mBootThread = new Thread(new Runnable() {
					public void run() { // Build notification
						NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(
								getApplicationContext());
						notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
						notificationBuilder.setContentTitle(getApplicationContext().getString(R.string.app_name));
						notificationBuilder.setContentText(String.format(
								getApplicationContext().getString(R.string.msg_migrating), "0 %"));
						notificationBuilder.setWhen(System.currentTimeMillis());
						notificationBuilder.setAutoCancel(true);
						Notification notification = notificationBuilder.build();

						// Migrate settings
						try {
							Util.log(null, Log.WARN, "Start fg");
							startForeground(Util.NOTIFY_MIGRATING, notification);
							migrate(getApplicationContext());
							Thread.sleep(5 * 1000);
							Util.log(null, Log.WARN, "End fg");
							stopForeground(true);
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}

						// Randomize settings
						randomizeSettings(getApplicationContext(), 0);
						for (ApplicationInfo aInfo : getApplicationContext().getPackageManager()
								.getInstalledApplications(0))
							randomizeSettings(getApplicationContext(), aInfo.uid);
					}
				});
				mBootThread.start();

			} else if (cActionChange.equals(extras.getString(cAction))) {
				// Package change for self
				if (PrivacyService.getClient() != null) {
					mChangeThread = new Thread(new Runnable() {
						// Upgrade restrictions
						public void run() {
							// Build notification
							NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(
									getApplicationContext());
							notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
							notificationBuilder.setContentTitle(getApplicationContext().getString(R.string.app_name));
							notificationBuilder.setContentText(String.format(
									getApplicationContext().getString(R.string.msg_upgrading), "0 %"));
							notificationBuilder.setWhen(System.currentTimeMillis());
							notificationBuilder.setAutoCancel(true);
							Notification notification = notificationBuilder.build();

							try {
								// https://code.google.com/p/android/issues/detail?id=21635
								Util.log(null, Log.WARN, "Start fg");
								startForeground(Util.NOTIFY_UPGRADING, notification);
								upgradeRestrictions(getApplicationContext());
								Thread.sleep(5 * 1000);
								Util.log(null, Log.WARN, "End fg");
								stopForeground(true);
							} catch (Throwable ex) {
								Util.bug(null, ex);
							}
						}
					});
					mChangeThread.start();
				}
			}
	}

	private void randomizeSettings(Context context, int uid) {
		boolean random = PrivacyManager.getSettingBool(null, uid, PrivacyManager.cSettingRandom, false, false);
		if (random) {
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingLatitude, PrivacyManager.getRandomProp("LAT"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingLongitude, PrivacyManager.getRandomProp("LON"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingSerial, PrivacyManager.getRandomProp("SERIAL"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingMac, PrivacyManager.getRandomProp("MAC"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingPhone, PrivacyManager.getRandomProp("PHONE"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingImei, PrivacyManager.getRandomProp("IMEI"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingId, PrivacyManager.getRandomProp("ANDROID_ID"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingGsfId, PrivacyManager.getRandomProp("GSF_ID"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingAdId,
					PrivacyManager.getRandomProp("AdvertisingId"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingCountry,
					PrivacyManager.getRandomProp("ISO3166"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingSubscriber,
					PrivacyManager.getRandomProp("SubscriberId"));
			PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingSSID, PrivacyManager.getRandomProp("SSID"));
		}
	}

	private void migrate(final Context context) {
		try {
			// Disable some restrictions for self
			PrivacyManager.setRestricted(null, PrivacyManager.cAndroidUid, PrivacyManager.cIdentification, "/proc",
					false, false);
			PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cIdentification, "getString", false,
					false);
			PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cIPC, null, false, false);
			PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cStorage, null, false, false);
			PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cView, null, false, false);

			// Migrate xml settings files
			boolean migrated = PrivacyProvider.migrateRestrictions(context);
			migrated = PrivacyProvider.migrateSettings(context) || migrated;
			PrivacyService.getClient().migrated();
			Util.log(null, Log.WARN, "Migration complete");

			// Build notification
			if (migrated) {
				NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
				notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
				notificationBuilder.setContentTitle(context.getString(R.string.app_name));
				notificationBuilder.setContentText(context.getString(R.string.msg_migrated));
				notificationBuilder.setWhen(System.currentTimeMillis());
				notificationBuilder.setAutoCancel(true);
				Notification notification = notificationBuilder.build();

				// Display notification
				NotificationManager notificationManager = (NotificationManager) context
						.getSystemService(Context.NOTIFICATION_SERVICE);
				notificationManager.notify(99, notification);
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

	private void upgradeRestrictions(Context context) {
		try {
			// Get old version
			PackageManager pm = context.getPackageManager();
			PackageInfo pInfo = pm.getPackageInfo(context.getPackageName(), 0);
			Version sVersion = new Version(PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingVersion, "0.0",
					false));

			// Upgrade
			if (sVersion.compareTo(new Version("0.0")) != 0) {
				Util.log(null, Log.WARN, "Starting upgrade from version " + sVersion + " to version "
						+ pInfo.versionName);
				boolean dangerous = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingDangerous, false,
						false);

				// All packages
				for (ApplicationInfo aInfo : pm.getInstalledApplications(0))
					for (String restrictionName : PrivacyManager.getRestrictions())
						for (PrivacyManager.Hook md : PrivacyManager.getHooks(restrictionName))
							if (md.getFrom() != null)
								if (sVersion.compareTo(md.getFrom()) < 0) {
									// Disable new dangerous restrictions
									if (!dangerous && md.isDangerous()) {
										Util.log(null, Log.WARN, "Upgrading dangerous " + md + " from=" + md.getFrom()
												+ " pkg=" + aInfo.packageName);
										PrivacyManager.setRestricted(null, aInfo.uid, md.getRestrictionName(),
												md.getName(), false, true);
									}

									// Restrict replaced methods
									if (md.getReplaces() != null)
										if (PrivacyManager.getRestricted(null, aInfo.uid, md.getRestrictionName(),
												md.getReplaces(), false, false)) {
											Util.log(null, Log.WARN, "Replaced " + md.getReplaces() + " by " + md
													+ " from=" + md.getFrom() + " pkg=" + aInfo.packageName);
											PrivacyManager.setRestricted(null, aInfo.uid, md.getRestrictionName(),
													md.getName(), true, true);
										}
								}

				Util.log(null, Log.WARN, "Upgrade done");
			}

			// Set new version
			PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingVersion, pInfo.versionName);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}

}
