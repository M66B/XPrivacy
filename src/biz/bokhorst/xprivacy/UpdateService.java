package biz.bokhorst.xprivacy;

import java.io.File;
import java.io.IOException;
import java.util.List;

import android.app.IntentService;
import android.app.Notification;
import android.app.NotificationManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.os.Process;
import android.os.RemoteException;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

public class UpdateService extends IntentService {
	private static Thread mChangeThread;
	private static Thread mBootThread;

	public static String cAction = "Action";
	public static String cActionBoot = "Boot";
	public static String cActionChange = "Change";
	public static String cActionDone = "Done";
	public static String cStatus = "Status";
	public static String cStatusMigrated = "Migrated";

	public UpdateService() {
		super("xprivacy-update");
	}

	@Override
	protected void onHandleIntent(Intent intent) {
		Bundle extras = intent.getExtras();
		if (extras.containsKey(cAction)) {
			// Start foreground service
			NotificationCompat.Builder builder = new NotificationCompat.Builder(UpdateService.this);
			builder.setSmallIcon(R.drawable.ic_launcher);
			builder.setContentTitle(UpdateService.this.getString(R.string.app_name));
			builder.setWhen(System.currentTimeMillis());
			builder.setAutoCancel(false);
			builder.setOngoing(true);
			Notification notification = builder.build();
			startForeground(Util.NOTIFY_UPDATE, notification);

			// Check action
			if (cActionBoot.equals(extras.getString(cAction))) {
				// Boot received
				mBootThread = new Thread(new Runnable() {
					public void run() {
						boolean migrated = false;

						// Migrate settings
						try {
							migrated = migrate(UpdateService.this);
						} catch (Throwable ex) {
							Util.bug(null, ex);
						}

						// Randomize settings
						if (!migrated)
							try {
								randomize(UpdateService.this);
							} catch (Throwable ex) {
								Util.bug(null, ex);
							}

						// Signal completion
						Intent completedIntent = new Intent();
						completedIntent.setClass(UpdateService.this, UpdateService.class);
						completedIntent.putExtra(UpdateService.cAction, UpdateService.cActionDone);
						if (migrated)
							completedIntent.putExtra(UpdateService.cStatus, UpdateService.cStatusMigrated);
						UpdateService.this.startService(completedIntent);
					}
				});
				mBootThread.start();

			} else if (cActionChange.equals(extras.getString(cAction))) {
				// Package change for self
				if (PrivacyService.getClient() != null) {
					mChangeThread = new Thread(new Runnable() {
						// Upgrade restrictions
						public void run() {
							try {
								upgrade(UpdateService.this);
							} catch (Throwable ex) {
								Util.bug(null, ex);
							}

							// Signal completion
							Intent completedIntent = new Intent();
							completedIntent.setClass(UpdateService.this, UpdateService.class);
							completedIntent.putExtra(UpdateService.cAction, UpdateService.cActionDone);
							UpdateService.this.startService(completedIntent);

						}
					});
					mChangeThread.start();
				}
			}

			else if (cActionDone.equals(extras.getString(cAction))) {
				// End foreground service
				stopForeground(true);

				// Report migration complete
				if (extras.containsKey(cStatus) && cStatusMigrated.equals(extras.getString(cStatus))) {
					NotificationManager notificationManager = (NotificationManager) UpdateService.this
							.getSystemService(Context.NOTIFICATION_SERVICE);
					builder.setContentText(UpdateService.this.getString(R.string.msg_migrated));
					builder.setOngoing(false);
					builder.setAutoCancel(true);
					builder.setWhen(System.currentTimeMillis());
					notification = builder.build();
					notificationManager.notify(Util.NOTIFY_UPDATE, notification);
				}
			}
		}
	}

	private boolean migrate(final Context context) throws IOException, RemoteException {
		// Check if something to do
		boolean work = false;
		File prefs = new File(Util.getUserDataDirectory(Process.myUid()) + File.separator + "shared_prefs");
		for (File file : prefs.listFiles())
			if (!file.getName().startsWith("biz.bokhorst.xprivacy.provider.usage.") && file.getName().endsWith(".xml")
					&& !new File(file + ".migrated").exists()) {
				work = true;
				break;
			}

		// Perform migration
		if (work) {
			Util.log(null, Log.WARN, "Migration");
			String format = context.getString(R.string.msg_migrating);
			notifyProgress(context, format, 0);

			// Convert legacy settings
			PrivacyProvider.migrateLegacy(context);

			// Migrate settings
			PrivacyProvider.migrateSettings(context);

			// Migrate restrictions
			List<ApplicationInfo> listAppInfo = context.getPackageManager().getInstalledApplications(
					PackageManager.GET_META_DATA);
			for (int i = 0; i < listAppInfo.size(); i++) {
				PrivacyProvider.migrateApp(context, listAppInfo.get(i));
				notifyProgress(context, format, 100 * (i + 1) / listAppInfo.size());
			}

			Util.log(null, Log.WARN, "Migration complete");
		}

		// Use settings/restrictions
		PrivacyService.getClient().migrated();

		// Disable some restrictions for self
		// TODO: multi-user
		PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cIdentification, "getString", false, false);
		PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cIPC, null, false, false);
		PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cStorage, null, false, false);
		PrivacyManager.setRestricted(null, Process.myUid(), PrivacyManager.cView, null, false, false);

		return work;
	}

	private void randomize(Context context) {
		Util.log(null, Log.WARN, "Randomization");
		String format = context.getString(R.string.msg_randomizing);
		notifyProgress(context, format, 0);

		randomizeSettings(context, 0);

		List<ApplicationInfo> listAppInfo = context.getPackageManager().getInstalledApplications(0);
		for (int i = 0; i < listAppInfo.size(); i++) {
			randomizeSettings(context, listAppInfo.get(i).uid);
			notifyProgress(context, format, 100 * (i + 1) / listAppInfo.size());
		}
		Util.log(null, Log.WARN, "Randomization complete");
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

	private void upgradeApp(Version sVersion, boolean dangerous, ApplicationInfo aInfo) {
		for (String restrictionName : PrivacyManager.getRestrictions())
			for (PrivacyManager.Hook md : PrivacyManager.getHooks(restrictionName))
				if (md.getFrom() != null)
					if (sVersion.compareTo(md.getFrom()) < 0) {
						// Disable new dangerous restrictions
						if (!dangerous && md.isDangerous()) {
							Util.log(null, Log.WARN, "Upgrading dangerous " + md + " from=" + md.getFrom() + " pkg="
									+ aInfo.packageName);
							PrivacyManager.setRestricted(null, aInfo.uid, md.getRestrictionName(), md.getName(), false,
									true);
						}

						// Restrict replaced methods
						if (md.getReplaces() != null)
							if (PrivacyManager.getRestricted(null, aInfo.uid, md.getRestrictionName(),
									md.getReplaces(), false, false)) {
								Util.log(null, Log.WARN,
										"Replaced " + md.getReplaces() + " by " + md + " from=" + md.getFrom()
												+ " pkg=" + aInfo.packageName);
								PrivacyManager.setRestricted(null, aInfo.uid, md.getRestrictionName(), md.getName(),
										true, true);
							}
					}
	}

	private void upgrade(Context context) throws NameNotFoundException {
		// Get old version
		PackageManager pm = context.getPackageManager();
		PackageInfo pInfo = pm.getPackageInfo(context.getPackageName(), 0);
		Version sVersion = new Version(PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingVersion, "0.0", false));

		// Upgrade
		if (sVersion.compareTo(new Version("0.0")) != 0) {
			Util.log(null, Log.WARN, "Starting upgrade from version " + sVersion + " to version " + pInfo.versionName);
			String format = context.getString(R.string.msg_upgrading);
			notifyProgress(context, format, 0);

			boolean dangerous = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingDangerous, false, false);

			// All packages
			List<ApplicationInfo> listAppInfo = context.getPackageManager().getInstalledApplications(0);
			for (int i = 0; i < listAppInfo.size(); i++) {
				upgradeApp(sVersion, dangerous, listAppInfo.get(i));
				notifyProgress(context, format, 100 * (i + 1) / listAppInfo.size());
			}

			Util.log(null, Log.WARN, "Upgrade complete");
		}

		// Set new version
		PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingVersion, pInfo.versionName);
	}

	private void notifyProgress(Context context, String format, int percent) {
		NotificationManager notificationManager = (NotificationManager) context
				.getSystemService(Context.NOTIFICATION_SERVICE);
		// Build notification
		NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
		notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
		notificationBuilder.setContentTitle(context.getString(R.string.app_name));
		notificationBuilder.setContentText(String.format(format, String.format("%d %%", percent)));
		notificationBuilder.setOngoing(true);
		notificationBuilder.setWhen(System.currentTimeMillis());
		Notification notification = notificationBuilder.build();
		notificationManager.notify(Util.NOTIFY_UPDATE, notification);
	}
}
