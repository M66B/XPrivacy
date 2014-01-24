package biz.bokhorst.xprivacy;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Process;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

public class UpdateService extends Service {
	public static String cAction = "Action";
	public static int cActionBoot = 1;
	public static int cActionUpdated = 2;

	private static String cTodoType = "Type";
	private static int cTodoSetting = 1;
	private static int cTodoRestriction = 2;
	private static int cTodoRename = 3;

	private static String cTodoUid = "Uid";
	private static String cTodoName = "Name";
	private static String cTodoValue = "Value";
	private static String cTodoSource = "Source";
	private static String cTodoTarget = "Target";
	private static String cTodoCategory = "Category";
	private static String cTodoMethod = "Method";
	private static String cTodoRestricted = "Restricted";

	private static Thread mWorker;

	@Override
	public IBinder onBind(Intent intent) {
		return null;
	}

	@Override
	public void onTrimMemory(int level) {
		Util.log(null, Log.WARN, "Service received trim memory level=" + level);
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
			mWorker = new Thread(new Runnable() {
				@Override
				public void run() {
					try {
						// Check action
						if (action == cActionBoot) {
							// Boot received

							// Allow some restrictions for self
							PrivacyManager.setRestriction(null, Process.myUid(), PrivacyManager.cIdentification,
									"getString", false, false);
							PrivacyManager.setRestriction(null, Process.myUid(), PrivacyManager.cIPC, null, false,
									false);
							PrivacyManager.setRestriction(null, Process.myUid(), PrivacyManager.cStorage, null, false,
									false);
							PrivacyManager.setRestriction(null, Process.myUid(), PrivacyManager.cSystem, null, false,
									false);
							PrivacyManager.setRestriction(null, Process.myUid(), PrivacyManager.cView, null, false,
									false);

							// Migrate
							List<Bundle> listWork = getMigrateWork(UpdateService.this);
							if (listWork.size() > 0) {
								String format = getString(R.string.msg_migrating);
								notifyProgress(UpdateService.this, Util.NOTIFY_MIGRATE, false, format, 0);
								executeWork(UpdateService.this, listWork, Util.NOTIFY_MIGRATE, format);
								notifyProgress(UpdateService.this, Util.NOTIFY_MIGRATE, true, format, 100);

								// Use database
								PrivacyService.getClient().migrated();
							} else
								Util.log(null, Log.WARN, "No migrate work");

							// Randomize
							listWork = getRandomizeWork(UpdateService.this);
							if (listWork.size() > 0) {
								String format = getString(R.string.msg_randomizing);
								notifyProgress(UpdateService.this, Util.NOTIFY_RANDOMIZE, false, format, 0);
								executeWork(UpdateService.this, listWork, Util.NOTIFY_RANDOMIZE, format);
								notifyProgress(UpdateService.this, Util.NOTIFY_RANDOMIZE, true, format, 100);
							} else
								Util.log(null, Log.WARN, "No randomize work");

							// Done
							stopForeground(true);
							stopSelf();
						} else if (action == cActionUpdated) {
							// Self updated

							// Migrate
							List<Bundle> listWork = getUpgradeWork(UpdateService.this);
							if (listWork.size() > 0) {
								// Upgrade restriction
								String format = getString(R.string.msg_upgrading);
								notifyProgress(UpdateService.this, Util.NOTIFY_UPGRADE, false, format, 0);
								executeWork(UpdateService.this, listWork, Util.NOTIFY_MIGRATE, format);
								notifyProgress(UpdateService.this, Util.NOTIFY_UPGRADE, true, format, 100);

								// Update version
								PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
								PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingVersion, pInfo.versionName);
							} else
								Util.log(null, Log.WARN, "No upgrade work");

							// Done
							stopForeground(true);
							stopSelf();
						} else {
							Util.log(null, Log.ERROR, "Unknown action=" + action);

							// Done
							stopForeground(true);
							stopSelf();
						}
					} catch (Throwable ex) {
						Util.bug(null, ex);
					}
				}
			});
			mWorker.start();
		} else
			Util.log(null, Log.ERROR, "Action missing");

		return START_STICKY;
	}

	private static List<Bundle> getMigrateWork(Context context) {
		List<Bundle> listWork = new ArrayList<Bundle>();

		// Convert legacy settings
		try {
			PrivacyProvider.migrateLegacy(context);
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		// Migrate settings
		try {
			listWork.addAll(PrivacyProvider.migrateSettings(context));
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		// Migrate restrictions
		for (ApplicationInfo appInfo : context.getPackageManager().getInstalledApplications(0))
			try {
				listWork.addAll(PrivacyProvider.migrateApplication(context, appInfo));
			} catch (Throwable ex) {
				Util.bug(null, ex);
			}

		return listWork;
	}

	private static List<Bundle> getRandomizeWork(Context context) {
		List<Bundle> listWork = getRandomizeWork(context, 0);
		for (ApplicationInfo appInfo : context.getPackageManager().getInstalledApplications(0))
			listWork.addAll(getRandomizeWork(context, appInfo.uid));
		return listWork;
	}

	private static List<Bundle> getRandomizeWork(Context context, int uid) {
		List<Bundle> listWork = new ArrayList<Bundle>();
		if (PrivacyManager.getSettingBool(null, uid, PrivacyManager.cSettingRandom, false, true)) {
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingLatitude, PrivacyManager.getRandomProp("LAT")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingLongitude, PrivacyManager.getRandomProp("LON")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingSerial, PrivacyManager.getRandomProp("SERIAL")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingMac, PrivacyManager.getRandomProp("MAC")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingPhone, PrivacyManager.getRandomProp("PHONE")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingImei, PrivacyManager.getRandomProp("IMEI")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingId, PrivacyManager.getRandomProp("ANDROID_ID")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingGsfId, PrivacyManager.getRandomProp("GSF_ID")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingAdId,
					PrivacyManager.getRandomProp("AdvertisingId")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingCountry, PrivacyManager.getRandomProp("ISO3166")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingSubscriber,
					PrivacyManager.getRandomProp("SubscriberId")));
			listWork.add(getBundleSetting(uid, PrivacyManager.cSettingSSID, PrivacyManager.getRandomProp("SSID")));
		}
		return listWork;
	}

	private static List<Bundle> getUpgradeWork(Context context) {
		List<Bundle> listWork = new ArrayList<Bundle>();

		try {
			// Get old version
			PackageManager pm = context.getPackageManager();
			PackageInfo pInfo = pm.getPackageInfo(context.getPackageName(), 0);
			Version sVersion = new Version(PrivacyManager.getSetting(null, 0, PrivacyManager.cSettingVersion, "0.0",
					false));

			// Upgrade packages
			if (sVersion.compareTo(new Version("0.0")) == 0)
				PrivacyManager.setSetting(null, 0, PrivacyManager.cSettingVersion, pInfo.versionName);
			else {
				Util.log(null, Log.WARN, "Starting upgrade from version " + sVersion + " to version "
						+ pInfo.versionName);
				boolean dangerous = PrivacyManager.getSettingBool(null, 0, PrivacyManager.cSettingDangerous, false,
						false);
				for (ApplicationInfo appInfo : context.getPackageManager().getInstalledApplications(0))
					listWork.addAll(getUpgradeWork(sVersion, dangerous, appInfo));
			}

		} catch (Throwable ex) {
			Util.bug(null, ex);
		}

		return listWork;
	}

	private static List<Bundle> getUpgradeWork(Version sVersion, boolean dangerous, ApplicationInfo aInfo) {
		List<Bundle> listWork = new ArrayList<Bundle>();
		for (String restrictionName : PrivacyManager.getRestrictions())
			for (Hook md : PrivacyManager.getHooks(restrictionName))
				if (md.getFrom() != null)
					if (sVersion.compareTo(md.getFrom()) < 0) {
						// Disable new dangerous restrictions
						if (!dangerous && md.isDangerous()) {
							Util.log(null, Log.WARN, "Upgrading dangerous " + md + " from=" + md.getFrom() + " pkg="
									+ aInfo.packageName);
							listWork.add(getBundleRestriction(aInfo.uid, md.getRestrictionName(), md.getName(), false));
						}

						// Restrict replaced methods
						if (md.getReplaces() != null)
							if (PrivacyManager.getRestriction(null, aInfo.uid, md.getRestrictionName(),
									md.getReplaces(), false, false)) {
								Util.log(null, Log.WARN,
										"Replaced " + md.getReplaces() + " by " + md + " from=" + md.getFrom()
												+ " pkg=" + aInfo.packageName);
								listWork.add(getBundleRestriction(aInfo.uid, md.getRestrictionName(), md.getName(),
										true));
							}
					}
		return listWork;
	}

	private static void executeWork(Context context, List<Bundle> listTodo, int id, String format) {
		int pperc = -1;
		for (int i = 1; i <= listTodo.size(); i++) {
			Bundle work = listTodo.get(i - 1);
			int perc = 100 * i / listTodo.size();
			if (pperc != perc) {
				pperc = perc;
				notifyProgress(context, id, false, format, perc);
			}

			int type = work.getInt(cTodoType);
			if (type == cTodoSetting) {
				int uid = work.getInt(cTodoUid);
				String name = work.getString(cTodoName);
				String value = work.getString(cTodoValue);
				PrivacyManager.setSetting(null, uid, name, value);

			} else if (type == cTodoRestriction) {
				int uid = work.getInt(cTodoUid);
				String restrictionName = work.getString(cTodoCategory);
				String methodName = work.getString(cTodoMethod);
				boolean restricted = work.getBoolean(cTodoRestricted);
				PrivacyManager.setRestriction(null, uid, restrictionName, methodName, restricted, false);

			} else if (type == cTodoRename) {
				String source = work.getString(cTodoSource);
				String target = work.getString(cTodoTarget);
				new File(source).renameTo(new File(target));

			} else
				Util.log(null, Log.ERROR, "Unknown work type=" + type);
		}
	}

	private static void notifyProgress(Context context, int id, boolean completed, String format, int percentage) {
		String message = String.format(format, String.format("%d %%", percentage));
		Util.log(null, Log.WARN, message);

		NotificationManager notificationManager = (NotificationManager) context
				.getSystemService(Context.NOTIFICATION_SERVICE);
		NotificationCompat.Builder builder = new NotificationCompat.Builder(context);
		builder.setSmallIcon(R.drawable.ic_launcher);
		builder.setContentTitle(context.getString(R.string.app_name));
		builder.setContentText(message);
		builder.setWhen(System.currentTimeMillis());
		builder.setAutoCancel(completed);
		builder.setOngoing(!completed);
		Notification notification = builder.build();
		notificationManager.notify(id, notification);
	}

	public static Bundle getBundleRestriction(int uid, String category, String method, boolean restricted) {
		Bundle bundle = new Bundle();
		bundle.putInt(UpdateService.cTodoType, cTodoRestriction);
		bundle.putInt(UpdateService.cTodoUid, uid);
		bundle.putString(UpdateService.cTodoCategory, category);
		bundle.putString(UpdateService.cTodoMethod, method);
		bundle.putBoolean(UpdateService.cTodoRestricted, restricted);
		return bundle;
	}

	public static Bundle getBundleSetting(int uid, String name, String value) {
		Bundle bundle = new Bundle();
		bundle.putInt(cTodoType, cTodoSetting);
		bundle.putInt(cTodoUid, uid);
		bundle.putString(cTodoName, name);
		bundle.putString(cTodoValue, value);
		return bundle;
	}

	public static Bundle getBundleRename(File source, File target) {
		Bundle bundle = new Bundle();
		bundle.putInt(UpdateService.cTodoType, UpdateService.cTodoRename);
		bundle.putString(UpdateService.cTodoSource, source.getAbsolutePath());
		bundle.putString(UpdateService.cTodoTarget, target.getAbsolutePath());
		return bundle;
	}
}
