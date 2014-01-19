package biz.bokhorst.xprivacy;

import java.util.List;

import android.annotation.SuppressLint;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.support.v4.app.NotificationCompat;
import android.text.TextUtils;
import android.util.Log;

public class PackageChange extends BroadcastReceiver {

	@Override
	@SuppressLint("NewApi")
	public void onReceive(final Context context, Intent intent) {
		// Check privacy service client
		if (PrivacyService.getClient() == null)
			return;

		try {
			// Check uri
			Uri inputUri = Uri.parse(intent.getDataString());
			if (inputUri.getScheme().equals("package")) {
				// Get data
				int uid = intent.getIntExtra(Intent.EXTRA_UID, 0);
				boolean replacing = intent.getBooleanExtra(Intent.EXTRA_REPLACING, false);
				NotificationManager notificationManager = (NotificationManager) context
						.getSystemService(Context.NOTIFICATION_SERVICE);

				Util.log(null, Log.INFO, "Package change action=" + intent.getAction() + " replacing=" + replacing
						+ " uid=" + uid);

				// Check action
				if (intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED)) {
					// Get data
					ApplicationInfoEx appInfo = new ApplicationInfoEx(context, uid);

					// Default deny new user apps
					if (!replacing) {
						// Delete any existing restrictions
						PrivacyManager.deleteRestrictions(uid, true);
						PrivacyManager.deleteSettings(uid);
						PrivacyManager.deleteUsage(uid);

						// Restrict new non-system apps
						if (!appInfo.isSystem())
							for (String restrictionName : PrivacyManager.getRestrictions()) {
								String templateName = PrivacyManager.cSettingTemplate + "." + restrictionName;
								if (PrivacyManager.getSettingBool(null, 0, templateName, true, false))
									PrivacyManager.setRestricted(null, uid, restrictionName, null, true, true);
							}
					}

					// New/update notification
					if (!replacing
							|| PrivacyManager.getSettingBool(null, uid, PrivacyManager.cSettingNotify, true, false)) {
						Intent resultIntent = new Intent(Intent.ACTION_MAIN);
						resultIntent.putExtra(ActivityApp.cUid, uid);
						resultIntent.setClass(context.getApplicationContext(), ActivityApp.class);

						// Build pending intent
						PendingIntent pendingIntent = PendingIntent.getActivity(context, uid, resultIntent,
								PendingIntent.FLAG_UPDATE_CURRENT);

						// Build result intent settings
						Intent resultIntentSettings = new Intent(Intent.ACTION_MAIN);
						resultIntentSettings.putExtra(ActivityApp.cUid, uid);
						resultIntentSettings.putExtra(ActivityApp.cAction, ActivityApp.cActionSettings);
						resultIntentSettings.setClass(context.getApplicationContext(), ActivityApp.class);

						// Build pending intent settings
						PendingIntent pendingIntentSettings = PendingIntent.getActivity(context, uid - 10000,
								resultIntentSettings, PendingIntent.FLAG_UPDATE_CURRENT);

						// Build result intent clear
						Intent resultIntentClear = new Intent(Intent.ACTION_MAIN);
						resultIntentClear.putExtra(ActivityApp.cUid, uid);
						resultIntentClear.putExtra(ActivityApp.cAction, ActivityApp.cActionClear);
						resultIntentClear.setClass(context.getApplicationContext(), ActivityApp.class);

						// Build pending intent clear
						PendingIntent pendingIntentClear = PendingIntent.getActivity(context, uid + 10000,
								resultIntentClear, PendingIntent.FLAG_UPDATE_CURRENT);

						// Title
						String title = null;
						NotificationCompat.InboxStyle inboxStyle = null;
						if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
							inboxStyle = new NotificationCompat.InboxStyle();
							inboxStyle.setBigContentTitle(context.getString(replacing ? R.string.msg_update
									: R.string.msg_new));

							List<String> names = appInfo.getApplicationName();
							List<String> versions = appInfo.getPackageVersionName(context);
							for (int i = 0; i < Math.min(4, names.size()); i++)
								inboxStyle.addLine(String.format("%s %s", names.get(i), versions.get(i)));
							if (names.size() > 4)
								inboxStyle.addLine("...");

							inboxStyle.setSummaryText(context.getString(R.string.msg_applied));
						} else {
							title = String.format("%s %s %s",
									context.getString(replacing ? R.string.msg_update : R.string.msg_new),
									TextUtils.join(", ", appInfo.getApplicationName()),
									TextUtils.join(", ", appInfo.getPackageVersionName(context)));
							if (!replacing)
								title = String.format("%s %s", title, context.getString(R.string.msg_applied));
						}

						// Build notification
						NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
						notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
						notificationBuilder.setContentTitle(context.getString(R.string.app_name));
						if (title != null)
							notificationBuilder.setContentText(title);
						if (inboxStyle != null)
							notificationBuilder.setStyle(inboxStyle);
						notificationBuilder.setContentIntent(pendingIntent);
						notificationBuilder.setWhen(System.currentTimeMillis());
						notificationBuilder.setAutoCancel(true);

						// Actions
						notificationBuilder.addAction(android.R.drawable.ic_menu_edit,
								context.getString(R.string.menu_app_settings), pendingIntentSettings);
						notificationBuilder.addAction(android.R.drawable.ic_menu_delete,
								context.getString(R.string.menu_clear), pendingIntentClear);

						// Notify
						Notification notification = notificationBuilder.build();
						notificationManager.notify(appInfo.getUid(), notification);
					}

					// Mark as new/changed
					PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingState,
							Integer.toString(ActivityMain.STATE_ATTENTION));
				} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REPLACED)) {
					// Notify reboot required
					String packageName = inputUri.getSchemeSpecificPart();
					if (packageName.equals(context.getPackageName())) {
						// Build notification
						NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
						notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
						notificationBuilder.setContentTitle(context.getString(R.string.app_name));
						notificationBuilder.setContentText(context.getString(R.string.msg_reboot));
						notificationBuilder.setWhen(System.currentTimeMillis());
						notificationBuilder.setAutoCancel(true);
						Notification notification = notificationBuilder.build();

						// Notify
						notificationManager.notify(0, notification);

						// Upgrade restrictions
						new Thread(new Runnable() {
							public void run() {
								upgradeRestrictions(context);
							}
						}).start();
					}
				} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REMOVED) && !replacing) {
					// Package removed
					notificationManager.cancel(uid);
					PrivacyManager.deleteRestrictions(uid, true);
					PrivacyManager.deleteSettings(uid);
					PrivacyManager.deleteUsage(uid);
				}
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
