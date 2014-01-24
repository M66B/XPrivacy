package biz.bokhorst.xprivacy;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

public class PackageChange extends BroadcastReceiver {
	@Override
	public void onReceive(final Context context, Intent intent) {
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
					String packageName = inputUri.getSchemeSpecificPart();

					// Default deny new user apps
					if (PrivacyService.getClient() != null) {
						if (!replacing) {
							// Delete existing restrictions
							PrivacyManager.deleteRestrictions(uid, true);
							PrivacyManager.deleteSettings(uid);
							PrivacyManager.deleteUsage(uid);

							// Restrict new non-system apps
							if (!appInfo.isSystem())
								for (String restrictionName : PrivacyManager.getRestrictions()) {
									String templateName = PrivacyManager.cSettingTemplate + "." + restrictionName;
									if (PrivacyManager.getSettingBool(null, 0, templateName, true, false))
										PrivacyManager.setRestriction(null, uid, restrictionName, null, true, true);
								}
						}

						// Mark as new/changed
						PrivacyManager.setSetting(null, uid, PrivacyManager.cSettingState,
								Integer.toString(ActivityMain.STATE_ATTENTION));
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
						String title = String.format("%s %s %s",
								context.getString(replacing ? R.string.msg_update : R.string.msg_new),
								appInfo.getApplicationName(packageName),
								appInfo.getPackageVersionName(context, packageName));
						if (!replacing)
							title = String.format("%s %s", title, context.getString(R.string.msg_applied));

						// Build notification
						NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
						notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
						notificationBuilder.setContentTitle(context.getString(R.string.app_name));
						notificationBuilder.setContentText(title);
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
				} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REPLACED)) {
					// Notify reboot required
					String packageName = inputUri.getSchemeSpecificPart();
					if (packageName.equals(context.getPackageName())) {
						// Start package update
						Intent changeIntent = new Intent();
						changeIntent.setClass(context, UpdateService.class);
						changeIntent.putExtra(UpdateService.cAction, UpdateService.cActionUpdated);
						context.startService(changeIntent);

						// Build notification
						NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
						notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
						notificationBuilder.setContentTitle(context.getString(R.string.app_name));
						notificationBuilder.setContentText(context.getString(R.string.msg_reboot));
						notificationBuilder.setWhen(System.currentTimeMillis());
						notificationBuilder.setAutoCancel(true);
						Notification notification = notificationBuilder.build();

						// Notify
						notificationManager.notify(Util.NOTIFY_RESTART, notification);
					}
				} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REMOVED) && !replacing) {
					// Package removed
					notificationManager.cancel(uid);

					// Delete restrictions
					if (PrivacyService.getClient() != null) {
						PrivacyManager.deleteRestrictions(uid, true);
						PrivacyManager.deleteSettings(uid);
						PrivacyManager.deleteUsage(uid);
					}
				}
			}
		} catch (Throwable ex) {
			Util.bug(null, ex);
		}
	}
}
