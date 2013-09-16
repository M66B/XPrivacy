package biz.bokhorst.xprivacy;

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
import android.support.v4.app.NotificationCompat;

public class PackageChange extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		// Check uri
		Uri inputUri = Uri.parse(intent.getDataString());
		if (inputUri.getScheme().equals("package")) {
			// Get data
			String packageName = inputUri.getSchemeSpecificPart();
			int uid = intent.getIntExtra(Intent.EXTRA_UID, 0);
			boolean replacing = intent.getBooleanExtra(Intent.EXTRA_REPLACING, false);
			boolean fSystem = PrivacyManager.getSettingBool(null, context, 0, PrivacyManager.cSettingFSystem, true,
					false);

			NotificationManager notificationManager = (NotificationManager) context
					.getSystemService(Context.NOTIFICATION_SERVICE);
			if (intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED)) {
				// Package added
				boolean system = false;
				PackageInfo pInfo = null;
				PackageManager pm = context.getPackageManager();
				try {
					pInfo = pm.getPackageInfo(packageName, 0);
					system = (pInfo.applicationInfo.flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
				} catch (Throwable ex) {
					Util.bug(null, ex);
					return;
				}

				if (fSystem ? !system : true) {
					// Default deny new user apps
					if (!system && !replacing) {
						// Check for existing restrictions
						boolean someRestricted = false;
						for (boolean restricted : PrivacyManager.getRestricted(context, uid, false))
							if (restricted) {
								someRestricted = true;
								break;
							}

						// Restrict if no previous restrictions
						if (!someRestricted)
							for (String restrictionName : PrivacyManager.getRestrictions(false))
								if (PrivacyManager.getSettingBool(null, context, 0,
										String.format("Template.%s", restrictionName), true, false))
									PrivacyManager.setRestricted(null, context, uid, restrictionName, null, true);
					}

					// Build result intent
					Intent resultIntent = new Intent(context, ActivityApp.class);
					resultIntent.putExtra(ActivityApp.cPackageName, packageName);
					resultIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

					// Build pending intent
					PendingIntent pendingIntent = PendingIntent.getActivity(context, uid, resultIntent,
							PendingIntent.FLAG_UPDATE_CURRENT);

					// Build result intent clear
					Intent resultIntentClear = new Intent(context, ActivityApp.class);
					resultIntentClear.putExtra(ActivityApp.cPackageName, packageName);
					resultIntentClear.putExtra(ActivityApp.cActionClear, true);
					resultIntentClear.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

					// Build pending intent clear
					PendingIntent pendingIntentClear = PendingIntent.getActivity(context, uid, resultIntentClear,
							PendingIntent.FLAG_UPDATE_CURRENT);

					// Title
					String title = String.format("%s %s %s",
							context.getString(replacing ? R.string.msg_update : R.string.msg_new),
							pm.getApplicationLabel(pInfo.applicationInfo), pInfo.versionName);

					// Build notification
					NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
					notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
					notificationBuilder.setContentTitle(context.getString(R.string.app_name));
					notificationBuilder.setContentText(title);
					notificationBuilder.setContentIntent(pendingIntent);
					notificationBuilder.setWhen(System.currentTimeMillis());
					notificationBuilder.setAutoCancel(true);
					notificationBuilder.addAction(R.drawable.cross_holo_dark, context.getString(R.string.menu_clear),
							pendingIntentClear);
					Notification notification = notificationBuilder.build();

					// Notify
					notificationManager.notify(pInfo.applicationInfo.uid, notification);
				}
			} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REPLACED)) {
				// Notify reboot required
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
				}

				// Upgrade
				try {
					// Get stored version
					PackageManager pm = context.getPackageManager();
					Version sVersion = new Version(PrivacyManager.getSetting(null, context, 0,
							PrivacyManager.cSettingVersion, "0.0", false));

					if (sVersion.compareTo(new Version("0.0")) != 0) {
						// Version 1.7+
						if (sVersion.compareTo(new Version("1.7")) < 0) {
							// Disable identification//proc
							for (ApplicationInfo aInfo : pm.getInstalledApplications(0))
								PrivacyManager.setRestricted(null, context, aInfo.uid, PrivacyManager.cIdentification,
										"/proc", false);
						}

						// Version 1.7.4+
						if (sVersion.compareTo(new Version("1.7.4")) < 0) {
							// Disable location/getProviders,isProviderEnabled
							for (ApplicationInfo aInfo : pm.getInstalledApplications(0)) {
								PrivacyManager.setRestricted(null, context, aInfo.uid, PrivacyManager.cLocation,
										"getProviders", false);
								PrivacyManager.setRestricted(null, context, aInfo.uid, PrivacyManager.cLocation,
										"isProviderEnabled", false);
							}
						}

						// Version 1.9.9+
						if (sVersion.compareTo(new Version("1.9.9")) < 0) {
							// Disable identification//system/build.prop
							for (ApplicationInfo aInfo : pm.getInstalledApplications(0))
								PrivacyManager.setRestricted(null, context, aInfo.uid, PrivacyManager.cIdentification,
										"/system/build.prop", false);

							// Select user applications
							PrivacyManager.setSetting(null, context, 0, PrivacyManager.cSettingFSystem, "U");
						}
					}

					// Update stored version
					PackageInfo pInfo = pm.getPackageInfo(context.getPackageName(), 0);
					PrivacyManager.setSetting(null, context, 0, PrivacyManager.cSettingVersion, pInfo.versionName);
				} catch (Throwable ex) {
					Util.bug(null, ex);
				}
			} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REMOVED) && !replacing) {
				// Package removed
				notificationManager.cancel(uid);
				PrivacyManager.deleteRestrictions(context, uid);
				PrivacyManager.deleteUsage(context, uid);
			}
		}
	}
}
