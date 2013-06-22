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
import android.util.Log;

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
			boolean expert = Boolean.parseBoolean(XRestriction.getSetting(null, context, XRestriction.cSettingExpert,
					Boolean.FALSE.toString(), false));
			NotificationManager notificationManager = (NotificationManager) context
					.getSystemService(Context.NOTIFICATION_SERVICE);

			// Log
			XUtil.log(null, Log.INFO, intent.getAction() + " package=" + packageName + " uid=" + uid + " replacing="
					+ replacing);

			if (intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED) && !replacing) {
				// Package added
				boolean system = false;
				PackageInfo pInfo = null;
				PackageManager pm = context.getPackageManager();
				try {
					pInfo = pm.getPackageInfo(packageName, 0);
					system = (pInfo.applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0;
				} catch (Throwable ex) {
					XUtil.bug(null, ex);
					return;
				}

				if (expert ? true : !system) {
					// Default deny new user apps
					if (!system)
						for (String restrictionName : XRestriction.getRestrictions())
							XRestriction.setRestricted(null, context, uid, restrictionName, null, true);

					// Build result intent
					Intent resultIntent = new Intent(context, ActivityApp.class);
					resultIntent.putExtra(ActivityApp.cPackageName, packageName);
					resultIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_NO_USER_ACTION);

					// Build pending intent
					PendingIntent pendingIntent = PendingIntent.getActivity(context, uid, resultIntent,
							PendingIntent.FLAG_UPDATE_CURRENT);

					// Build notification
					NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
					notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
					notificationBuilder.setContentTitle(context.getString(R.string.app_name));
					notificationBuilder.setContentText(pm.getApplicationLabel(pInfo.applicationInfo));
					notificationBuilder.setContentIntent(pendingIntent);
					notificationBuilder.setWhen(System.currentTimeMillis());
					notificationBuilder.setAutoCancel(true);
					Notification notification = notificationBuilder.build();

					// Notify
					notificationManager.notify(pInfo.applicationInfo.uid, notification);
				}
			} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REMOVED) && !replacing) {
				// Package removed
				notificationManager.cancel(uid);

				// Remove existing restrictions
				for (String restrictionName : XRestriction.getRestrictions())
					XRestriction.setRestricted(null, context, uid, restrictionName, null, false);

				// Remove usage data
				XRestriction.deleteUsageData(context, uid);
			}
		}
	}
}
