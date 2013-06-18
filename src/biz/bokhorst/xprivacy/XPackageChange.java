package biz.bokhorst.xprivacy;

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

public class XPackageChange extends BroadcastReceiver {

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
					Boolean.FALSE.toString()));

			// TODO: check permission changes when replacing

			if (intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED) && !replacing) {
				// Package added
				XUtil.log(null, Log.INFO, "Added package=" + packageName + " uid=" + uid);

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
						for (String restrictionName : XRestriction.getRestrictions(context))
							XRestriction.setRestricted(null, context, uid, restrictionName, true);

					// Build result intent
					Intent resultIntent = new Intent(context, ActivityApp.class);
					resultIntent.putExtra(ActivityApp.cPackageName, packageName);
					resultIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_NO_USER_ACTION);

					// Build pending inent
					PendingIntent pendingIntent = PendingIntent.getActivity(context, 0, resultIntent,
							PendingIntent.FLAG_UPDATE_CURRENT);

					// Build notification
					NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
					notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
					notificationBuilder.setContentTitle(context.getString(R.string.app_name));
					notificationBuilder.setContentText(pm.getApplicationLabel(pInfo.applicationInfo));
					notificationBuilder.setContentIntent(pendingIntent);
					notificationBuilder.setWhen(System.currentTimeMillis());
					notificationBuilder.setAutoCancel(true);

					// Notify
					NotificationManager notificationManager = (NotificationManager) context
							.getSystemService(Context.NOTIFICATION_SERVICE);
					notificationManager.notify(pInfo.applicationInfo.uid, notificationBuilder.build());
				}
			} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REMOVED) && !replacing) {
				// Package removed
				XUtil.log(null, Log.INFO, "Removed package=" + packageName + " uid=" + uid);

				// Remove existing restrictions
				for (String restrictionName : XRestriction.getRestrictions(context))
					XRestriction.setRestricted(null, context, uid, restrictionName, false);

				// Remove audit trail
				XRestriction.deleteAuditTrail(context, uid);
			}
		}
	}
}
