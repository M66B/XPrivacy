package biz.bokhorst.xprivacy;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.support.v4.app.NotificationCompat;

public class BootReceiver extends BroadcastReceiver {
	@Override
	public void onReceive(Context context, Intent bootIntent) {
		// Randomize
		boolean random = PrivacyManager.getSettingBool(null, context, PrivacyManager.cSettingRandom, true, false);
		if (random) {
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingLatitude,
					PrivacyManager.getRandomProp("LAT"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingLongitude,
					PrivacyManager.getRandomProp("LON"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingSerial,
					PrivacyManager.getRandomProp("SERIAL"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingMac, PrivacyManager.getRandomProp("MAC"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingPhone,
					PrivacyManager.getRandomProp("PHONE"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingImei, PrivacyManager.getRandomProp("IMEI"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingId,
					PrivacyManager.getRandomProp("ANDROID_ID"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingGsfId,
					PrivacyManager.getRandomProp("GSF_ID"));
			PrivacyManager.setSetting(null, context, PrivacyManager.cSettingCountry,
					PrivacyManager.getRandomProp("ISO3166"));
		}

		// Check if Xposed enabled
		if (!Util.isXposedEnabled()) {
			// Create Xposed installer intent
			Intent xInstallerIntent = context.getPackageManager().getLaunchIntentForPackage(
					"de.robv.android.xposed.installer");
			if (xInstallerIntent != null)
				xInstallerIntent.putExtra("opentab", 1);

			PendingIntent pi = (xInstallerIntent == null ? null : PendingIntent.getActivity(context, 0,
					xInstallerIntent, PendingIntent.FLAG_UPDATE_CURRENT));

			// Build notification
			NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context);
			notificationBuilder.setSmallIcon(R.drawable.ic_launcher);
			notificationBuilder.setContentTitle(context.getString(R.string.app_name));
			notificationBuilder.setContentText(context.getString(R.string.app_notenabled));
			notificationBuilder.setWhen(System.currentTimeMillis());
			notificationBuilder.setAutoCancel(true);
			if (pi != null)
				notificationBuilder.setContentIntent(pi);
			Notification notification = notificationBuilder.build();

			// Display notification
			NotificationManager notificationManager = (NotificationManager) context
					.getSystemService(Context.NOTIFICATION_SERVICE);
			notificationManager.notify(0, notification);
		}
	}
}
