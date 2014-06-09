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
	public void onReceive(final Context context, Intent bootIntent) {
		// Start boot update
		Intent changeIntent = new Intent();
		changeIntent.setClass(context, UpdateService.class);
		changeIntent.putExtra(UpdateService.cAction, UpdateService.cActionBoot);
		context.startService(changeIntent);

		// Check if Xposed enabled
		if (Util.isXposedEnabled())
			context.sendBroadcast(new Intent("biz.bokhorst.xprivacy.action.ACTIVE"));
		else {
			// Create Xposed installer intent
			Intent xInstallerIntent = Util.getXInstallerIntent(context);

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
			notificationManager.notify(Util.NOTIFY_NOTXPOSED, notification);
		}
	}
}
