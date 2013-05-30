package biz.bokhorst.xprivacy;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

public class XPackageChange extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		// Get intent data
		Uri inputUri = Uri.parse(intent.getDataString());
		if (!inputUri.getScheme().equals("package"))
			return;
		String packageName = inputUri.getSchemeSpecificPart();

		// Handle package add
		if (intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED)) {
			Intent intentAppDetail = new Intent();
			intentAppDetail.setAction("android.settings.APPLICATION_DETAILS_SETTINGS");
			intentAppDetail.addCategory(Intent.CATEGORY_DEFAULT);
			intentAppDetail.setData(Uri.parse("package:" + packageName));
			intentAppDetail.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);

			try {
				context.startActivity(intentAppDetail);
			} catch (Throwable ex) {
				XUtil.bug(null, ex);
			}
		}
	}
}
