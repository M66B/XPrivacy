package biz.bokhorst.xprivacy;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

public class XPackageChange extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		// Check intent data
		Uri inputUri = Uri.parse(intent.getDataString());
		if (inputUri.getScheme().equals("package")) {
			String packageName = inputUri.getSchemeSpecificPart();

			if (intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED)) {
				// Handle package add
				Intent intentSettings = new Intent(context, XAppSettings.class);
				intentSettings.putExtra(XAppSettings.cExtraPackageName, packageName);
				intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_NO_USER_ACTION);
				context.startActivity(intentSettings);
			} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REPLACED)) {
				// TODO: handle package replaced
			} else if (intent.getAction().equals(Intent.ACTION_PACKAGE_REMOVED)) {
				// TODO: handle package removed
			}
		}
	}
}
