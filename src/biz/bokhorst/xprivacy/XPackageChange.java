package biz.bokhorst.xprivacy;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.util.Log;

public class XPackageChange extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		// Check uri and action
		Uri inputUri = Uri.parse(intent.getDataString());
		if (inputUri.getScheme().equals("package") && intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED)) {
			// Get package name
			String packageName = inputUri.getSchemeSpecificPart();
			XUtil.log(null, Log.INFO, "Installed package=" + packageName);

			// Default deny new apps, except Play store
			if (!packageName.equals("com.android.vending"))
				try {
					PackageManager pm = context.getPackageManager();
					ApplicationInfo appInfo = pm.getApplicationInfo(packageName, 0);
					for (String restrictionName : XRestriction.getRestrictions())
						XRestriction.setRestricted(null, context, appInfo.uid, restrictionName, true);
				} catch (Throwable ex) {
					XUtil.bug(null, ex);
				}

			// Send intent to edit settings
			Intent intentSettings = new Intent(context, XAppSettings.class);
			intentSettings.putExtra(XAppSettings.cPackageName, packageName);
			intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_NO_USER_ACTION);
			context.startActivity(intentSettings);
		}
	}
}
