package biz.bokhorst.xprivacy;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
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

			if (intent.getAction().equals(Intent.ACTION_PACKAGE_ADDED) && !replacing) {
				// Package added
				XUtil.log(null, Log.INFO, "Added package=" + packageName + " uid=" + uid);

				// Default deny new apps, except Play store
				if (!packageName.equals("com.android.vending"))
					for (String restrictionName : XRestriction.getRestrictions(context))
						XRestriction.setRestricted(null, context, uid, restrictionName, true);

				// Send intent to edit settings
				Intent intentSettings = new Intent(context, XActivitySingleApp.class);
				intentSettings.putExtra(XActivitySingleApp.cPackageName, packageName);
				intentSettings.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_NO_USER_ACTION);
				context.startActivity(intentSettings);
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
