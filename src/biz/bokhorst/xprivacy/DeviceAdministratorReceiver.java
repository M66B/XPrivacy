package biz.bokhorst.xprivacy;

import android.app.admin.DeviceAdminReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class DeviceAdministratorReceiver extends DeviceAdminReceiver {
	@Override
	public void onEnabled(Context context, Intent intent) {
		super.onEnabled(context, intent);
		Util.log(null, Log.WARN, "Device admin enabled");
	}

	@Override
	public void onDisabled(Context context, Intent intent) {
		super.onDisabled(context, intent);
		Util.log(null, Log.WARN, "Device admin disabled");
	}
}
