package biz.bokhorst.xprivacy;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import android.util.Log;

public class XTestCallReceiver extends BroadcastReceiver {
	@Override
	public void onReceive(Context context, Intent intent) {
		Bundle bundle = intent.getExtras();
		if (bundle != null) {
			XUtil.log(null, Log.INFO, "In " + bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER));
			XUtil.log(null, Log.INFO, "Out " + bundle.getString(Intent.EXTRA_PHONE_NUMBER));
		}
	}
}
