package biz.bokhorst.xprivacy;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import android.widget.Toast;

public class XTestCallReceiver extends BroadcastReceiver {
	@Override
	public void onReceive(Context context, Intent intent) {
		Bundle bundle = intent.getExtras();
		if (bundle != null) {
			String incoming = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
			String outgoing = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
			String msg = String.format("XPrivacy: Phone in={0} out={1}", incoming, outgoing);
			Toast toast = Toast.makeText(context, msg, Toast.LENGTH_LONG);
			toast.show();
		}
	}
}
