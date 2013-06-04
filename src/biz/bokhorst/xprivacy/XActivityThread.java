package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.Intent;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityThread extends XHook {

	private String mActionName;

	public XActivityThread(String methodName, String restrictionName, String[] permissions, String actionName) {
		super(methodName, restrictionName, permissions);
		mActionName = actionName;
	}

	// private void handleReceiver(ReceiverData data)
	// frameworks/base/core/java/android/app/ActivityThread.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.args[0] != null) {
			// Get intent
			Field fieldIntent = findField(param.args[0].getClass(), "intent");
			Intent intent = (Intent) fieldIntent.get(param.args[0]);

			// Check action
			if (intent != null && mActionName.equals(intent.getAction())) {
				// Get bundle
				Bundle bundle = intent.getExtras();
				if (bundle == null)
					return;

				// Process action
				if (intent.getAction().equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
					// Outgoing call
					String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
					if (phoneNumber != null)
						if (isRestricted(param))
							intent.putExtra(Intent.EXTRA_PHONE_NUMBER, XRestriction.cDefaceString);

				} else if (intent.getAction().equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
					// Incoming call
					String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
					if (phoneNumber != null) {
						if (isRestricted(param))
							intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER, XRestriction.cDefaceString);
					}
				}
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
