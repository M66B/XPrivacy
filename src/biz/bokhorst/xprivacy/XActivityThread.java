package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import android.content.BroadcastReceiver;
import android.content.Intent;
import android.nfc.NfcAdapter;
import android.os.Bundle;
import android.service.notification.NotificationListenerService;
import android.telephony.TelephonyManager;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityThread extends XHook {

	private String mActionName;

	public XActivityThread(String methodName, String restrictionName, String actionName) {
		super(restrictionName, methodName, actionName);
		mActionName = actionName;
	}

	public XActivityThread(String methodName, String restrictionName, String actionName, int sdk) {
		super(restrictionName, methodName, actionName, sdk);
		mActionName = actionName;
	}

	public String getClassName() {
		return "android.app.ActivityThread";
	}

	@Override
	public boolean isVisible() {
		return false;
	}

	// @formatter:off

	// private void handleReceiver(ReceiverData data)
	// frameworks/base/core/java/android/app/ActivityThread.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("handleReceiver")) {
			if (param.args.length > 0 && param.args[0] != null) {
				// Get intent
				Intent intent = null;
				try {
					Field fieldIntent = findField(param.args[0].getClass(), "intent");
					intent = (Intent) fieldIntent.get(param.args[0]);
				} catch (Throwable ex) {
					Util.bug(this, ex);
				}

				// Process intent
				if (intent != null) {
					// Check action
					String action = intent.getAction();
					if (mActionName.equals(action)) {
						if (action.equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
							// Outgoing call
							Bundle bundle = intent.getExtras();
							if (bundle != null) {
								String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
								if (phoneNumber != null)
									if (isRestricted(param, mActionName))
										intent.putExtra(Intent.EXTRA_PHONE_NUMBER,
												(String) PrivacyManager.getDefacedProp("PhoneNumber"));
							}
						} else if (action.equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
							// Incoming call
							Bundle bundle = intent.getExtras();
							if (bundle != null) {
								String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
								if (phoneNumber != null) {
									if (isRestricted(param, mActionName))
										intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER,
												(String) PrivacyManager.getDefacedProp("PhoneNumber"));
								}
							}
						} else if (action.equals(TelephonyManager.ACTION_RESPOND_VIA_MESSAGE)) {
							if (isRestricted(param, mActionName)) {
								finish(param);
								param.setResult(null);
							}
						} else if (action.equals(NfcAdapter.ACTION_NDEF_DISCOVERED)
								|| action.equals(NfcAdapter.ACTION_TAG_DISCOVERED)
								|| action.equals(NfcAdapter.ACTION_TECH_DISCOVERED)) {
							if (isRestricted(param, mActionName)) {
								finish(param);
								param.setResult(null);
							}
						} else if (action.equals(NotificationListenerService.SERVICE_INTERFACE)) {
							if (isRestricted(param, mActionName)) {
								finish(param);
								param.setResult(null);
							}
						} else if (action.equals(Intent.ACTION_PACKAGE_ADDED)
								|| action.equals(Intent.ACTION_PACKAGE_REPLACED)
								|| action.equals(Intent.ACTION_PACKAGE_RESTARTED)
								|| action.equals(Intent.ACTION_PACKAGE_REMOVED)) {
							if (isRestricted(param, mActionName)) {
								finish(param);
								param.setResult(null);
							}
						} else
							Util.log(this, Log.WARN, "Unhandled action=" + mActionName);
					}
				}
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	private void finish(MethodHookParam param) {
		// unscheduleGcIdler
		try {
			Method unschedule = param.thisObject.getClass().getDeclaredMethod("unscheduleGcIdler");
			unschedule.invoke(param.thisObject);
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}

		// data.finish
		try {
			BroadcastReceiver.PendingResult pr = (BroadcastReceiver.PendingResult) param.args[0];
			pr.finish();
		} catch (Throwable ex) {
			Util.bug(this, ex);
		}
	}
}
