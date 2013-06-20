package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.app.AndroidAppHelper;
import android.content.Intent;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityThread extends XHook {

	private String mActionName;

	public XActivityThread(String methodName, String restrictionName, String[] permissions, String actionName) {
		super(methodName, restrictionName, permissions, actionName);
		mActionName = actionName;
	}

	// @formatter:off

	// private void handleReceiver(ReceiverData data)
	// frameworks/base/core/java/android/app/ActivityThread.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.args[0] != null) {
			// Get intent
			Intent intent = null;
			try {
				Field fieldIntent = findField(param.args[0].getClass(), "intent");
				intent = (Intent) fieldIntent.get(param.args[0]);
			} catch (Throwable ex) {
				XUtil.bug(this, ex);
			}

			// Process intent
			if (intent == null)
				XUtil.log(this, Log.WARN, "Intent missing");
			else {
				// Check action
				String action = intent.getAction();
				if (mActionName.equals(action)) {
					if (intent.getAction().equals(Intent.ACTION_BOOT_COMPLETED)) {
						// Boot completed
						if (isRestricted(param, mActionName))
							if (Boolean.parseBoolean(XRestriction.getSetting(this,
									AndroidAppHelper.currentApplication(), XRestriction.cSettingExpert,
									Boolean.FALSE.toString())))
								param.setResult(null);
					} else if (intent.getAction().equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
						// Outgoing call
						Bundle bundle = intent.getExtras();
						if (bundle != null) {
							String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
							if (phoneNumber != null)
								if (isRestricted(param, mActionName))
									intent.putExtra(Intent.EXTRA_PHONE_NUMBER, XRestriction.getDefacedString());
						}
					} else if (intent.getAction().equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
						// Incoming call
						Bundle bundle = intent.getExtras();
						if (bundle != null) {
							String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
							if (phoneNumber != null) {
								if (isRestricted(param, mActionName))
									intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER,
											XRestriction.getDefacedString());
							}
						}
					} else
						XUtil.log(this, Log.WARN, "Unhandled action=" + mActionName);
				}
			}
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
