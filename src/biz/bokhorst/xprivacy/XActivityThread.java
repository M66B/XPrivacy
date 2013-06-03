package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.Intent;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityThread extends XHook {

	private String mActionName;

	public XActivityThread(String methodName, String restrictionName, String actionName) {
		super(methodName, restrictionName);
		mActionName = actionName;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		try {
			if (param.args[0] != null) {
				// Get intent
				Field fieldIntent = findField(param.args[0].getClass(), "intent");
				Intent intent = (Intent) fieldIntent.get(param.args[0]);
				if (intent != null && intent.getAction() != null)
					XUtil.log(this, Log.INFO, "Handle action=" + intent.getAction());

				// Process intent
				if (intent != null && mActionName.equals(intent.getAction())) {
					Bundle bundle = intent.getExtras();
					if (bundle == null)
						return;
					if (isRestricted(param)) {
						if (intent.getAction().equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
							String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
							if (phoneNumber != null)
								intent.putExtra(Intent.EXTRA_PHONE_NUMBER, XRestriction.cDefaceString);
						} else if (intent.getAction().equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
							String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
							if (phoneNumber != null)
								intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER, XRestriction.cDefaceString);
						}
					}
				}
			}
		} catch (Throwable ex) {
			XUtil.bug(this, ex);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
