package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;

import android.content.Intent;
import android.os.Binder;
import android.os.Bundle;
import android.os.Process;
import android.telephony.TelephonyManager;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityThread extends XHook {

	private String mActionName;

	public XActivityThread(String methodName, String restrictionName, String[] permissions, String actionName) {
		super(methodName, restrictionName, permissions);
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
					XUtil.log(this, Log.INFO, "Handle action=" + intent.getAction() + " uid=" + Process.myUid()
							+ " call=" + Binder.getCallingUid());
				XUtil.dumpIntent(intent);

				// Process intent
				if (intent != null && mActionName.equals(intent.getAction())) {
					Bundle bundle = intent.getExtras();
					if (bundle == null)
						return;
					if (intent.getAction().equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
						String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
						XUtil.log(this, Log.INFO, "Number out=" + phoneNumber);
						if (phoneNumber != null)
							if (isRestricted(param))
								intent.putExtra(Intent.EXTRA_PHONE_NUMBER, XRestriction.cDefaceString);
					} else if (intent.getAction().equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
						String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
						XUtil.log(this, Log.INFO, "Number in=" + phoneNumber);
						if (phoneNumber != null)
							if (isRestricted(param))
								intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER, XRestriction.cDefaceString);
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
