package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ProviderInfo;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XActivityThread extends XHook {

	private String mActionName;
	
	private final static int cUidAndroid = 1000;

	public XActivityThread(String methodName, String restrictionName, String[] permissions, String actionName) {
		super(methodName, restrictionName, permissions);
		mActionName = actionName;
	}

	// @formatter:off

	// private void handleReceiver(ReceiverData data)
	// private void installContentProviders(Context context, List<ProviderInfo> providers)
	// frameworks/base/core/java/android/app/ActivityThread.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("handleReceiver")) {
			if (param.args[0] != null) {
				// Get intent
				// CM10/CM10.1
				Field fieldIntent = findField(param.args[0].getClass(), "intent");
				Intent intent = (Intent) fieldIntent.get(param.args[0]);
				if (intent != null) {
					String action = intent.getAction();
					XUtil.log(this, Log.INFO, "action=" + action);

					// Check action
					if (mActionName.equals(action)) {
						if (intent.getAction().equals(Intent.ACTION_BOOT_COMPLETED)) {
							// Boot completed
							if (isRestricted(param))
								param.setResult(null);
						} else if (intent.getAction().equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
							// Outgoing call
							Bundle bundle = intent.getExtras();
							if (bundle != null) {
								String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
								if (phoneNumber != null)
									if (isRestricted(param))
										intent.putExtra(Intent.EXTRA_PHONE_NUMBER, XRestriction.cDefaceString);
							}
						} else if (intent.getAction().equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
							// Incoming call
							Bundle bundle = intent.getExtras();
							if (bundle != null) {
								String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
								if (phoneNumber != null) {
									if (isRestricted(param))
										intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER,
												XRestriction.cDefaceString);
								}
							}
						} else
							XUtil.log(this, Log.WARN, "Unhandled action=" + mActionName);
					}
				}
			}
		} else if (methodName.equals("installContentProviders")) {
			try {
				Context context = (Context) param.args[0];
				@SuppressWarnings("unchecked")
				List<ProviderInfo> providers = (List<ProviderInfo>) param.args[1];
				List<ProviderInfo> allowed = new ArrayList<ProviderInfo>();
				for (ProviderInfo provider : providers) {
					int uid = provider.applicationInfo.uid;
					XUtil.log(this, Log.INFO, "provider=" + provider.getClass().getName() + " uid=" + uid);
					if (uid == cUidAndroid || !getRestricted(context, uid, true))
						allowed.add(provider);
				}
				param.args[1] = allowed;
			} catch (Throwable ex) {
				XUtil.bug(this, ex);
			}
		} else
			XUtil.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
