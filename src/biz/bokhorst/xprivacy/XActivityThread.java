package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.content.pm.ProviderInfo;
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
	// private void installContentProviders(Context context, List<ProviderInfo> providers)
	// frameworks/base/core/java/android/app/ActivityThread.java

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("handleReceiver")) {
			if (param.args[0] != null) {
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
						if (action.equals(Intent.ACTION_BOOT_COMPLETED)) {
							// Boot completed
							if (isRestricted(param, mActionName))
								param.setResult(null);
						} else if (action.equals(Intent.ACTION_NEW_OUTGOING_CALL)) {
							// Outgoing call
							Bundle bundle = intent.getExtras();
							if (bundle != null) {
								String phoneNumber = bundle.getString(Intent.EXTRA_PHONE_NUMBER);
								if (phoneNumber != null)
									if (isRestricted(param, mActionName))
										intent.putExtra(Intent.EXTRA_PHONE_NUMBER,
												Restriction.getDefacedProp("PhoneNumber"));
							}
						} else if (action.equals(TelephonyManager.ACTION_PHONE_STATE_CHANGED)) {
							// Incoming call
							Bundle bundle = intent.getExtras();
							if (bundle != null) {
								String phoneNumber = bundle.getString(TelephonyManager.EXTRA_INCOMING_NUMBER);
								if (phoneNumber != null) {
									if (isRestricted(param, mActionName))
										intent.putExtra(TelephonyManager.EXTRA_INCOMING_NUMBER,
												Restriction.getDefacedProp("PhoneNumber"));
								}
							}
						} else
							Util.log(this, Log.WARN, "Unhandled action=" + mActionName);
					}
				}
			}
		} else if (methodName.equals("installContentProviders")) {
			try {
				// Get providers
				@SuppressWarnings("unchecked")
				List<ProviderInfo> listProvider = (List<ProviderInfo>) param.args[1];

				// Build allowed list
				List<ProviderInfo> listAllowed = new ArrayList<ProviderInfo>();
				for (ProviderInfo provider : listProvider) {
					// Skip if Android
					if (provider.applicationInfo.uid == Restriction.cUidAndroid)
						return;
					int uid = provider.applicationInfo.uid;
					Util.log(this, Log.INFO, "provider=" + provider.getClass().getName() + " uid=" + uid);
					if (getRestricted(null, uid, false))
						listAllowed.add(provider);
				}

				// Set result
				param.args[1] = listAllowed;
			} catch (Throwable ex) {
				Util.bug(this, ex);
			}
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
