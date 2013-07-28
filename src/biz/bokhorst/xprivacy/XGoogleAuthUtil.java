package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XGoogleAuthUtil extends XHook {

	private XGoogleAuthUtil(String methodName, String restrictionName, String specifier) {
		super(restrictionName, methodName, specifier);
	}

	public String getClassName() {
		return "com.google.android.gms.auth.GoogleAuthUtil";
	}

	// @formatter:off

	// static String getToken(Context context, String accountName, String scope)
	// static String getToken(Context context, String accountName, String scope, Bundle extras)
	// static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras)
	// static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras, Intent callback)
	// static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras, String authority, Bundle syncBundle)
	// https://developer.android.com/reference/com/google/android/gms/auth/GoogleAuthUtil.html

	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XGoogleAuthUtil("getToken", PrivacyManager.cAccounts, "getTokenGoogle"));
		listHook.add(new XGoogleAuthUtil("getTokenWithNotification", PrivacyManager.cAccounts,
				"getTokenWithNotificationGoogle"));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("getToken") || methodName.equals("getTokenWithNotification")) {
			if (param.getResult() != null && isRestricted(param))
				param.setThrowable(new IOException());
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}
}
