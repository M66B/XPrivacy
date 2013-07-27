package biz.bokhorst.xprivacy;

import java.io.IOException;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XGoogleAuthUtil extends XHook {

	public XGoogleAuthUtil(String methodName, String restrictionName, String[] permissions, String specifier) {
		super(methodName, restrictionName, permissions, specifier);
	}

	// @formatter:off

	// static String getToken(Context context, String accountName, String scope)
	// static String getToken(Context context, String accountName, String scope, Bundle extras)
	// static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras)
	// static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras, Intent callback)
	// static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras, String authority, Bundle syncBundle)
	// https://developer.android.com/reference/com/google/android/gms/auth/GoogleAuthUtil.html

	// @formatter:on

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResult() != null)
			if (isRestricted(param))
				param.setThrowable(new IOException());
	}
}
