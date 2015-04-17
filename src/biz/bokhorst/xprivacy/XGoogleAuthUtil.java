package biz.bokhorst.xprivacy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.accounts.Account;
import android.util.Log;

public class XGoogleAuthUtil extends XHook {
	private Methods mMethod;

	private XGoogleAuthUtil(Methods method, String restrictionName, String specifier) {
		super(restrictionName, method.name(), specifier);
		mMethod = method;
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

	private enum Methods {
		getToken, getTokenWithNotification
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XGoogleAuthUtil(Methods.getToken, PrivacyManager.cAccounts, "getTokenGoogle"));
		listHook.add(new XGoogleAuthUtil(Methods.getTokenWithNotification, PrivacyManager.cAccounts,
				"getTokenWithNotificationGoogle"));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getToken || mMethod == Methods.getTokenWithNotification) {
			if (param.args.length > 1) {
				String accountName = null;
				if (param.args[1] instanceof String)
					accountName = (String) param.args[1];
				else if (param.args[1] instanceof Account)
					accountName = ((Account) param.args[1]).type;
				if (param.getResult() != null && isRestrictedExtra(param, accountName))
					param.setThrowable(new IOException("XPrivacy"));
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
