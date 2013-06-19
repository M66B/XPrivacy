package biz.bokhorst.xprivacy;

import android.net.NetworkInfo;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XConnectivityManager extends XHook {

	public XConnectivityManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public NetworkInfo getActiveNetworkInfo()
	// public NetworkInfo getActiveNetworkInfoForUid(int uid)
	// public NetworkInfo[] getAllNetworkInfo()
	// public NetworkInfo getNetworkInfo(int networkType)
	// frameworks/base/core/java/android/net/ConnectivityManager.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			String methodName = param.method.getName();
			if (methodName.equals("getAllNetworkInfo"))
				param.setResult(new NetworkInfo[0]);
			else
				param.setResult(null);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
