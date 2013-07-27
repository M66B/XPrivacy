package biz.bokhorst.xprivacy;

import android.net.NetworkInfo;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XConnectivityManager extends XHook {

	public XConnectivityManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public NetworkInfo getActiveNetworkInfo()
	// public NetworkInfo[] getAllNetworkInfo()
	// public NetworkInfo getNetworkInfo(int networkType)
	// frameworks/base/core/java/android/net/ConnectivityManager.java
	// http://developer.android.com/reference/android/net/ConnectivityManager.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResult() != null)
			if (isRestricted(param))
				if (param.method.getName().equals("getAllNetworkInfo"))
					param.setResult(new NetworkInfo[0]);
				else
					param.setResult(null);
	}
}
