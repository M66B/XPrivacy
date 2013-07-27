package biz.bokhorst.xprivacy;

import android.net.NetworkInfo;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XNetworkInfo extends XHook {

	public XNetworkInfo(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public DetailedState getDetailedState()
	// public State getState()
	// public boolean isConnected()
	// public boolean isConnectedOrConnecting()
	// frameworks/base/core/java/android/net/NetworkInfo.java
	// http://developer.android.com/reference/android/net/NetworkInfo.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (isRestricted(param)) {
			String methodName = param.method.getName();
			if (methodName.equals("getDetailedState"))
				param.setResult(NetworkInfo.DetailedState.DISCONNECTED);
			else if (methodName.equals("getState"))
				param.setResult(NetworkInfo.State.DISCONNECTED);
			else if (methodName.equals("isConnected") || methodName.equals("isConnectedOrConnecting"))
				param.setResult(false);
			else
				Util.log(this, Log.WARN, "Unknown method=" + methodName);
		}
	}
}
