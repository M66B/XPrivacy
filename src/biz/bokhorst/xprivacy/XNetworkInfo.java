package biz.bokhorst.xprivacy;

import android.net.NetworkInfo;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XNetworkInfo extends XHook {

	public XNetworkInfo(String methodName, String restrictionName, String[] permissions) {
		super(restrictionName, methodName, permissions, null);
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
		String methodName = param.method.getName();
		if (methodName.equals("getDetailedState")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(NetworkInfo.DetailedState.DISCONNECTED);
		} else if (methodName.equals("getState")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(NetworkInfo.State.DISCONNECTED);
		} else if (methodName.equals("isConnected") || methodName.equals("isConnectedOrConnecting")) {
			if (isRestricted(param))
				param.setResult(false);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);

	}
}
