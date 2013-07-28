package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.net.NetworkInfo;
import android.util.Log;
import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XConnectivityManager extends XHook {

	private XConnectivityManager(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "android.net.ConnectivityManager";
	}

	// public NetworkInfo getActiveNetworkInfo()
	// public NetworkInfo[] getAllNetworkInfo()
	// public NetworkInfo getNetworkInfo(int networkType)
	// frameworks/base/core/java/android/net/ConnectivityManager.java
	// http://developer.android.com/reference/android/net/ConnectivityManager.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		String[] connmgrs = new String[] { "getActiveNetworkInfo", "getAllNetworkInfo", "getNetworkInfo" };
		for (String connmgr : connmgrs)
			listHook.add(new XConnectivityManager(connmgr, PrivacyManager.cInternet));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (methodName.equals("getActiveNetworkInfo") || methodName.equals("getNetworkInfo")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(null);
		} else if (methodName.equals("getAllNetworkInfo")) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new NetworkInfo[0]);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + methodName);
	}
}
