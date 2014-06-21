package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.net.NetworkInfo;
import android.util.Log;

public class XConnectivityManager extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XConnectivityManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// public NetworkInfo getActiveNetworkInfo()
	// public NetworkInfo[] getAllNetworkInfo()
	// public NetworkInfo getNetworkInfo(int networkType)
	// frameworks/base/core/java/android/net/ConnectivityManager.java
	// http://developer.android.com/reference/android/net/ConnectivityManager.html

	private enum Methods {
		getActiveNetworkInfo, getAllNetworkInfo, getNetworkInfo
	};

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods connmgr : Methods.values())
			listHook.add(new XConnectivityManager(connmgr, PrivacyManager.cInternet, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getActiveNetworkInfo || mMethod == Methods.getNetworkInfo) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(null);

		} else if (mMethod == Methods.getAllNetworkInfo) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new NetworkInfo[0]);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
