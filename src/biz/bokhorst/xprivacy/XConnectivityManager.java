package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.net.NetworkInfo;

public class XConnectivityManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.net.ConnectivityManager";

	private XConnectivityManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), "Connectivity." + method.name());
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

	public static List<XHook> getInstances(String className, boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			for (Methods connmgr : Methods.values())
				listHook.add(new XConnectivityManager(connmgr, PrivacyManager.cInternet, className));
		}
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getActiveNetworkInfo:
		case getNetworkInfo:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(null);
			break;

		case getAllNetworkInfo:
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new NetworkInfo[0]);
			break;
		}
	}
}
