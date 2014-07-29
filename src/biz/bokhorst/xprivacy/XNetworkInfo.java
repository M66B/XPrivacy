package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.net.NetworkInfo;
import android.os.Binder;
import android.util.Log;

public class XNetworkInfo extends XHook {
	private Methods mMethod;

	private XNetworkInfo(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "NetworkInfo." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.net.NetworkInfo";
	}

	// public DetailedState getDetailedState()
	// public State getState()
	// public boolean isConnected()
	// public boolean isConnectedOrConnecting()
	// frameworks/base/core/java/android/net/NetworkInfo.java
	// http://developer.android.com/reference/android/net/NetworkInfo.html

	private enum Methods {
		getDetailedState, getExtraInfo, getState, isConnected, isConnectedOrConnecting
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods ninfo : Methods.values())
			if (ninfo == Methods.getExtraInfo)
				listHook.add(new XNetworkInfo(ninfo, PrivacyManager.cNetwork));
			else
				listHook.add(new XNetworkInfo(ninfo, PrivacyManager.cInternet));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getDetailedState) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(NetworkInfo.DetailedState.DISCONNECTED);

		} else if (mMethod == Methods.getExtraInfo) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "ExtraInfo"));

		} else if (mMethod == Methods.getState) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(NetworkInfo.State.DISCONNECTED);

		} else if (mMethod == Methods.isConnected || mMethod == Methods.isConnectedOrConnecting) {
			if (isRestricted(param))
				param.setResult(false);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
