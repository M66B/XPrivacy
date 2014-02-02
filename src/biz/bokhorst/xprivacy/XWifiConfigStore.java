package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.net.wifi.WifiConfiguration;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XWifiConfigStore extends XHook {
	private Methods mMethod;

	private XWifiConfigStore(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "Srv." + method.name());
		mMethod = method;
	}

	public boolean isVisible() {
		return false;
	}

	public String getClassName() {
		return "android.net.wifi.WifiConfigStore";
	}

	// List<WifiConfiguration> getConfiguredNetworks()
	// frameworks/base/wifi/java/android/net/wifi/WifiConfigStore.java

	private enum Methods {
		getConfiguredNetworks
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XWifiConfigStore(Methods.getConfiguredNetworks, PrivacyManager.cNetwork));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.getConfiguredNetworks) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<WifiConfiguration>());
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
