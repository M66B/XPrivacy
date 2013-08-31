package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import android.net.DhcpInfo;
import android.net.wifi.ScanResult;
import android.net.wifi.SupplicantState;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiInfo;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

@SuppressWarnings("deprecation")
public class XWifiManager extends XHook {
	private Methods mMethod;

	private XWifiManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.net.wifi.WifiManager";
	}

	// public List<WifiConfiguration> getConfiguredNetworks()
	// public WifiInfo getConnectionInfo()
	// public DhcpInfo getDhcpInfo()
	// public List<ScanResult> getScanResults()
	// public WifiConfiguration getWifiApConfiguration()
	// frameworks/base/wifi/java/android/net/wifi/WifiManager.java
	// frameworks/base/wifi/java/android/net/wifi/WifiInfo.java
	// frameworks/base/core/java/android/net/DhcpInfo.java
	// http://developer.android.com/reference/android/net/wifi/WifiManager.html

	private enum Methods {
		getConfiguredNetworks, getConnectionInfo, getDhcpInfo, getScanResults, getWifiApConfiguration
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods wifi : Methods.values())
			listHook.add(new XWifiManager(wifi, PrivacyManager.cNetwork));

		listHook.add(new XWifiManager(Methods.getScanResults, PrivacyManager.cLocation));

		// This is to fake "offline", no permission required
		listHook.add(new XWifiManager(Methods.getConnectionInfo, PrivacyManager.cInternet));
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
		} else if (mMethod == Methods.getConnectionInfo) {
			if (param.getResult() != null && isRestricted(param)) {
				WifiInfo wInfo = (WifiInfo) param.getResult();
				if (getRestrictionName().equals(PrivacyManager.cInternet)) {
					// Supplicant state
					try {
						Field fieldState = findField(WifiInfo.class, "mSupplicantState");
						fieldState.set(wInfo, SupplicantState.DISCONNECTED);
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}
				} else {
					// BSSID
					try {
						Field fieldBSSID = findField(WifiInfo.class, "mBSSID");
						fieldBSSID.set(wInfo, PrivacyManager.getDefacedProp(Binder.getCallingUid(), "MAC"));
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}

					// IP address
					try {
						Field fieldIp = findField(WifiInfo.class, "mIpAddress");
						fieldIp.set(wInfo, PrivacyManager.getDefacedProp(Binder.getCallingUid(), "InetAddress"));
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}

					// MAC address
					try {
						Field fieldMAC = findField(WifiInfo.class, "mMacAddress");
						fieldMAC.set(wInfo, PrivacyManager.getDefacedProp(Binder.getCallingUid(), "MAC"));
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}

					// SSID
					try {
						Field fieldSSID = findField(WifiInfo.class, "mSSID");
						fieldSSID.set(wInfo, PrivacyManager.getDefacedProp(Binder.getCallingUid(), "SSID"));
					} catch (Throwable ex) {
						try {
							Field fieldWifiSsid = findField(WifiInfo.class, "mWifiSsid");
							Object mWifiSsid = fieldWifiSsid.get(wInfo);
							if (mWifiSsid != null) {
								Field octets = findField(mWifiSsid.getClass(), "octets");
								octets.set(mWifiSsid,
										PrivacyManager.getDefacedProp(Binder.getCallingUid(), "WifiSsid.octets"));
							}
						} catch (Throwable exex) {
							Util.bug(this, exex);
						}
					}
				}
			}
		} else if (mMethod == Methods.getDhcpInfo) {
			if (param.getResult() != null && isRestricted(param)) {
				DhcpInfo dInfo = (DhcpInfo) param.getResult();
				dInfo.ipAddress = (Integer) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "IPInt");
				dInfo.gateway = dInfo.ipAddress;
				dInfo.dns1 = dInfo.ipAddress;
				dInfo.dns2 = dInfo.ipAddress;
				dInfo.serverAddress = dInfo.ipAddress;
			}
		} else if (mMethod == Methods.getScanResults) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<ScanResult>());
		} else if (mMethod == Methods.getWifiApConfiguration) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(null);
		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
