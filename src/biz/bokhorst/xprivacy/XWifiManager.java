package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;

import android.net.DhcpInfo;
import android.net.wifi.ScanResult;
import android.net.wifi.SupplicantState;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiInfo;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;
import static de.robv.android.xposed.XposedHelpers.findField;

public class XWifiManager extends XHook {

	public XWifiManager(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public List<WifiConfiguration> getConfiguredNetworks()
	// public WifiInfo getConnectionInfo()
	// public DhcpInfo getDhcpInfo()
	// public List<ScanResult> getScanResults()
	// public WifiConfiguration getWifiApConfiguration()
	// frameworks/base/wifi/java/android/net/wifi/WifiManager.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("getConfiguredNetworks")) {
			if (isRestricted(param))
				param.setResult(new ArrayList<WifiConfiguration>());
		} else if (param.method.getName().equals("getScanResults")) {
			if (isRestricted(param))
				param.setResult(new ArrayList<ScanResult>());
		} else if (param.method.getName().equals("getWifiApConfiguration")) {
			if (isRestricted(param))
				param.setResult(null);
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("getConnectionInfo")) {
			// frameworks/base/wifi/java/android/net/wifi/WifiInfo.java
			WifiInfo wInfo = (WifiInfo) param.getResult();
			if (wInfo != null)
				if (isRestricted(param))
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
							fieldBSSID.set(wInfo, PrivacyManager.getDefacedProp("MAC"));
						} catch (Throwable ex) {
							Util.bug(this, ex);
						}

						// IP address
						try {
							Field fieldIp = findField(WifiInfo.class, "mIpAddress");
							fieldIp.set(wInfo, PrivacyManager.getDefacedProp("InetAddress"));
						} catch (Throwable ex) {
							Util.bug(this, ex);
						}

						// MAC address
						try {
							Field fieldMAC = findField(WifiInfo.class, "mMacAddress");
							fieldMAC.set(wInfo, PrivacyManager.getDefacedProp("MAC"));
						} catch (Throwable ex) {
							Util.bug(this, ex);
						}

						// SSID
						try {
							Field fieldSSID = findField(WifiInfo.class, "mSSID");
							fieldSSID.set(wInfo, PrivacyManager.getDefacedProp("SSID"));
						} catch (Throwable ex) {
							try {
								Field fieldWifiSsid = findField(WifiInfo.class, "mWifiSsid");
								Object mWifiSsid = fieldWifiSsid.get(wInfo);
								if (mWifiSsid != null) {
									Field octets = findField(mWifiSsid.getClass(), "octets");
									octets.set(mWifiSsid, PrivacyManager.getDefacedProp("WifiSsid.octets"));
								}
							} catch (Throwable exex) {
								Util.bug(this, exex);
							}
						}
					}
		} else if (param.method.getName().equals("getDhcpInfo")) {
			// frameworks/base/core/java/android/net/DhcpInfo.java
			DhcpInfo dInfo = (DhcpInfo) param.getResult();
			if (dInfo != null)
				if (isRestricted(param)) {
					dInfo.ipAddress = (Integer) PrivacyManager.getDefacedProp("IPInt");
					dInfo.gateway = (Integer) PrivacyManager.getDefacedProp("IPInt");
					dInfo.dns1 = (Integer) PrivacyManager.getDefacedProp("IPInt");
					dInfo.dns2 = (Integer) PrivacyManager.getDefacedProp("IPInt");
					dInfo.serverAddress = (Integer) PrivacyManager.getDefacedProp("IPInt");
				}
		}
	}
}
