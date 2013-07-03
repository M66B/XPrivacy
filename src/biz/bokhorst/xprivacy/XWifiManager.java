package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.util.ArrayList;

import android.net.DhcpInfo;
import android.net.wifi.ScanResult;
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
	// frameworks/base/wifi/java/android/net/wifi/WifiManager.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("getConfiguredNetworks")) {
			if (isRestricted(param))
				param.setResult(new ArrayList<WifiConfiguration>());
		} else if (param.method.getName().equals("getScanResults")) {
			if (isRestricted(param))
				param.setResult(new ArrayList<ScanResult>());
		}
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.method.getName().equals("getConnectionInfo")) {
			// frameworks/base/wifi/java/android/net/wifi/WifiInfo.java
			WifiInfo wInfo = (WifiInfo) param.getResult();
			if (wInfo != null)
				if (isRestricted(param)) {
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
						fieldIp.set(wInfo, PrivacyManager.getDefacedInetAddress());
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

					try {
						// SSID (4.2-)
						Field fieldSSID = findField(WifiInfo.class, "mSSID");
						fieldSSID.set(wInfo, PrivacyManager.getDefacedProp("SSID"));
					} catch (Throwable ex) {
						try {
							// WifiSsid (4.2+)
							Field fieldWifiSsid = findField(WifiInfo.class, "mWifiSsid");
							Object mWifiSsid = fieldWifiSsid.get(wInfo);
							Field fieldOctets = findField(mWifiSsid.getClass(), "octets");
							fieldOctets.set(mWifiSsid, PrivacyManager.getDefacedProp("WifiSsid.octets"));
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
					dInfo.ipAddress = PrivacyManager.getDefacedIPInt();
					dInfo.gateway = PrivacyManager.getDefacedIPInt();
					dInfo.dns1 = PrivacyManager.getDefacedIPInt();
					dInfo.dns2 = PrivacyManager.getDefacedIPInt();
					dInfo.serverAddress = PrivacyManager.getDefacedIPInt();
				}
		}
	}
}
