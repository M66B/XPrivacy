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
						fieldBSSID.set(wInfo, Restriction.getDefacedProp("MAC"));
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}

					// IP address
					try {
						Field fieldIp = findField(WifiInfo.class, "mIpAddress");
						fieldIp.set(wInfo, Restriction.getDefacedInetAddress());
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}

					// MAC address
					try {
						Field fieldMAC = findField(WifiInfo.class, "mMacAddress");
						fieldMAC.set(wInfo, Restriction.getDefacedProp("MAC"));
					} catch (Throwable ex) {
						Util.bug(this, ex);
					}

					// SSID
					try {
						Field fieldSSID = findField(WifiInfo.class, "mSSID");
						fieldSSID.set(wInfo, Restriction.getDefacedProp("SSID"));
					} catch (Throwable ex) {
						try {
							Field fieldWifiSsid = findField(WifiInfo.class, "mWifiSsid");
							fieldWifiSsid.set(wInfo, Restriction.getDefacedProp("SSID"));
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
					dInfo.ipAddress = Restriction.getDefacedIPInt();
					dInfo.gateway = Restriction.getDefacedIPInt();
					dInfo.dns1 = Restriction.getDefacedIPInt();
					dInfo.dns2 = Restriction.getDefacedIPInt();
					dInfo.serverAddress = Restriction.getDefacedIPInt();
				}
		}
	}
}
