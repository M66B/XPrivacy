package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.net.InetAddress;
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
						fieldBSSID.set(wInfo, XRestriction.cDefacedMac);
					} catch (Throwable ex) {
						XUtil.bug(this, ex);
					}

					// IP address
					try {
						Field fieldIp = findField(WifiInfo.class, "mIpAddress");
						fieldIp.set(wInfo, InetAddress.getLocalHost());
					} catch (Throwable ex) {
						XUtil.bug(this, ex);
					}

					// MAC address
					try {
						Field fieldMAC = findField(WifiInfo.class, "mMacAddress");
						fieldMAC.set(wInfo, XRestriction.cDefacedMac);
					} catch (Throwable ex) {
						XUtil.bug(this, ex);
					}

					// SSID
					try {
						Field fieldSSID = findField(WifiInfo.class, "mSSID");
						fieldSSID.set(wInfo, XRestriction.cDefaceString);
					} catch (Throwable ex) {
						XUtil.bug(this, ex);
					}
				}
		} else if (param.method.getName().equals("getDhcpInfo")) {
			// frameworks/base/core/java/android/net/DhcpInfo.java
			DhcpInfo dInfo = (DhcpInfo) param.getResult();
			if (dInfo != null)
				if (isRestricted(param)) {
					dInfo.ipAddress = XRestriction.cDefaceIP;
					dInfo.gateway = XRestriction.cDefaceIP;
					dInfo.dns1 = XRestriction.cDefaceIP;
					dInfo.dns2 = XRestriction.cDefaceIP;
					dInfo.serverAddress = XRestriction.cDefaceIP;
				}
		}
	}
}
