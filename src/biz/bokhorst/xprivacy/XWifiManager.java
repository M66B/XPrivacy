package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import android.net.DhcpInfo;
import android.net.wifi.ScanResult;
import android.net.wifi.SupplicantState;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiInfo;
import android.os.Binder;
import android.os.Build;

public class XWifiManager extends XHook {
	private Methods mMethod;
	private String mClassName;
	private static final String cClassName = "android.net.wifi.WifiManager";

	private XWifiManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name().replace("Srv_", ""), "WiFi." + method.name());
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
	}

	// @formatter:off

	// public List<WifiConfiguration> getConfiguredNetworks()
	// public WifiInfo getConnectionInfo()
	// public DhcpInfo getDhcpInfo()
	// public List<ScanResult> getScanResults()
	// public WifiConfiguration getWifiApConfiguration()
	// frameworks/base/wifi/java/android/net/wifi/WifiManager.java
	// frameworks/base/wifi/java/android/net/wifi/WifiInfo.java
	// frameworks/base/core/java/android/net/DhcpInfo.java
	// http://developer.android.com/reference/android/net/wifi/WifiManager.html

	// public java.util.List<android.net.wifi.BatchedScanResult> getBatchedScanResults(java.lang.String callingPackage)
	// public java.util.List<android.net.wifi.WifiConfiguration> getConfiguredNetworks()
	// public android.net.wifi.WifiInfo getConnectionInfo()
	// public android.net.DhcpInfo getDhcpInfo()
	// public java.util.List<android.net.wifi.ScanResult> getScanResults(java.lang.String callingPackage)
	// public android.net.wifi.WifiConfiguration getWifiApConfiguration()
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.4.4_r1/com/android/server/wifi/WifiService.java
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/com/android/server/wifi/WifiServiceImpl.java

	// @formatter:on

	// @formatter:off
	private enum Methods {
		getConfiguredNetworks, getConnectionInfo, getDhcpInfo, getScanResults, getWifiApConfiguration,
		Srv_getBatchedScanResults, Srv_getConfiguredNetworks, Srv_getConnectionInfo, Srv_getDhcpInfo, Srv_getScanResults, Srv_getWifiApConfiguration
	};
	// @formatter:on

	public static List<XHook> getInstances(String className, boolean server) {
		List<XHook> listHook = new ArrayList<XHook>();
		if (!cClassName.equals(className)) {
			if (className == null)
				className = cClassName;

			String srvClassName;
			if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
				srvClassName = "com.android.server.wifi.WifiServiceImpl";
			else
				srvClassName = "com.android.server.wifi.WifiService";

			for (Methods wifi : Methods.values())
				if (wifi.name().startsWith("Srv_")) {
					if (server)
						listHook.add(new XWifiManager(wifi, PrivacyManager.cNetwork, srvClassName));
				} else
					listHook.add(new XWifiManager(wifi, PrivacyManager.cNetwork, className));

			listHook.add(new XWifiManager(Methods.getScanResults, PrivacyManager.cLocation, className));
			if (server)
				listHook.add(new XWifiManager(Methods.Srv_getScanResults, PrivacyManager.cLocation, srvClassName));

			// This is to fake "offline", no permission required
			listHook.add(new XWifiManager(Methods.getConnectionInfo, PrivacyManager.cInternet, className));
			if (server)
				listHook.add(new XWifiManager(Methods.Srv_getConnectionInfo, PrivacyManager.cInternet, srvClassName));
		}

		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	@SuppressWarnings("rawtypes")
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case Srv_getBatchedScanResults:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList());
			break;

		case getConfiguredNetworks:
		case Srv_getConfiguredNetworks:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<WifiConfiguration>());
			break;

		case getConnectionInfo:
		case Srv_getConnectionInfo:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					WifiInfo result = (WifiInfo) param.getResult();
					WifiInfo wInfo = WifiInfo.class.getConstructor(WifiInfo.class).newInstance(result);
					if (getRestrictionName().equals(PrivacyManager.cInternet)) {
						// Supplicant state
						try {
							Field fieldState = WifiInfo.class.getDeclaredField("mSupplicantState");
							fieldState.setAccessible(true);
							fieldState.set(wInfo, SupplicantState.DISCONNECTED);
						} catch (Throwable ex) {
							Util.bug(this, ex);
						}

					} else {
						// BSSID
						try {
							Field fieldBSSID = WifiInfo.class.getDeclaredField("mBSSID");
							fieldBSSID.setAccessible(true);
							fieldBSSID.set(wInfo, PrivacyManager.getDefacedProp(Binder.getCallingUid(), "MAC"));
						} catch (Throwable ex) {
							Util.bug(this, ex);
						}

						// IP address
						try {
							Field fieldIp = WifiInfo.class.getDeclaredField("mIpAddress");
							fieldIp.setAccessible(true);
							fieldIp.set(wInfo, PrivacyManager.getDefacedProp(Binder.getCallingUid(), "InetAddress"));
						} catch (Throwable ex) {
							Util.bug(this, ex);
						}

						// MAC address
						try {
							Field fieldMAC = WifiInfo.class.getDeclaredField("mMacAddress");
							fieldMAC.setAccessible(true);
							fieldMAC.set(wInfo, PrivacyManager.getDefacedProp(Binder.getCallingUid(), "MAC"));
						} catch (Throwable ex) {
							Util.bug(this, ex);
						}

						// SSID
						String ssid = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "SSID");
						try {
							Field fieldSSID = WifiInfo.class.getDeclaredField("mSSID");
							fieldSSID.setAccessible(true);
							fieldSSID.set(wInfo, ssid);
						} catch (Throwable ex) {
							try {
								Field fieldWifiSsid = WifiInfo.class.getDeclaredField("mWifiSsid");
								fieldWifiSsid.setAccessible(true);
								Object mWifiSsid = fieldWifiSsid.get(wInfo);
								if (mWifiSsid != null) {
									// public static WifiSsid
									// createFromAsciiEncoded(String
									// asciiEncoded)
									Method methodCreateFromAsciiEncoded = mWifiSsid.getClass().getDeclaredMethod(
											"createFromAsciiEncoded", String.class);
									fieldWifiSsid.set(wInfo, methodCreateFromAsciiEncoded.invoke(null, ssid));
								}
							} catch (Throwable exex) {
								Util.bug(this, exex);
							}
						}
					}
					param.setResult(wInfo);
				}
			break;

		case getDhcpInfo:
		case Srv_getDhcpInfo:
			if (param.getResult() != null)
				if (isRestricted(param)) {
					DhcpInfo result = (DhcpInfo) param.getResult();
					DhcpInfo dInfo = DhcpInfo.class.getConstructor(DhcpInfo.class).newInstance(result);
					dInfo.ipAddress = (Integer) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "IPInt");
					dInfo.gateway = dInfo.ipAddress;
					dInfo.dns1 = dInfo.ipAddress;
					dInfo.dns2 = dInfo.ipAddress;
					dInfo.serverAddress = dInfo.ipAddress;
					param.setResult(dInfo);
				}
			break;

		case getScanResults:
		case Srv_getScanResults:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<ScanResult>());
			break;

		case getWifiApConfiguration:
		case Srv_getWifiApConfiguration:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(null);
			break;

		}
	}
}
