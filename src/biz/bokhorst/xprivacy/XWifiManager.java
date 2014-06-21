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
import android.util.Log;

public class XWifiManager extends XHook {
	private Methods mMethod;
	private String mClassName;

	private XWifiManager(Methods method, String restrictionName, String className) {
		super(restrictionName, method.name(), null);
		mMethod = method;
		mClassName = className;
	}

	public String getClassName() {
		return mClassName;
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

	public static List<XHook> getInstances(String className) {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods wifi : Methods.values())
			listHook.add(new XWifiManager(wifi, PrivacyManager.cNetwork, className));

		listHook.add(new XWifiManager(Methods.getScanResults, PrivacyManager.cLocation, className));

		// This is to fake "offline", no permission required
		listHook.add(new XWifiManager(Methods.getConnectionInfo, PrivacyManager.cInternet, className));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getConfiguredNetworks) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult(new ArrayList<WifiConfiguration>());

		} else if (mMethod == Methods.getConnectionInfo) {
			if (param.getResult() != null && isRestricted(param)) {
				WifiInfo wInfo = (WifiInfo) param.getResult();
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
								// createFromAsciiEncoded(String asciiEncoded)
								Method methodCreateFromAsciiEncoded = mWifiSsid.getClass().getDeclaredMethod(
										"createFromAsciiEncoded", String.class);
								fieldWifiSsid.set(wInfo, methodCreateFromAsciiEncoded.invoke(null, ssid));
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
