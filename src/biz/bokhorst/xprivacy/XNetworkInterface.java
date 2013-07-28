package biz.bokhorst.xprivacy;

import static de.robv.android.xposed.XposedHelpers.findField;

import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.InterfaceAddress;
import java.net.NetworkInterface;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XNetworkInterface extends XHook {

	private XNetworkInterface(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "java.net.NetworkInterface";
	}

	// Internet:
	// - public static NetworkInterface getByInetAddress(InetAddress address)
	// - public static NetworkInterface getByName(String interfaceName)
	// - public static Enumeration<NetworkInterface> getNetworkInterfaces()

	// Network:
	// - public byte[] getHardwareAddress()
	// - public Enumeration<InetAddress> getInetAddresses()
	// - public List<InterfaceAddress> getInterfaceAddresses()

	// libcore/luni/src/main/java/java/net/NetworkInterface.java
	// http://developer.android.com/reference/java/net/NetworkInterface.html

	// libcore/luni/src/main/java/java/net/InetAddress.java
	// libcore/luni/src/main/java/java/net/Inet4Address.java
	// libcore/luni/src/main/java/java/net/InterfaceAddress.java

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		String[] nets = new String[] { "getHardwareAddress", "getInetAddresses", "getInterfaceAddresses" };
		for (String net : nets)
			listHook.add(new XNetworkInterface(net, PrivacyManager.cNetwork));
		String[] inets = new String[] { "getByInetAddress", "getByName", "getNetworkInterfaces" };
		for (String inet : inets)
			listHook.add(new XNetworkInterface(inet, PrivacyManager.cInternet));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		String methodName = param.method.getName();
		if (getRestrictionName().equals(PrivacyManager.cInternet)) {
			// Internet: fake offline state
			if (methodName.equals("getByInetAddress") || methodName.equals("getByName")
					|| methodName.equals("getNetworkInterfaces")) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(null);
			} else
				Util.log(this, Log.WARN, "Unknown method=" + methodName);
		} else if (getRestrictionName().equals(PrivacyManager.cNetwork)) {
			// Network
			NetworkInterface ni = (NetworkInterface) param.thisObject;
			if (!ni.isLoopback())
				if (param.getResult() != null && isRestricted(param))
					if (methodName.equals("getHardwareAddress")) {
						String mac = (String) PrivacyManager.getDefacedProp("MAC");
						long lMac = Long.parseLong(mac.replace(":", ""), 16);
						byte[] address = ByteBuffer.allocate(8).putLong(lMac).array();
						param.setResult(address);
					} else if (methodName.equals("getInetAddresses")) {
						@SuppressWarnings("unchecked")
						Enumeration<InetAddress> addresses = (Enumeration<InetAddress>) param.getResult();
						List<InetAddress> listAddress = new ArrayList<InetAddress>();
						for (InetAddress address : Collections.list(addresses))
							if (address.isAnyLocalAddress() || address.isLoopbackAddress())
								listAddress.add(address);
							else
								listAddress.add((InetAddress) PrivacyManager.getDefacedProp("InetAddress"));
						param.setResult(Collections.enumeration(listAddress));
					} else if (methodName.equals("getInterfaceAddresses")) {
						@SuppressWarnings("unchecked")
						List<InterfaceAddress> listAddress = (List<InterfaceAddress>) param.getResult();
						for (InterfaceAddress address : listAddress) {
							// address
							try {
								Field fieldAddress = findField(InterfaceAddress.class, "address");
								fieldAddress.set(address, PrivacyManager.getDefacedProp("InetAddress"));
							} catch (Throwable ex) {
								Util.bug(this, ex);
							}

							// broadcastAddress
							try {
								Field fieldBroadcastAddress = findField(InterfaceAddress.class, "broadcastAddress");
								fieldBroadcastAddress.set(address, PrivacyManager.getDefacedProp("InetAddress"));
							} catch (Throwable ex) {
								Util.bug(this, ex);
							}
						}
					} else
						Util.log(this, Log.WARN, "Unknown method=" + methodName);
		}
	}
}
