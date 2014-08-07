package biz.bokhorst.xprivacy;

import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.InterfaceAddress;
import java.net.NetworkInterface;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XNetworkInterface extends XHook {
	private Methods mMethod;

	private XNetworkInterface(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "NetworkInterface." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "java.net.NetworkInterface";
	}

	// Internet:
	// - public static NetworkInterface getByIndex(int index)
	// - public static NetworkInterface getByInetAddress(InetAddress address)
	// - public static NetworkInterface getByName(String interfaceName)
	// - public static Enumeration<NetworkInterface> getNetworkInterfaces()

	// Network:
	// - public byte[] getHardwareAddress()
	// - public Enumeration<InetAddress> getInetAddresses()
	// - public List<InterfaceAddress> getInterfaceAddresses()

	// libcore/luni/src/main/java/java/net/NetworkInterface.java
	// http://developer.android.com/reference/java/net/NetworkInterface.html

	// @formatter:off
	private enum Methods {
		getByIndex, getByInetAddress, getByName, getNetworkInterfaces,
		getHardwareAddress, getInetAddresses, getInterfaceAddresses
	};
	// @formatter:on

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XNetworkInterface(Methods.getByIndex, PrivacyManager.cInternet));
		listHook.add(new XNetworkInterface(Methods.getByInetAddress, PrivacyManager.cInternet));
		listHook.add(new XNetworkInterface(Methods.getByName, PrivacyManager.cInternet));
		listHook.add(new XNetworkInterface(Methods.getNetworkInterfaces, PrivacyManager.cInternet));

		listHook.add(new XNetworkInterface(Methods.getHardwareAddress, PrivacyManager.cNetwork));
		listHook.add(new XNetworkInterface(Methods.getInetAddresses, PrivacyManager.cNetwork));
		listHook.add(new XNetworkInterface(Methods.getInterfaceAddresses, PrivacyManager.cNetwork));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (getRestrictionName().equals(PrivacyManager.cInternet)) {
			// Internet: fake offline state
			if (mMethod == Methods.getByIndex || mMethod == Methods.getByInetAddress || mMethod == Methods.getByName
					|| mMethod == Methods.getNetworkInterfaces) {
				if (param.getResult() != null && isRestricted(param))
					param.setResult(null);

			} else
				Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());

		} else if (getRestrictionName().equals(PrivacyManager.cNetwork)) {
			// Network
			NetworkInterface ni = (NetworkInterface) param.thisObject;
			if (ni != null)
				if (param.getResult() != null && isRestricted(param))
					if (mMethod == Methods.getHardwareAddress) {
						String mac = (String) PrivacyManager.getDefacedProp(Binder.getCallingUid(), "MAC");
						long lMac = Long.parseLong(mac.replace(":", ""), 16);
						byte[] address = ByteBuffer.allocate(8).putLong(lMac).array();
						param.setResult(address);

					} else if (mMethod == Methods.getInetAddresses) {
						@SuppressWarnings("unchecked")
						Enumeration<InetAddress> addresses = (Enumeration<InetAddress>) param.getResult();
						List<InetAddress> listAddress = new ArrayList<InetAddress>();
						for (InetAddress address : Collections.list(addresses))
							if (address.isAnyLocalAddress() || address.isLoopbackAddress())
								listAddress.add(address);
							else
								listAddress.add((InetAddress) PrivacyManager.getDefacedProp(Binder.getCallingUid(),
										"InetAddress"));
						param.setResult(Collections.enumeration(listAddress));

					} else if (mMethod == Methods.getInterfaceAddresses) {
						@SuppressWarnings("unchecked")
						List<InterfaceAddress> listAddress = (List<InterfaceAddress>) param.getResult();
						for (InterfaceAddress address : listAddress) {
							// address
							try {
								Field fieldAddress = InterfaceAddress.class.getDeclaredField("address");
								fieldAddress.setAccessible(true);
								fieldAddress.set(address,
										PrivacyManager.getDefacedProp(Binder.getCallingUid(), "InetAddress"));
							} catch (Throwable ex) {
								Util.bug(this, ex);
							}

							// broadcastAddress
							try {
								Field fieldBroadcastAddress = InterfaceAddress.class
										.getDeclaredField("broadcastAddress");
								fieldBroadcastAddress.setAccessible(true);
								fieldBroadcastAddress.set(address,
										PrivacyManager.getDefacedProp(Binder.getCallingUid(), "InetAddress"));
							} catch (Throwable ex) {
								Util.bug(this, ex);
							}
						}

					} else
						Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
		}
	}
}
