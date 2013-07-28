package biz.bokhorst.xprivacy;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XInetAddress extends XHook {

	private XInetAddress(String methodName, String restrictionName, String specifier) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return "java.net.InetAddress";
	}

	// public static InetAddress[] getAllByName(String host)
	// public static InetAddress getByAddress(byte[] ipAddress)
	// public static InetAddress getByAddress(String hostName, byte[] ipAddress)
	// public static InetAddress getByName(String host)
	// libcore/luni/src/main/java/java/net/InetAddress.java
	// http://developer.android.com/reference/java/net/InetAddress.html

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		String[] addrs = new String[] { "getAllByName", "getByAddress", "getByName" };
		for (String addr : addrs)
			listHook.add(new XInetAddress(addr, PrivacyManager.cInternet, null));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		Object result = param.getResult();
		if (result != null) {
			// Get addresses
			InetAddress[] addresses;
			if (result.getClass().equals(InetAddress.class))
				addresses = new InetAddress[] { (InetAddress) result };
			else if (result.getClass().equals(InetAddress[].class))
				addresses = (InetAddress[]) result;
			else
				addresses = new InetAddress[0];

			// Check to restrict
			boolean restrict = false;
			for (InetAddress address : addresses)
				if (!address.isLoopbackAddress()) {
					restrict = true;
					break;
				}

			// Restrict
			if (restrict && isRestricted(param))
				param.setThrowable(new UnknownHostException("Unable to resolve host"));
		}
	}
}