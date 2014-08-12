package biz.bokhorst.xprivacy;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

public class XInetAddress extends XHook {
	@SuppressWarnings("unused")
	private Methods mMethod;

	private XInetAddress(Methods method, String restrictionName, String specifier) {
		super(restrictionName, method.name(), "InetAddress." + method.name());
		mMethod = method;
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

	private enum Methods {
		getAllByName, getByAddress, getByName
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods addr : Methods.values())
			listHook.add(new XInetAddress(addr, PrivacyManager.cInternet, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		Object result = param.getResult();
		if (result != null) {
			// Get addresses
			InetAddress[] addresses;
			if (result instanceof InetAddress)
				addresses = new InetAddress[] { (InetAddress) result };
			else if (result instanceof InetAddress[])
				addresses = (InetAddress[]) result;
			else
				addresses = new InetAddress[0];

			// Check if restricted
			boolean restrict = false;
			for (InetAddress address : addresses)
				if (!address.isLoopbackAddress()) {
					restrict = true;
					break;
				}

			// Restrict
			if (restrict)
				if (param.args.length > 0 && param.args[0] instanceof String) {
					if (isRestrictedExtra(param, (String) param.args[0]))
						param.setThrowable(new UnknownHostException("XPrivacy"));

				} else {
					if (isRestricted(param))
						param.setThrowable(new UnknownHostException("XPrivacy"));
				}
		}
	}
}