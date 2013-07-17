package biz.bokhorst.xprivacy;

import java.net.InetAddress;
import java.net.UnknownHostException;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XInetAddress extends XHook {

	public XInetAddress(String methodName, String restrictionName, String[] permissions, String specifier) {
		super(methodName, restrictionName, permissions, null);
	}

	// public static InetAddress[] getAllByName(String host)
	// public static InetAddress getByAddress(byte[] ipAddress)
	// public static InetAddress getByAddress(String hostName, byte[] ipAddress)
	// public static InetAddress getByName(String host)
	// libcore/luni/src/main/java/java/net/InetAddress.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		Object result = param.getResult();
		if (result != null) {
			InetAddress[] addresses;
			if (result.getClass().equals(InetAddress.class))
				addresses = new InetAddress[] { (InetAddress) result };
			else if (result.getClass().equals(InetAddress[].class))
				addresses = (InetAddress[]) result;
			else
				addresses = new InetAddress[0];

			for (InetAddress address : addresses)
				if (!address.isLoopbackAddress())
					if (isRestricted(param)) {
						param.setThrowable(new UnknownHostException("Unable to resolve host"));
						break;
					}
		}
	}
}