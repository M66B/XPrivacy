package biz.bokhorst.xprivacy;

import java.net.InetAddress;
import java.net.InterfaceAddress;
import java.net.NetworkInterface;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XNetworkInterface extends XHook {

	public XNetworkInterface(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions);
	}

	// public byte[] getHardwareAddress()
	// public Enumeration<InetAddress> getInetAddresses()
	// public List<InterfaceAddress> getInterfaceAddresses()
	// libcore/luni/src/main/java/java/net/NetworkInterface.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		if (param.getResultOrThrowable() != null) {
			NetworkInterface ni = (NetworkInterface) param.thisObject;
			if (!ni.isLoopback())
				if (isRestricted(param)) {
					String methodName = param.method.getName();
					if (methodName.equals("getHardwareAddress")) {
						// Hardware address
						byte[] address = (byte[]) param.getResult();
						for (int i = 0; i < address.length; i++)
							address[i] = XRestriction.cDefaceBytes[i % XRestriction.cDefaceBytes.length];
						param.setResult(address);
					} else if (methodName.equals("getInetAddresses")) {
						// Inet addresses
						@SuppressWarnings("unchecked")
						Enumeration<InetAddress> addresses = (Enumeration<InetAddress>) param.getResult();
						List<InetAddress> listAddress = new ArrayList<InetAddress>();
						for (InetAddress address : Collections.list(addresses))
							if (!address.isLoopbackAddress())
								listAddress.add(address);
						param.setResult(Collections.enumeration(listAddress));
					} else if (methodName.equals("getInterfaceAddresses")) {
						// Interface addresses
						List<InterfaceAddress> listAddress = new ArrayList<InterfaceAddress>();
						param.setResult(Collections.enumeration(listAddress));
					} else
						XUtil.log(this, Log.WARN, "Unknown method=" + methodName);
				}
		}
	}
}
