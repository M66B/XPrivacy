package biz.bokhorst.xprivacy;

import java.net.InetAddress;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XInterfaceAddress extends XHook {

	private XInterfaceAddress(String methodName, String restrictionName) {
		super(restrictionName, methodName, null);
	}

	public String getClassName() {
		return null;
	}

	// public InetAddress getAddress()
	// public InetAddress getBroadcast()
	// libcore/luni/src/main/java/java/net/InterfaceAddress.java
	// http://developer.android.com/reference/java/net/InterfaceAddress.html

	@Override
	protected void before(MethodHookParam param) throws Throwable {
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		InetAddress address = (InetAddress) param.getResult();
		if (address != null)
			if (!(address.isAnyLocalAddress() || address.isLoopbackAddress()))
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp("InetAddress"));
	}
}
