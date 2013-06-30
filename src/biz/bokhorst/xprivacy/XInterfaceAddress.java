package biz.bokhorst.xprivacy;

import java.net.InetAddress;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XInterfaceAddress extends XHook {

	public XInterfaceAddress(String methodName, String restrictionName, String[] permissions) {
		super(methodName, restrictionName, permissions, null);
	}

	// public InetAddress getAddress()
	// public InetAddress getBroadcast()
	// libcore/luni/src/main/java/java/net/InterfaceAddress.java

	@Override
	protected void before(MethodHookParam param) throws Throwable {
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		InetAddress address = (InetAddress) param.getResult();
		if (address != null)
			if (!(address.isAnyLocalAddress() || address.isLoopbackAddress()))
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedInetAddress());
	}
}
