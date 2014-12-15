package biz.bokhorst.xprivacy;

import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

import android.net.LinkAddress;
import android.net.LinkProperties;

public class XLinkProperties extends XHook {
	private Methods mMethod;

	private XLinkProperties(Methods method, String restrictionName, String specifier) {
		super(restrictionName, method.name(), "LinkProperties." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.net.LinkProperties";
	}

	// public List<InetAddress> getAddresses()
	// public List<InetAddress> getAllAddresses()
	// public List<LinkAddress> getAllLinkAddresses()
	// public List<LinkAddress> getLinkAddresses()
	// public @NonNull List<LinkProperties> getStackedLinks()
	// http://developer.android.com/reference/android/net/LinkProperties.html
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/android/net/LinkProperties.java

	private enum Methods {
		getAddresses, getAllAddresses, getAllLinkAddresses, getLinkAddresses, getStackedLinks
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		for (Methods addr : Methods.values())
			listHook.add(new XLinkProperties(addr, PrivacyManager.cInternet, null));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getAddresses:
		case getAllAddresses:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<InetAddress>());
			break;

		case getAllLinkAddresses:
		case getLinkAddresses:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<LinkAddress>());
			break;

		case getStackedLinks:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(new ArrayList<LinkProperties>());
			break;
		}
	}
}