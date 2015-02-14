package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import biz.bokhorst.xprivacy.XHook;

public class XIpPrefix extends XHook {
	private Methods mMethod;

	private XIpPrefix(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "IpPrefix." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.net.IpPrefix";
	}

	// public InetAddress getAddress()
	// public byte[] getRawAddress()
	// https://developer.android.com/reference/android/net/IpPrefix.html
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.0.0_r1/android/net/IpPrefix.java

	private enum Methods {
		getAddress, getRawAddress
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XIpPrefix(Methods.getAddress, PrivacyManager.cInternet));
		listHook.add(new XIpPrefix(Methods.getRawAddress, PrivacyManager.cInternet));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		switch (mMethod) {
		case getAddress:
			if (isRestricted(param))
				param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "InetAddress"));
			break;

		case getRawAddress:
			if (param.getResult() != null)
				if (isRestricted(param))
					param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "IPInt"));
			break;
		}
	}
}
