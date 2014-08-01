package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XLinkAddress extends XHook {
	private Methods mMethod;

	private XLinkAddress(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "LinkAddress." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.net.LinkAddress";
	}

	// public String toString ()
	// http://developer.android.com/reference/android/net/LinkAddress.html

	private enum Methods {
		toString
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		//listHook.add(new XLinkAddress(Methods.toString, PrivacyManager.cInternet));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.toString) {
			if (param.getResult() != null && isRestricted(param))
				param.setResult("127.0.1.1/24");

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
