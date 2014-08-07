package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.util.Log;

public class XSipManager extends XHook {
	private Methods mMethod;

	private XSipManager(Methods method, String restrictionName) {
		super(restrictionName, method.name(), "SIP." + method.name());
		mMethod = method;
	}

	public String getClassName() {
		return "android.net.sip.SipManager";
	}

	// @formatter:off

	// static boolean isApiSupported(Context context)
	// static boolean isSipWifiOnly(Context context)
	// static boolean isVoipSupported(Context context)
	// public static SipManager newInstance (Context context)
	// http://developer.android.com/reference/android/net/sip/SipManager.html

	// @formatter:on

	private enum Methods {
		isApiSupported, isSipWifiOnly, isVoipSupported, newInstance
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XSipManager(Methods.isApiSupported, PrivacyManager.cCalling));
		listHook.add(new XSipManager(Methods.isSipWifiOnly, PrivacyManager.cCalling));
		listHook.add(new XSipManager(Methods.isVoipSupported, PrivacyManager.cCalling));
		listHook.add(new XSipManager(Methods.newInstance, PrivacyManager.cCalling));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		if (mMethod == Methods.isApiSupported || mMethod == Methods.isSipWifiOnly || mMethod == Methods.isVoipSupported) {
			if (isRestricted(param))
				param.setResult(false);

		} else if (mMethod == Methods.newInstance) {
			if (isRestricted(param))
				param.setResult(null);

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(XParam param) throws Throwable {
		// Do nothing
	}
}
