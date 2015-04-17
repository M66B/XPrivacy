package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.util.Log;

public class XAdvertisingIdClientInfo extends XHook {
	private Methods mMethod;

	private XAdvertisingIdClientInfo(Methods method, String restrictionName, String specifier) {
		super(restrictionName, method.name(), specifier);
		mMethod = method;
	}

	public String getClassName() {
		return "com.google.android.gms.ads.identifier.AdvertisingIdClient$Info";
	}

	// @formatter:off

	// String getId()
	// http://developer.android.com/reference/com/google/android/gms/ads/identifier/AdvertisingIdClient.Info.html

	// @formatter:on

	private enum Methods {
		getId
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XAdvertisingIdClientInfo(Methods.getId, PrivacyManager.cIdentification, "AdvertisingId"));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getId) {
			String adid = (String) param.getResult();
			if (adid != null)
				if (isRestrictedValue(param, adid))
					param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "AdvertisingId"));

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
