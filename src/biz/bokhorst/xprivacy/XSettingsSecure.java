package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.os.Binder;
import android.provider.Settings;
import android.util.Log;

public class XSettingsSecure extends XHook {
	private Methods mMethod;

	private XSettingsSecure(Methods method, String restrictionName) {
		super(restrictionName, method.name(), null);
		mMethod = method;
	}

	public String getClassName() {
		return "android.provider.Settings.Secure";
	}

	// @formatter:off

	// public synchronized static String getString(ContentResolver resolver, String name)
	// frameworks/base/core/java/android/provider/Settings.java
	// frameworks/base/core/java/android/content/ContentResolver.java
	// http://developer.android.com/reference/android/provider/Settings.Secure.html

	// @formatter:on

	private enum Methods {
		getString
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XSettingsSecure(Methods.getString, PrivacyManager.cIdentification));
		return listHook;
	}

	@Override
	protected void before(XParam param) throws Throwable {
		// Do nothing
	}

	@Override
	protected void after(XParam param) throws Throwable {
		if (mMethod == Methods.getString) {
			String name = (param.args.length > 1 ? (String) param.args[1] : null);
			if (Settings.Secure.ANDROID_ID.equals(name)) {
				String id = (String) param.getResult();
				if (id != null)
					if (isRestrictedValue(param, id))
						param.setResult(PrivacyManager.getDefacedProp(Binder.getCallingUid(), "ANDROID_ID"));
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}
}
