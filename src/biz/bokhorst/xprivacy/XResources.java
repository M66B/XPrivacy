package biz.bokhorst.xprivacy;

import java.util.ArrayList;
import java.util.List;

import android.content.res.Configuration;
import android.os.Binder;
import android.util.Log;

import de.robv.android.xposed.XC_MethodHook.MethodHookParam;

public class XResources extends XHook {
	private Methods mMethod;

	private XResources(Methods method, int sdk) {
		super(null, method.name(), null, sdk);
		mMethod = method;
	}

	public String getClassName() {
		return "android.content.res.Resources";
	}

	// public void updateConfiguration(Configuration config, ...)
	// frameworks/base/core/java/android/content/res/Resources.java
	// http://developer.android.com/reference/android/content/res/Resources.html
	// http://developer.android.com/reference/android/content/res/Configuration.html

	private enum Methods {
		updateConfiguration
	};

	public static List<XHook> getInstances() {
		List<XHook> listHook = new ArrayList<XHook>();
		listHook.add(new XResources(Methods.updateConfiguration, 1));
		return listHook;
	}

	@Override
	protected void before(MethodHookParam param) throws Throwable {
		if (mMethod == Methods.updateConfiguration) {
			if (param.args.length > 0 && param.args[0] != null && param.args[0] instanceof Configuration) {
				boolean restricted = false;
				int uid = Binder.getCallingUid();
				Configuration config = new Configuration((Configuration) param.args[0]);

				if (getRestricted(uid, PrivacyManager.cPhone, "Configuration.MCC")) {
					restricted = true;
					try {
						config.mcc = Integer.parseInt((String) PrivacyManager.getDefacedProp(uid, "MCC"));
					} catch (Throwable ex) {
						config.mcc = 1;
					}
				}

				if (getRestricted(uid, PrivacyManager.cPhone, "Configuration.MNC")) {
					restricted = true;
					try {
						config.mnc = Integer.parseInt((String) PrivacyManager.getDefacedProp(uid, "MNC"));
					} catch (Throwable ex) {
						config.mnc = 1;
					}
				}

				if (restricted)
					param.args[0] = config;
			}

		} else
			Util.log(this, Log.WARN, "Unknown method=" + param.method.getName());
	}

	@Override
	protected void after(MethodHookParam param) throws Throwable {
		// Do nothing
	}
}
